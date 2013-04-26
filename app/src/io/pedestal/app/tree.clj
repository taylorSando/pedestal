; Copyright 2013 Relevance, Inc.

; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0)
; which can be found in the file epl-v10.html at the root of this distribution.
;
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
;
; You must not remove this notice, or any other, from this software.

(ns ^:shared io.pedestal.app.tree
    "A tree structure which can be used to represent the user
     interface of an application.

    A tree node may contain a value, attributes and data model
    transformations.

    Values are arbitrary Clojure data.

    Attributes are a map of keys to values.

    Data model transformations are maps of transform names to
    collections of messages which can transform the data model.

    The purpose of this tree is to provide an information model for an
    application. This model contains all of the information that is
    required to render a user interface without including information
    about how to render it."
    (:require [clojure.set :as set]
              [io.pedestal.app.util.log :as log]
              [io.pedestal.app.query :as query]))

(def ^:dynamic *gc-deltas* true)

(defmulti inverse (fn [delta] (first delta)))

(defmethod inverse :node-create [[op path type]]
  [:node-destroy path type])

(defmethod inverse :node-destroy [[op path type]]
  [:node-create path type])

(defmethod inverse :value [[op path o n]]
  [op path n o])

(defmethod inverse :attr [[op path k o n]]
  [op path k n o])

(defmethod inverse :transform-enable [[op path transform-name msgs]]
  [:transform-disable path transform-name msgs])

(defmethod inverse :transform-disable [[op path transform-name msgs]]
  [:transform-enable path transform-name msgs])

(defn invert [deltas]
  (mapv inverse (reverse deltas)))

(defn- real-path [path]
  (vec (interpose :children (into [:tree] path))))

(defn- new-node [children]
  {:children children})

(defn- node-type [x]
  (cond (map? x) :map
        (vector? x) :vector
        :else :unknown))

(defn- existing-node-has-same-type? [tree r-path type]
  (if-let [node (get-in tree r-path)]
    (= (node-type (:children node)) type)
    true))

(defn- parent-exists? [tree path]
  ;; If the path is the root, we know it exists already
  (if (= path [])
    true
    ;; Otherwise, remove the last element from the path, expand it.
    ;; This is the parent path.  See if it exists in the tree.
    (let [r-path (real-path (vec (butlast path)))]
      (get-in tree r-path))))

(defn- apply-to-tree-dispatch [_ delta]
  (if (fn? delta)
    :function
    (first delta)))

;; Dispatch on the type of delta
;; Takes a tree and delta as arguments
(defmulti ^:private apply-to-tree apply-to-tree-dispatch)

(defmethod apply-to-tree :default [tree _]
  tree)

(declare map->deltas)

(defmethod apply-to-tree :node-create [tree delta]  
  (let [[_ path type] delta]
    (if (map? type)
      (reduce apply-to-tree tree (map->deltas type path))
      ;; If type is nil, it wasn't specified, and by default it's :map
      (let [type (or type :map)
            ;; The node create delta requires a type, so if it wasn't specified, i.e., there are only 2 arguments,
            ;; then it needs to be added.  By default, the type is a map
            delta (if (= (count delta) 2) [:node-create path type] delta)
            ;; Set the path to its expanded tree form
            r-path (real-path path)
            children (condp = type
                       :vector []
                       :map {})
            ;; Find out if the given path's parent exists in the tree already                        
            tree (if (parent-exists? tree path)
                   ;; If it does already exist, then it does not need to be created
                   tree
                   ;; Otherwise, need to recursively create the parent nodes
                   (let [children-type (if (keyword? (last path)) :map :vector)]
                     (apply-to-tree tree [:node-create (vec (butlast path)) children-type])))]
        (assert (parent-exists? tree path)
                (str "The parent of " path " does not exist."))
        (assert (existing-node-has-same-type? tree r-path type)
                (str "The node at " path " exists and is not the same type as the requested node.\n"
                     "node:\n"
                     (get-in tree r-path) "\n"
                     "delta:\n"
                     delta))
        ;; If the node specified by the expanded path is now in the tree, return the tree
        (if (get-in tree r-path)
          tree
          ;; Given that the path to this node exists, create its children,
          ;; which will be either a vector, or a map, which was specified above.
          ;; Then place the delta that was used to create the node in :this-tx,
          ;; which holds all the deltas that were actually used in the transaction
          (-> tree
              (assoc-in r-path (new-node children))
              (update-in [:this-tx] conj delta)))))))

(defn- remove-index-from-vector [vector index]
  (let [[begin end] (split-at index vector)]
    (into (vec begin) (rest end))))

(defn- child-keys [children]
  "Extracts the keys of the child
Returns the key names if it's a map, or the vector indexes if the child is a vector"
  (condp = (node-type children)
    :map (keys children)
    :vector (reverse (range (count children)))
    :else []))

(defn- remove-children [tree path children]
  "Remove all the children from the tree specified by the path"
  (reduce apply-to-tree tree (map (fn [k] [:node-destroy (conj path k)])
                                  (child-keys children))))

(defmethod apply-to-tree :node-destroy [tree delta]
  "Removes a node, and all of its children, from the tree"
  (let [[_ path type] delta
        ;; Convert the shortform path to the long form
        r-path (real-path path)
        ;; Get the parent path of the long form
        containing-path (butlast r-path)
        ;; This is the node that the long form path points to
        node-to-remove (get-in tree r-path)
        ;; Get its children
        children (:children node-to-remove)
        ;; children-type
        type (or type (node-type children))
        ;; If type was not specified, add it to the end of the delta
        delta (if (= (count delta) 2) (conj delta type) delta)]
    ;; Is there a node to remove?
    (if (not node-to-remove)
      tree ;; Return the tree without doing anything
      ;; There is a node to remove, therefore, make sure the type requested for deletion matches
      ;; the actual child type
      (do (assert (= (node-type children) type)
                  (str "The given child node type does not match the actual type: "
                       (pr-str delta)))
          ;; Does this node we are removing have any children?
          (let [tree (if (not (empty? children))
                       ;; Must recursively remove the children first
                       (remove-children tree path children)
                       ;; Just return the tree, it has no children
                       tree)
                ;; Does this node have any value associated with it?
                tree (if (:value node-to-remove)
                       ;; Make sure that the value is set to nil, and removed
                       (apply-to-tree tree [:value path (:value node-to-remove) nil])
                       ;; There is no value associated with the node, so do nothing
                       tree)
                ;; Does this node have any transforms associated with it?
                tree (if-let [ks (:transforms node-to-remove)]
                       ;; Remove the transforms
                       (reduce apply-to-tree tree (map (fn [[k v]] [:transform-disable path k]) ks))
                       ;; Do nothing, there are no transforms
                       tree)
                ;; Does this node have any attributes associated with it?
                tree (if-let [ks (:attrs node-to-remove)]
                       ;; Remove the attrs
                       (reduce apply-to-tree tree (map (fn [[k v]] [:attr path k v nil]) ks))
                       ;; There are no attributes associated with the node
                       tree)
                ;; Create the new tree,
                ;; Does the parent path to this node exist?
                ;; If it does exist, the tree should no longer contain a reference to this node
                ;; that we are deleting
                new-tree (if (nil? containing-path)
                           ;; There is no parent path, so this node is the root, since we're deleting
                           ;; the root node, there is no tree left, so it's nil
                           (assoc tree :tree nil)
                           ;; There is a parent node above the node, therefore, need to remove
                           ;; the references in the tree from the parent to the node we're deleting
                           (let [last-path (last r-path)
                                 container (get-in tree containing-path)]
                             ;; Removal is based on whether the node is a map or a vector
                             (if (map? container)
                               (update-in tree containing-path dissoc last-path)
                               (update-in tree containing-path remove-index-from-vector last-path))))]
            ;; Update the :this-tx to include this delta, so that there is a record of the node removal
            (update-in new-tree [:this-tx] conj delta))))))

;; Is this even used?
(defmethod apply-to-tree :children-exit [tree delta]
  (let [[_ path] delta
        r-path (real-path path)
        c-path (conj r-path :children)
        children (get-in tree c-path)]
    (if (not (empty? children))
      (remove-children tree path children)
      tree)))

(defn- same-value? [tree path v]
  (= (get-in tree path) v))

(defn update-or-remove [tree path v]
  "Either change the value (v) stored at the given node in the tree, or remove the value at the node
This is a helper function used for both :value and :attrs"
  (if (nil? v)
    (update-in tree (butlast path) dissoc (last path))
    (assoc-in tree path v)))

(defmethod apply-to-tree :value [tree delta]
  "Change the value associated with a node, requires the old and new value"
  (let [[op path o n] delta
        ;; Get the long form of the path
        r-path (real-path path)
        ;; Get the path to the value from the long form
        ;; to look up in the tree
        v-path (conj r-path :value)
        ;; Grab the value that is presently in the tree, which will be the old value
        old-value (get-in tree v-path)
        ;; The delta should have four items.  If it has something else
        ;; set n to be o, which is what the old value is suppose to be
        [o n] (if (= (count delta) 4) [o n] [old-value o])]
    ;; This makes sure that what is really in the node, old-value, matches what was
    ;; specified by the user, o.
    (assert (= o old-value)
            (str "The old value at path " path " is " old-value
                 " but was expected to be " o "."))
    ;; If the old value and the new value are the same
    (if (= o n)
      ;; Nothing needs to be done
      tree
      ;; Otherwise, change the value at the node
      ;; Add this delta to the :this-tx in the tree since
      ;; it changed the tree
      (-> tree
          (update-or-remove v-path n)
          (update-in [:this-tx] conj [op path o n])))))

(defn remove-empty [tree path]
  (if (seq (get-in tree path))
    tree
    (update-in tree (butlast path) dissoc (last path))))

(defmethod apply-to-tree :attr [tree delta]
  (let [[op path k o n] delta
        ;; Get the long path to the node in the tree
        r-path (real-path path)
        ;; Get the path to the attribute in the tree
        a-path (conj r-path :attrs k)
        ;; Get the previous value of the attribute
        old-value (get-in tree a-path)
        ;; Make sure that the proper number of arguments exist
        [o n] (if (= (count delta) 5) [o n] [old-value o])]
    ;; Need to make sure what was specified by the user matches what is actually
    ;; in the tree's node's attribute right now
    (assert (= o old-value)
            (str "Error:" (pr-str delta) "\n"
                 "The old attribute value for " k " is "
                 old-value
                 " but was expected to be " o "."))
    ;; If the old value matches the new value
    (if (= o n)
      ;; Nothing needs to be done
      tree
      ;; Otherwise, change the value associated with the given attribute key
      ;; Store the delta transaction in :this-tx as a record of what happened
      (-> tree
          (update-or-remove a-path n)
          (remove-empty (conj r-path :attrs))
          (update-in [:this-tx] conj [op path k o n])))))

(defn- same-transform? [tree path msgs]
  (= (get-in tree path) msgs))

(defmethod apply-to-tree :transform-enable [tree delta]
  (let [[_ path k msgs] delta
        ;; Get the long path to the node in the tree
        r-path (real-path path)
        ;; Get the path to the transform in the node
        e-path (conj r-path :transforms k)]
    ;; Make sure a transform with the same key does not already exist in the node that contains a different message.    
    (assert (or (not (get-in tree e-path))
                (same-transform? tree e-path msgs))
            (str "A different transform " k " at path " path " already exists."))
    ;; If the same transform already exists
    (if (get-in tree e-path)
      ;; do nothing
      tree
      ;; Otherwise, add the transform to the node, and add the delta
      ;; to :this-tx to record what happened
      (-> tree
          (assoc-in e-path msgs)
          (update-in [:this-tx] conj delta)))))

(defmethod apply-to-tree :transform-disable [tree delta]
  (let [[_ path k] delta
        ;; Get the long path to the node in the tree
        r-path (real-path path)
        ;; Get the path to all the transforms in the node
        transforms-path (conj r-path :transforms)
        ;; Get the path to the transform specified by k in the transforms
        e-path (conj transforms-path k)]
    ;; Get the transform
    (if (get-in tree e-path)
      ;; Make a record of the delta removal in :this-tx in the tree
      ;; Remove the transform fom the node's transforms
      (-> tree
          (update-in [:this-tx] conj (conj delta (get-in tree e-path)))
          (update-in transforms-path dissoc k)
          (remove-empty transforms-path))
      ;; The transform did not exist, so do nothing
      tree)))
 
(defn- node-deltas [{:keys [value transforms attrs]} path]
  "Takes two arguments, a node and a path.
Extract the value, attrs and transforms from the node, and places them all in a vector in a sequence.
e.g.
 (node-deltas {:value :val :attrs {:x :y} :transforms {:g [:h :i]}}  [:a :b])
 ([:value [:a :b] :val] [:attr [:a :b] :x :y] [:transform-enable [:a :b] :g [:h :i]])"
  (concat []
          (when value [[:value path value]])
          (when attrs (vec (map (fn [[k v]]
                                  [:attr path k v])
                                attrs)))
          (when transforms (vec (map (fn [[k v]]
                                   [:transform-enable path k v])
                                 transforms)))))



(defn- map->deltas [tree path]
  "Converts a map with :children :transforms :value and :attrs keys into a series of standard delta vectors.
This will recursively call itself until the entire map structure has been converted"
  ;; A node contains these keys
  (let [node-keys #{:children :transforms :value :attrs}
        ;; If the tree is a map, and it contains at least one of the node keys, node? will be true
        node? (and (map? tree) (not (empty? (set/intersection (set (keys tree)) node-keys))))
        ;; If this is a node, it could have children, so get them, otherwise, just return the tree
        children (if node? (or (:children tree) {}) tree)
        ;; Children can be either :vector or :map
        children-type (node-type children)
        ;; If the tree is a node, then extract the value, attrs and transforms from it
        ;; These will be separate deltas later on
        node-deltas (when node?
                      (node-deltas tree path))]
    ;; Create the node with the given path, which will recursively create all the parent nodes
    ;; if required.
    ;; Then, add the deltas that represent the node's attrs, value and transforms
    ;; Finally, extract all the children nodes, and recursively call map->deltas on it.
    ;; This will extract the children's deltas and standardize them in the vector delta form
    (concat [[:node-create path children-type]]
            node-deltas
            (mapcat (fn [k]
                      (map->deltas (get-in tree (if node? [:children k] [k])) (conj path k)))
                    (cond (= children-type :map)
                          (keys children)
                          (= children-type :vector)
                          (range (count children))
                          :else [])))))

;; This should probably get renamed to standardize delta
(defn expand-map [delta]
  "Convert a delta map into a standard delta vector, or leave it alone"
  (if (map? delta)
    (map->deltas delta [])
    [delta]))

(defn- expand-maps [deltas]
  "Given a series of deltas, which may be in map format, must call expand-map on each of them to convert them all
into the standard delta vector format"
  (mapcat expand-map deltas))

(defn- update-tree
  "Update the tree and return the actual deltas which were used to
update the tree. A single delta can be expanded into multiple deltas."
  [tree deltas]
  (reduce apply-to-tree tree deltas))

(defmethod apply-to-tree :function [tree f]
  (let [deltas (f tree)]
    ;; TODO: What do we do if f causes an error
    (update-tree tree deltas)))

;; Query
;; ================================================================================


;; The tree nodes each have a distinct entity id
(def ^:private next-eid-atom
  "Holds the next entity id"
  (atom 0))

(defn- next-eid []
  "Generates the next entity id"
  (swap! next-eid-atom inc))

(defn- transform->entities [transform-name msgs node-id]
  "Creates the transform entitiy for the given node with the specified messages"
  ;; Generate a new entity id for the transform
  (let [transform-id (next-eid)]
    ;; There are two parts to the transform
    ;; The first is the transform identity, which establishes the transform's
    ;; id, name, type and the node it belongs to
    ;; The second part is all the messages that are associated with transform
    ;; Each of the messages gets its own entity id, is associated with the transform identity
    ;; and its type is :t/message
    (concat [{:t/id transform-id :t/transform-name transform-name :t/node node-id :t/type :t/transform}]
            (map (fn [m] (merge m {:t/id (next-eid) :t/transform transform-id :t/type :t/message})) msgs))))

(defn- transforms->entities [transforms node-id]
  "Creates transform entity map for each transform in transforms for the node id"
  (reduce (fn [acc [transform-name msgs]]
            (concat acc (transform->entities transform-name msgs node-id)))
          []
          transforms))

(defn- attrs->entities [attrs node-id]
  "Creates a new entity for each attr in attrs using the node id"
  (when (not (empty? attrs)) [(merge attrs {:t/id (next-eid) :t/node node-id :t/type :t/attrs})]))

(defn- node->entities [node path parent-id node-id]
  "Extracts the value, attrs and transforms from the node"
  (let [{:keys [value attrs transforms]} node
        ;; The node entity
        node-e {:t/id node-id :t/path path :t/type :t/node :t/segment (last path)}
        ;; Add the parent id to the node entity if it exists
        node-e (if parent-id
                 (assoc node-e :t/parent parent-id)
                 node-e)
        ;; Add the value to the node entity if it exists
        node-e (if value
                 (assoc node-e :t/value value)
                 node-e)
        ;; Create the attr entities for the node
        attrs-es (attrs->entities attrs node-id)
        ;; Create the transform entities for the node
        transform-es (transforms->entities transforms node-id)]
    ;; Return the node entity, attrs entity and the transforms entity
    (concat [node-e] attrs-es transform-es)))

(defn- tree->entities [tree path parent-id]
"Convert a tree structure into a sequence of node entities.  Each entity contains an entity id (:t/id), 
entity type (:t/type), and values specific for the entity, i.e. :t/parent, :t/segment, :t/message, etc"  
  (let [{:keys [children]} tree
        ;; child keys
        ks (child-keys children)
        ;; Assign the next node id
        node-id (next-eid)
        ;; Create entities for each of the value, attrs and transforms in the node
        ;; specified by the path in the tree
        node-tuples (node->entities tree path parent-id node-id)]
    ;; Return the top level node and recursively convert all of its children
    ;; nodes into entities
    (concat node-tuples
            (mapcat (fn [k] (tree->entities (get-in tree [:children k]) (conj path k) node-id))
                    ks))))

(defn- entity->tuples [e]
  "Converts an entity map into a tuple
e.g.
 {:t/id 4 :t/path [:a :b] :t/type :t/node :t/segment :a :t/value 5}
becomes ([4 :t/path [:a :b]] [4 :t/type :t/node] [4 :t/segment :a] [4 :t/value 5])"
  ;; Save the entity id
  (let [id (:t/id e)]
    ;; Convert the entity map into a sequence of vectors
    ;; Where each vector's first element is the :t/id
    ;; The second element is the map's key
    ;; The third element is the map's value
    (map (fn [[k v]] [id k v]) (dissoc e :t/id))))

(defn- entities->tuples [entities]
  "Convert every entity map into a sequence of tuples"
  (mapcat entity->tuples entities))

(defn- tree->tuples [tree]
  "Converts a tree to a series of tuples and vectors"
  (if (:tree tree)
    (entities->tuples
     ;; Convert the tree into map entities
     (tree->entities (:tree tree) [] nil))
    []))

;;A record type that can hold the application state
;; It implements the TupleSource protocol of query
;; This allows the tree to be queried
(defrecord Tree []
  query/TupleSource
  (tuple-seq [this]
    (tree->tuples this)))

(defn delete-deltas [t deltas]
  (reduce (fn [d k]
            (if (< k t)
              (do (log/debug :gc (str "GC: Deleting " (count (get d k)) " deltas at time " k))
                  (dissoc d k))
              d))
          deltas
          (keys deltas)))

(defn gc [state]
  (if *gc-deltas*
    (do (log/debug :gc "GC: Running...")
        (let [t (:t state)
              delete-t (- t 2)]
          (log/debug :gc (str "GC: Deleting t < " delete-t))
          (log/debug :gc (str "GC: There are currently "
                              (count (apply concat (vals (:deltas state))))
                              " deltas."))
          (update-in state [:deltas] (partial delete-deltas delete-t))))
    (do (log/debug :gc (str "GC is turned off. There are "
                            (count (apply concat (vals (:deltas state))))
                            " accumulated deltas"))
        state)))

;; Public API
;; ================================================================================




(defn apply-deltas
  "Given an old tree and a sequence of deltas, return an updated tree.
  The deltas can be a sequence of tuples or a map which can be
  expanded into a sequence of tuples.

  The keyword :commit can be inserted into the stream of deltas to force
  a commit at a specific point."
  [old deltas]
  ;; Get the old sequence and transaction number
  (let [{:keys [seq t]} old
        ;; Convert the deltas into a vector format if they are in map form
        ;; otherwise leave them alone.  
        deltas (expand-maps deltas)
        ;; Using the deltas, update the tree using the old tree
        {:keys [tree this-tx]} (update-tree old deltas)
        ;; The previous deltas may have created their own deltas within the transaction
        ;; Increment the sequence number, and extract each of these deltas
        ;; This may represent the deltas that were used to create the actual transaction
        deltas (map (fn [d s]
                      {:delta d
                       :t t
                       :seq s})
                    this-tx
                    (iterate inc seq))]
    ;; Associate the deltas from this transaction into [:deltas t], where t represents
    ;; the transaction number
    ;; this-tx is now empty, there are no more deltas to process
    ;; There were a certain number of deltas added, therefore, the number of sequences in this
    ;; transaction needs to be increased by this amount
    ;; The updated tree is put in :tree
    ;; The transaction number is incremented
    (-> old
        (assoc-in [:deltas t] deltas)
        (assoc-in [:this-tx] [])
        (update-in [:seq] + (count deltas))
        (assoc-in [:tree] tree)
        (update-in [:t] inc))))

(defn value [tree path]
  (let [r-path (real-path path)]
    (get-in tree (conj r-path :value))))

(defn node-exists? [tree path]
  (let [r-path (real-path path)]
    (get-in tree r-path)))


(def new-app-model
  (map->Tree
   {:deltas {}           ;; all changes made to the tree indexed by time
    :this-tx []          ;; in-transaction deltas
    ;; Outside of a transaction this will always be empty, in a
    ;; transaction it will accumulate deltas to be committed at the
    ;; end of the transaction.
    :tree nil            ;; the current tree
    :seq 0               ;; the next available seq number
    :t 0                 ;; the next available transaction number
    }))

(defn t
  "Get the current tree time value."
  [tree]
  (:t tree))

(defn since-t
  "Get all deltas since time t, inclusive."
  [tree t]
  (let [ts (range t (:t tree))]
    (vec (map :delta (mapcat #(get (:deltas tree) %) ts)))))




(comment
  ;; The standardized deltas
  ;; is a list/sequence where each element contains a vector.  The vector contains either:
  ;; node-create, node-destroy, transform-enable, value, attr or transform-disable.
  ;; op path value(s)
  #_([:node-create [] :map] [:node-create [:a] :map] [:value [:a] 42] [:attr [:a] :color :red] [:attr [:a] :size 10] [:transform-enable [:a] :x [{:y :z}]] [:node-create [:a :b] :map] [:node-create [:a :b :c] :vector] [:node-create [:a :b :c 0] :map] [:value [:a :b :c 0] 2] [:transform-enable [:a :b :c 0] :f [{:x :p}]] [:node-create [:a :b :c 1] :map] [:value [:a :b :c 1] 3] [:attr [:a :b :c 1] :color :blue])


  ;; This is how the app tree is specified:
;; :deltas are indexed by the transaction id
;; e.g. {0 {:delta [delta-vector] :seq 0 :t 0
;;         {:delta [delta-vector] :seq 1 :t 0
;;       1 {:delta [delta-vector] :seq 2 :t1}}
;; Multiple deltas can have the same :t, but can't have same seq number

;; The tree is represented in long form as:
;; {:children {:x {:value 42, :attrs {}, :transforms: {}, :children {}}}}
;; children can be maps or vectors

;; delta values:
;; [:value [path] <old-value> <new-value>]
;; delta attrs
;; [:attrs [path] <key> <old-value> <new-value>]

  


  )
