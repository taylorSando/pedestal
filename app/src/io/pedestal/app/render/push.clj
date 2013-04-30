; Copyright 2013 Relevance, Inc.

; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0)
; which can be found in the file epl-v10.html at the root of this distribution.
;
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
;
; You must not remove this notice, or any other, from this software.

(ns ^:shared io.pedestal.app.render.push
  "A Renderer implementation for the DOM which supports push
  rendering. Provides functions which help to map an application model
  to the DOM."
  (:require [io.pedestal.app.protocols :as p]
            [io.pedestal.app.util.platform :as platform]
            [io.pedestal.app.util.log :as log]
            [io.pedestal.app.tree :as tree]))

;; Handlers
;; ================================================================================

(def ^:private search-ops {:node-create #{:node-* :*}
                           :node-destroy #{:node-* :*}
                           :value #{:value :*}
                           :attr #{:attr :*}
                           :transform-enable #{:transform-* :*}
                           :transform-disable #{:transform-* :*}})

(defn- real-path [op path]
  "Given an operation and path, converts them into the expanded tree form
   (real-path {} :op [:a :b :c])
   (:op :children :a :children :b :children :c :handler)"
  (cons op (conj (vec (interleave (repeat :children) path)) :handler)))

(defn add-handler [handlers op path f]
  "Add the given operation to the path in handlers with the function f as the handler"
  ;; real-path represents the expanded tree form of an application state map
  ;; It places a handler function there so that it can process changes that occur
  ;; that are relevant to the path and operation
  (assoc-in handlers (real-path op path) f))

(defn add-handlers
  "Add multiple handlers to the handler"
  ([hs]
     (add-handlers {} hs))
  ([m hs]
     (reduce (fn [acc [op path f]]
               (add-handler acc op path f))
             m
             hs)))

(defn- matching-keys [ks p]
  "Given a sequence of keys, attempt to find which ones match p"
  (filter (fn [k]
            ;; If the key and p are the same, they are a match
            (or (= k p)
                ;; If k is :*, it's a match
                (= k :*)
                ;; If k is :**, it's a match
                (= k :**)
                ;; When p is a key in the search-ops, like node-create, value, etc
                (when (contains? search-ops p)
                  ;; Find out if k is equal to the special ops, like node :*, attr :*, etc
                  (contains? (p search-ops) k))))
          ks))

(defn- sort-keys [ks]
  "This will sort the given keys, making sure to eliminate any duplicate :**,and making sure that :* and :**
   end up at the end of the list"
  ;; Remove any key that matches :**, need to eliminate possible duplicates
  ;; sorted-keys is now a sequence
  (let [sorted-keys (remove #(= % :**) (sort ks))]
    ;; Find out if any of the keys were removed
    (reverse (if (> (count ks) (count sorted-keys))
               ;; We know that a :** was removed, so add it back to the start of the list
               (conj sorted-keys :**)
               ;; Return the sequence
               sorted-keys))))

(defn- select-matches [handlers p]
  "Selects all the potential matches for the path in handlers
   Returns a sequence of vectors, [k handler-path], where k is the
   possible matching path to the handler path"
  ;; Find all keys that match the given path within handlers  
  (let [keys (matching-keys (keys handlers) p)]
    ;; Sort the candidate keys, whereby :* and :** will appear at the end
    ;; Return a sequence of vectors, [<key> <handler-fn>]
    (map (fn [k] [k (get handlers k)]) (sort-keys keys))))

(defn- find-handler* [handlers path]
  "Given a set of handlers, attempt to locate the most specific handler using path"
  ;; If the path is empty
  (if (empty? path)
    ;; Return all handlers at the root
    (:handler handlers)
    ;; Otherwise, must find more specific handler that match the path
    (some (fn [[k v]]
            ;; Attempt to find a more specific handler within the path
            ;; This is a recursive call
            (if-let [handler (find-handler* v (rest path))]
              ;; Found the most specific path
              handler
              (when (= k :**) (:handler v))))
          ;; Select all candidate matches, starting from the first element in the path
          (select-matches (:children handlers) (first path)))))

(defn find-handler [handlers op path]
  "Given a set of handlers, an operation, and a path, find the handler that satisfies
   the operation and path.     
   For example, it will prefer to match :node-create [:a :b] over :node-create [:a :**]
   :* is a wild card, but only matches a single path element
   :** is a wild card, but can math more than one path element
   For example, [:a :b :*] would match [:a :b :c], but not [:a :b :c :d]
   [:a :b :**] would match [:a :b :c], [:a :b :c :d], and [:a :b :c :d :e]"
  (find-handler* {:children handlers} (vec (cons op path))))

;; Rendering
;; ================================================================================

(defprotocol DomMapper
  (get-id [this path])
  (get-parent-id [this path])
  (new-id! [this path] [this path v]
    "Create a new id for this given path. Store this id in the renderer's environment.
    Returns the generated id. An id can be provided as a third
    argument.")
  (delete-id! [this path]
    "Delete this id and all information associated with it from the
    environment. This will also delete all ids and information
    associated with child nodes.")
  (on-destroy! [this path f]
    "Add a function to be called when the node at path is destroyed.")
  (set-data! [this ks d])
  (drop-data! [this ks])
  (get-data [this ks]))

(defn- run-on-destroy!
  "Given a node in the environement which is going to be deleted, run all on-destroy
  functions in the tree."
  [env]
  (let [nodes (tree-seq (constantly true)
                        (fn [n]
                          (map #(get n %) (remove #{:id :on-destroy :_data} (keys n))))
                        env)]
    (doseq [f (mapcat :on-destroy nodes)]
      (f))))

(defrecord DomRenderer [env]
  DomMapper
  (get-id [this path]
    (if (seq path)
      (get-in @env (conj path :id))
      (:id @env)))
  (get-parent-id [this path]
    (when (seq path)
      (get-id this (vec (butlast path)))))
  (new-id! [this path]
    (new-id! this path (gensym)))
  (new-id! [this path v]
    (log/info :in :new-id! :msg (str "creating new id " v " at path " path))
    (swap! env assoc-in (conj path :id) v)
    v)
  (delete-id! [this path]
    (run-on-destroy! (get-in @env path))
    (swap! env assoc-in path nil))
  (on-destroy! [this path f]
    (swap! env update-in (conj path :on-destroy) (fnil conj []) f))
  (set-data! [this ks d]
    ;; TODO: Use namespaced keywords
    (swap! env assoc-in (concat [:_data] ks) d))
  (drop-data! [this ks]
    (swap! env update-in (concat [:_data] (butlast ks)) dissoc (last ks)))
  (get-data [this ks]
    (get-in @env (concat [:_data] ks))))

(defn renderer
  "Given a root-id, a set of handlers, and an optional log-fn, returns a render-fn 
   that can be used to consume an app-model queue.  The handlers are designed to
   handle deltas.  The handler functions in handlers receieve three arguments:
   (1) The renderer, which is a DomRenderer instance, which is a generalized DOM element
   (2) The delta message
   (3) The input queue"
  ([root-id handlers]
     ;; Use identity as the log-fn
     (renderer root-id handlers identity))
  ([root-id handlers log-fn]
     ;; If the handlers are in vector form, convert them, otherwise, use what was given
     (let [handlers (if (vector? handlers) (add-handlers handlers) handlers)
           ;; Create a DOM Renderer, which will act as the DOM for the application
           renderer (->DomRenderer (atom {:id root-id}))]
       ;; Return a render-fn that will process deltas from the app-model queue
       (fn [deltas input-queue]
         ;; Log the deltas
         (log-fn deltas)
         ;; Handle each individual application delta
         (doseq [d deltas]
           ;; Get the delta operation and path
           (let [[op path] d
                 ;; Find the matching handler
                 handler (find-handler handlers op path)]
             ;; When there is a matching handler
             ;; Let the handler deal with the delta             
             (when handler (handler renderer d input-queue))))))))

;; The handler functions can update the view, change the dom, or update the application state
;; by placing messages onto the input queue
