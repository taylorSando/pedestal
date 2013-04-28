; Copyright 2013 Relevance, Inc.

; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0)
; which can be found in the file epl-v10.html at the root of this distribution.
;
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
;
; You must not remove this notice, or any other, from this software.

(ns io.pedestal.app.templates
  "HTML templating for Clojure and ClojureScript.

  * Combine HTML fragments into complete documents

  * Extract HTML fragments from documents for use in ClojureScript

  * Generate functions which return 'filled-in' HTML when passed a map
    of values

  * Generate functions/data which can be used to dynamically updated
    templates after they have been added to the DOM

  The functions in this namespace are either called from Clojure and
  used from macros to produce functions or data which can be used in
  ClojureScript.
  "
  (:use net.cgrand.enlive-html)
  (:require [clojure.string :as string])
  (:import java.io.File))

(defn render
  "Given a seq of Enlive nodes, return the corresponding HTML string."
  [t]
  (apply str (emit* t)))

(declare construct-html)

;; We need to use this instead of Enlive's html-snippet, because
;; html-snippet throws away the doctype
(defn html-parse
  "Parse a string into a seq of Enlive nodes."
  [s]
  (html-resource (java.io.StringReader. s)))

(defn- html-body [name]
  "Extracts the body content from the file if it exists, or returns the sequence of nodes if there is no body."
  (let [nodes (html-resource name)]
    (or
     (:content (first (select nodes [:body])))
     nodes)))

(defn- include-html [h]
  "Locate and replace all of the _include html tags with the file content that they specify"
  ;; Find all the _include tags in the html
  (let [includes (select h [:_include])]
    ;; h is the html
    (loop [h h
           includes (seq includes)]
      ;; Make sure there are still includes
      (if includes
        ;; Extract the first include's file attribute
        (let [file (-> (first includes) :attrs :file)
              ;; Extract the include file's html
              ;; This could cause a recursive call, because construct-html while look for
              ;; includes within the file.  This will find all child includes.
              include (construct-html (html-body file))]
          ;; At this point, all the child includes for this include should have been located
          ;; The include that had the file attribute above will be replaced by what was
          ;; found in the include above
          ;; Using this new html, start processing the next top level include
          (recur (transform h [[:_include (attr= :file file)]] (substitute include))
                 (next includes)))
        h))))

(defn- maps [c]
  "Extract all the maps from the sequence"
  (filter map? c))

(defn- replace-html [h c]
  "Within the html, h, substitute with the content, c, where the id and tag in c match
   within h."
  ;; Get the id attribute of the content
  (let [id (-> c :attrs :id)
        ;; Extract the tag from the content
        tag (:tag c)
        ;; Create a css selector for the id and tag so that it
        ;; can be selected
        selector (keyword (str (name tag) "#" id))]
    ;; In the html, find the content that matches the selector
    ;; and substitute it with the content
    (transform h [selector] (substitute c))))

(defn- wrap-html [h]
  "Insert the content inside the within tags in the html that is specified by the file attribute of the
   _within tag.
   Given an html snippet, h, locate the within tags inside, and grab the content inside of it.  Then,
   identify the file that correspondsd to the file attribute of the _within tag.  The idea is that the
   content inside the _within tag matches entities in the file specified by the attribute of _within.
   For example, let us say that there is a file called sample.html.  It contains the following:
   <_within file=\"example.html\"><div id=\"content\">I am the new content</div>
   This means that the file example.html should contain a reference to <div id=\"content\"></div>
   and that the inner html of div#content will be replaced by I am the new content, which was specified
   inside of sample.html"
  ;; Select all of the within tags
  (let [within (seq (select h [:_within]))]
    ;; Make sure there are tags to process
    (if within
      ;; Extract the file attribute within the first within tag
      (let [file (-> (first within) :attrs :file)
            ;; Extract the html from the file (this could have includes within it)
            outer (construct-html (html-resource file))
            ;; Grab the content that is a map in the first within tag
            ;; This would be all the child content that are tags, not text
            content (maps (:content (first within)))]        
        (loop [outer outer
               content (seq content)]
          ;; If there is child content to process
          (if content
            ;; Within the outer content, need to stick the content inside of it
            ;; replacing where the id/tag of the content match in the outer html
            (recur (replace-html outer (first content)) (next content))
            outer)))
      h)))

(defn construct-html
  "Process a seq of Enlive nodes looking for `_include` and `_within` tags.
  Occurrences of `_include` are replaced by the resource to which they
  refer. The contents of `_within` tags are inserted into the resource
  to which they refer. `_within` is always the top-level tag in a file.
  `_include` can appear anywhere. Files with `_include` can reference
  files which themselves contain `_include` or `_within` tags, to an
  arbitrary level of nesting.

  For more information, see '[Design and Templating][dt]' in the project
  wiki.

  Returns a seq of Enlive nodes.

  [dt]: https://github.com/brentonashworth/one/wiki/Design-and-templating"
  [nodes]
  (wrap-html (include-html nodes)))

(defn load-html
  "Accept a file (a path to a resource on the classpath) and return a
  HTML string processed per construct-html."
  [file]
  (render (construct-html (html-resource file))))



;; Convert a snippent of HTML into a template function

(defn- split-field-pair [fp]
  "Splits a raw field pair string into its component parts, where the first part is the template type,n 
   and the second part is the template identifier"
  (string/split fp #":"))

(defn field-pairs [s]
  "Converts a template string into a series of field pairs.
   The strings in the s argument represent raw field pairs, i.e. id:id, or content:name. These field pairs can themselves
   be delimited by commas, i.e. id:id, content:name.  The goal is to return a sequence of sequences of these field pairs
   e.g.
   id:id becomes ((\"id\" \"id\"))
   id:id,content:name becomes ((\"id\" \"id\") (\"content\" \"name\")"
  (partition 2 (for [pair (string/split s #",") x (split-field-pair pair)] x)))

(defn field-map [coll]
  (reduce (fn [a [k v]] (assoc a v k))
          {}
          (mapcat field-pairs coll)))

(defn make-template [nodes field-value]
  (reduce (fn [a [k v]]
            (transform a [[(attr= :field field-value)]]
                       (if (= k "content")
                         (html-content (str "~{" v "}"))
                         (set-attr (keyword k) (str "~{" v "}")))))
          nodes
          (field-pairs field-value)))

(defn simplify-tseq
  "Concats strings together in templates to optimize them slightly"
  [s]
  (mapcat 
   #(if (string? (first %))
      [(apply str %)]
      %)
   (partition-by string? s)))

(defn tfn [nodes]
  ;; 
  (let [map-sym (gensym)
        ;;field-nodes (-> nodes (select [(attr? :field)]))
        ;;ts (map (fn [x] (-> x :attrs :field)) field-nodes)
        ts (extract-field-pairs nodes)
        field-map (field-map ts)
        _ (println "Field Map")
        _ (println field-map)
        index (reduce (fn [a k]
                        (assoc a (str "~{" k "}") (list 'get map-sym (keyword k))))
                      {}
                      (keys field-map))
        _ (println "Index")
        _ (println index)
        nodes (reduce (fn [a field-value]
                        (make-template a field-value))
                      nodes
                      ts)
        _ (println "New Nodes")
        _ (println nodes)
        nodes (-> nodes
                  (transform [(attr? :field)] (remove-attr :field))
                  (transform [(attr? :template)] (remove-attr :template)))
        _ (println "New Nodes 2")
        _ (println nodes)
        seq (emit* nodes)
        _ (println "First Seq")
        _ (println seq)
        seq (remove #(= (set %) #{\space \newline}) seq)
        _ (println "SEcond seq")
        _ (println seq)
        seq (reduce (fn [a b]
                      (conj a (if (contains? index b)
                                (get index b)
                                b)))
                    []
                    seq)
        _ (println "Third seq")
        _ (println seq)
        seq (simplify-tseq seq)]
    (println "Fourth Seq")
    (println seq)
    (println "Raw")
    (println (cons 'str seq))
    (println "Fn")
    (println (list 'fn [map-sym]
          (cons 'str seq)))
    (list 'fn [map-sym]
          (cons 'str seq))))

(defn- convert-field-pair-to-map [fp]
  "Converts the raw field pair string (type:identifier) into a map that contains the field pair identifier
   as converted to a keyword as the key and a new map as the value.  The new value is a map itself, with keys :field, 
   :type, and :attr-name.
   :field is the raw field pair string
   :type is the type of template, either :attr or :content
   :attr-name is the string name of template type (the first part of the raw field pair) "
  (let [[type identifier] (split-field-pair fp)]
    {(keyword identifier)
     {:field fp
      :type (if (= type "content")
              :content
              :attr)
      :attr-name type}}))

(defn- convert-field-pairs-to-map [all-field-pairs]
  "Converts a sequence of field pairs to a map.  The map has keys that are the field pair identifiers and the
   values are properties corresponding to the type of the field pair
   e.g. (\"id:id-5\" \"content:name\"
   {:id-5 {:field id:id-5, :type :attr, :attr-name id}
    :name {:field content:name :type :content :attr-name content}}"
  (reduce (fn [a fp]
          (merge a (convert-field-pair-to-map fp)))
        {}
        all-field-pairs))

(defn make-dynamic-template [nodes key info]
  ;; Gives a sequence of enlive nodes, a template key, and template value (info)
  ;; Insert the template information into nodes that have a field attribute matching the template
  ;; field pair key
  ;; e.g. nodes is ({:tag :div, :attrs {:field id:id-5})
  ;; key is :id-5
  ;; info is {:id G_30001 :field id:id-5, :type :attr, :attr-name id}
  (reduce (fn [a [k v]]
            ;; Transform any node that has a field attribute matching
            ;; the template info :field value, by adding the field pair string
            ;; followed by an id identifying the symbol
            ;; In this example, it would add :id:id-5,G_30001
            (transform a [[(attr= :field (:field info))]]
                       (set-attr :field (str (:field info) "," "id:" (:id info)))))
          nodes
          ;; Extract the field pairs from the raw string field pair that is held within
          ;; the info map
          (field-pairs (:field info))))

(defn- extract-field-pairs [nodes]
  "Gets all the field pairs in the nodes"
  (let [field-nodes (select nodes [(attr? :field)])]
    (map (fn [x] (-> x :attrs :field)) field-nodes)))

(defn- field-to-symbol-mapping [fields]
  "When given a sequence of template fields, returns a map containing the fields string as a key
   and a unique symbol as their values.  This is useful later because we can map all instances of
   the same field string pair to the same symbol.
   e.g.
   (id:id-5 content:name) would become {id:id-5 G_3002, content:name G_3003}"
  (reduce (fn [a x]
            (assoc a x (gensym)))
          {}
          fields))

(defn- create-template-map [ts ts-syms]
  "When given a sequence of template field pairs, and a map of their symbols, create a template map that will
   map the template identifier to a map containing the various template properties, including its symbol
   e.g.
   (create-template-map '(\"id:id-5\" \"content:name\") {\"id:id-5\" G_3001, \"content:name\" G_3002})
   would become:
   {:id-5 {:field id:id-5 :id G_3001 :attr-name id :type :attr}
    :name {:field content:name :id G_3002 :attr-name content :type :content}} "
  (reduce (fn [a [k v]]
            (assoc a k (assoc v :id (get ts-syms (:field v)))))
          {}          
          (convert-field-pairs-to-map ts)))

(defn- insert-template-symbols-into-nodes [nodes t-map]
  "Go through the template map, extract the symbol ids and insert them into their corresponding locations in the
   nodes."
  (reduce (fn [a [k info]]
            (make-dynamic-template a key info))
          nodes
          t-map))

(defn- remove-unneeded-template-fields [t-map]
  (reduce (fn [a [k v]]
            ;; If the type of the template is :content
            ;; Remove the :attr-name from the map, otherwise,
            ;; leave it alone
            ;; In all cases, remove the :field attribute from the map
            (assoc a k (-> (if (= (:type v) :content) (dissoc v :attr-name) v)
                           (dissoc :field))))
          {}
          t-map))

(defn- select-template-symbol-ids [t-map]
  (map :id (vals t-map)))


(defn- dtfn1 [map-sym ids changes nodes]
  (list 'fn [] (list 'let (vec (interleave ids (repeat (list 'gensym))))
                     [changes (list 'fn [map-sym]
                                    (list (tfn nodes)
                                          (concat ['assoc map-sym]
                                                  (interleave (map keyword ids)
                                                              ids))))])))

(defn dtfn [nodes static-fields]
  (let [map-sym (gensym)
        ;; Get all the template field pairs as a sequence of strings
        ts (extract-field-pairs nodes)
        ;; Map all the template field pairs to unique symbols
        ts-syms (field-to-symbol-mapping ts)
        ;; Map the template pair identifiers to template properties
        t-map (create-template-map ts ts-syms)
        ;; Not really sure what this is doing
        ;; It's looking for keys in t-map that match the sequence that is static-fields
        ;; It doesn't seems to change anything whether it matches or not
        changes (reduce (fn [a [k v]]
                          (if (contains? static-fields k)
                            a
                            (assoc a k v)))
                        {}
                        t-map)        
        nodes (insert-template-symbols-into-nodes nodes changes)
        changes (remove-unneeded-template-fields changes)
        ;; Go through all the template maps and extract the symbol ids
        ids (select-template-symbol-ids changes)]
    (println "Final Template Map")
    (println changes)
    (dtfn1 map-sym ids changes nodes)))

(defn tnodes
  ([file name]
     ;; Extract the html from the file
     ;; Then select all the nodes that have attributes named template, and whose values are equal to name
     (select (html-resource file) [(attr= :template name)]))
  ([file name empty]
     (reduce (fn [a b]               
               (transform a b (html-content "")))
             (construct-html (tnodes file name))
             empty)))

(defn template-children [file name]
  (select (html-resource file) [(attr= :template name) :> :*]))




