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

(defn field-pairs [s]
  (partition 2 (for [pair (string/split s #",") x (string/split pair #":")] x)))

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
  (let [map-sym (gensym)
        field-nodes (-> nodes (select [(attr? :field)]))
        ts (map (fn [x] (-> x :attrs :field)) field-nodes)
        field-map (field-map ts)
        index (reduce (fn [a k]
                        (assoc a (str "~{" k "}") (list 'get map-sym (keyword k))))
                      {}
                      (keys field-map))
        nodes (reduce (fn [a field-value]
                        (make-template a field-value))
                      nodes
                      ts)
        nodes (-> nodes
                  (transform [(attr? :field)] (remove-attr :field))
                  (transform [(attr? :template)] (remove-attr :template)))
        seq (emit* nodes)
        seq (remove #(= (set %) #{\space \newline}) seq)
        seq (reduce (fn [a b]
                      (conj a (if (contains? index b)
                                (get index b)
                                b)))
                    []
                    seq)
        seq (simplify-tseq seq)]
    (list 'fn [map-sym]
          (cons 'str seq))))

(defn- change-index [fields]
  (apply merge (for [x fields y (field-pairs x)]
                 {(keyword (second y))
                  (merge {:field x
                          :type (if (= (first y) "content")
                                  :content
                                  :attr)
                          :attr-name (first y)})})))

(defn make-dynamic-template [nodes key info]
  (reduce (fn [a [k v]]
            (transform a [[(attr= :field (:field info))]]
                       (set-attr :field (str (:field info) "," "id:" (:id info)))))
          nodes
          (field-pairs (:field info))))

(defn dtfn [nodes static-fields]
  (let [map-sym (gensym)
        field-nodes (-> nodes (select [(attr? :field)]))
        ts (map (fn [x] (-> x :attrs :field)) field-nodes)
        ts-syms (reduce (fn [a x]
                          (assoc a x (gensym)))
                        {}
                        ts)
        change-index (reduce (fn [a [k v]]
                               (assoc a k (assoc v :id (get ts-syms (:field v)))))
                             {}
                             (change-index ts))
        changes (reduce (fn [a [k v]]
                          (if (contains? static-fields k)
                            a
                            (assoc a k v)))
                        {}
                        change-index)
        nodes (reduce (fn [a [k info]]
                        (make-dynamic-template a key info))
                      nodes
                      changes)
        changes (reduce (fn [a [k v]]
                          (assoc a k (-> (if (= (:type v) :content) (dissoc v :attr-name) v)
                                         (dissoc :field))))
                        {}
                        changes)
        ids (map :id (vals changes))]
    (list 'fn [] (list 'let (vec (interleave ids (repeat (list 'gensym))))
           [changes (list 'fn [map-sym]
                          (list (tfn nodes)
                                (concat ['assoc map-sym]
                                        (interleave (map keyword ids)
                                                    ids))))]))))

(defn tnodes
  ([file name]
     ;; Extract the html from the file
     ;; Then select all the nodes that have template tags
     ;; that match the template name
     (select (html-resource file) [(attr= :template name)]))
  ([file name empty]
     (reduce (fn [a b]               
               (transform a b (html-content "")))
             (construct-html (tnodes file name))
             empty)))

(defn template-children [file name]
  (select (html-resource file) [(attr= :template name) :> :*]))


