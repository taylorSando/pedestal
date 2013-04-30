; Copyright 2013 Relevance, Inc.

; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0)
; which can be found in the file epl-v10.html at the root of this distribution.
;
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
;
; You must not remove this notice, or any other, from this software.

(ns io.pedestal.app.render.push.templates
  (:require [io.pedestal.app.render.push :as render]
            [domina :as d]))

(defn sibling [path segment]
  (conj (vec (butlast path)) segment))

(defn parent [path]
  (vec (butlast path)))

(defn update-template [t m]
  "Given a template and a new template field data map"
  ;; The template map is a map that contains
  ;; a field identifier as a key, and a map as a value.
  ;; The map value contains an id, type and attr name
  (doseq [[k {:keys [id type attr-name]}] t]
    ;; Determine the field type of the template
    (case type
      ;; It's an attribute
      ;; If the new data map contains the field identifier
      ;; but its value is nil
      :attr (cond (and (contains? m k) (nil? (get m k)))
                  ;; Remove the attribute from the dom
                  (d/remove-attr! (d/by-id id) attr-name)
                  ;; If it has the key in the map
                  (contains? m k)
                  ;; Update the attribute stored in the dom
                  (d/set-attrs! (d/by-id id) {attr-name (get m k)}))
      ;; If the attribute is content, and the map contains the field identifier
      :content (when (contains? m k)
                 ;; Update the content stored in the dom
                 ;; to be the value stored in the new map data stored at the field
                 ;; identifier key
                 (d/set-html! (d/by-id id) (get m k)))
      nil)))

(defn- add-in-template [f t m]
  "f is either prepend, or append.  t is the previous template, and m is the new
   template data"
  ;; k is the field identifier, and v is the template field data
  (doseq [[k v] m]
    ;; The type stored with the field identifier should be content
    (assert (= (get-in t [k :type]) :content)
            "You may only add to content.")
    ;; If the template contains the field identifier
    (when (contains? t k)
      ;; Use f, which is either append/prepend, and insert the content
      ;; data into the dom at the location specified by the id
      (f (d/by-id (get-in t [k :id])) v))))

(defn update-t [r path data]
  "Updates the template data that is stored in the renderer at the path."
  ;; Get the old template value stored in the path location
  (let [template (render/get-data r (conj path ::template))]
    ;; Update the template using the data
    (update-template template data)))

(defn prepend-t [r path data]
  "Prepend data to the template specified in path. Path must specify
   a template with a content field type"
  (let [template (render/get-data r (conj path ::template))]
    (add-in-template d/prepend! template data)))

(defn append-t [r path data]
  "Append the data to the template specified in path.  Path must specify
   a template with a content field type"
  (let [template (render/get-data r (conj path ::template))]
    (add-in-template d/append! template data)))

(defn update-parent-t [r path data]
  "Update the parent template with the new data"
  (let [template (render/get-data r (conj (parent path) ::template))]
    (update-template template data)))

(defn add-template [r path make-template]
  "When given r (DomRenderer), path, and make-template, a template function.
   This will add the template to the renderer at [path ::template] and return
   a function that takes a map, and returns an html string.
   make-template is expected to be a function that returns a vector of two elements.
   The first element is the template map, and the second element is an html string
   generator that takes a map as an argument to generate html output as a string. "
  (let [[template html] (make-template)]
    ;; Put the template map in the [path ::template] location in the renderer
    (render/set-data! r (conj path ::template) template)
    ;; Return the html generator
    html))
