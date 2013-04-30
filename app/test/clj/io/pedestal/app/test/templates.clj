(ns io.pedestal.app.test.templates
  (:require [io.pedestal.app.templates :as tmp]
            [net.cgrand.enlive-html :as enlive]
            [clojure.java.io :as io])
  (:use clojure.test))

;; Useful for determining the current file name
(def source-loc nil)
;; Grab the file information, and get its parent, this is the current directory
;; Useful for creating temporary files later for testing
;; *file* does not seem to work when using a repl
(def template-file-loc
  (.getParent (java.io.File. (:file (meta #'source-loc)))))

;; This is for html-resource, which loads strings, but is relative to the classpath locations
;; These are located in various places.  Need to makes ure that where files are getting created for
;; testing line up to where html-resource can find them on the classpath
(def relative-path-to-template-loc
  ;; This is stripping the project root directory and /test/ from the absolute path
  ;; to create a relative path that can be used by html-resource when loading
  (clojure.string/replace template-file-loc (re-pattern (str (System/getProperty "user.dir") "/test/")) ""))


;; Created this macro to simplify working with html-resource, and to be able to create
;; temporary html files without polluting the test directory with a bunch of html files
(defmacro with-html-files
  "Supply a symbol, file name, and html content.  Creates the html page behind the scenes, where it can be referred to
   by the symbol, which is a classpath relative filename to the html resource.  It is used to work with html-resource,
   which expects string filenames to be relative to the classpath.  This creates the files in the current directory,
   allowing them to be used with anything that uses html-resource, like load-html, and then deletes these files after 
   you're outside of the with-html-files bindings.

   Usage:

   (with-html-files a \"a.html\" \"<div>I am content\"</div>)
   Internally, a now refers to the relative filename to the html content
   (tmp/load-html a) will now generate the proper html, because it is loading the file properly.
   Outside of with-html-file, the a.html will no longer exist
   Can be used with more than one form
   (with-html-file a a-filename a-content b b-filename b-content ...) "
  [bindings & body]  
  (cond
   (= (count bindings) 0) `(do ~@body)
   (symbol? (bindings 0)) `(let [~(bindings 0) (str relative-path-to-template-loc "/" ~(bindings 1))]
                             (let [absf# (str template-file-loc "/" ~(bindings 1))]                                
                               (try
                                 (spit absf# ~(bindings 2))
                                 (with-html-files ~(subvec bindings 3)                                    
                                   ~@body)
                                 (finally
                                   (io/delete-file absf#)))))
   :else (throw (IllegalArgumentException.
                 "with-open only allows Symbols in bindings"))))

(defn remove-html-whitespace [h]
  "A helper method for removing unnecessary whitespace in html, aides in testing if two html strings are the same.
   Don't need to worry about whitespace not being equal.  Good for converting enlive nodes into a plain html string and
   comparing them to other nodes that have been converted"
  (->
   (clojure.string/replace  h #"\n" "")
   (clojure.string/replace  #" {2,}" " ")
   clojure.string/trim))

(deftest test-remove-html-whitespace  
  (is (= (remove-html-whitespace "<div>
       <div>
           <div>
             </div>
                </div>
                      </div>")
         (remove-html-whitespace "<div>
                                             <div>
           <div>
             </div>
                  </div>
           </div>"))))


(defn html-string-equality [s1 s2]
  "Convenience method for testing html string equality, without having to specify remove-html-whitespace for both strings"
  (= (remove-html-whitespace s1)
     (remove-html-whitespace s2)))

(deftest test-remove-html-whitespace
  (let [s1 "<div>
                 <div>
                     <div>
                       </div>
                     </div>
            </div>"
        s2 "<div>
                                             <div>
                        <div>
             </div>
                  </div>
                                 </div>"]
    (is (= (html-string-equality s1 s2)))
    (is (= (html-string-equality s2 s1)))))

(deftest test-html-parse
  "Make sure that the proper enlive data structure is generated from a valid string of html.  
   It automatically adds html and body if  it does not already exist in the string"
  (is (= (tmp/html-parse "<div><a href='http://www.facebook.com/'>Facebook</a></div>")
         '({:tag :html, :attrs nil, :content
            ({:tag :body, :attrs nil, :content
              ({:tag :div, :attrs nil, :content
                ({:tag :a, :attrs {:href "http://www.facebook.com/"}, :content ("Facebook")})})})}))))

(deftest test-render
  "Make sure that it converts an enlive data structure back to an html string"
  (is (= (tmp/render '({:tag :html, :attrs nil, :content
                        ({:tag :body, :attrs nil, :content
                          ({:tag :div, :attrs nil, :content
                            ({:tag :a, :attrs {:href "http://www.facebook.com/"}, :content ("Facebook")})})})}))
         "<html><body><div><a href=\"http://www.facebook.com/\">Facebook</a></div></body></html>")))


(defn enlive-html-equality [nodes1 nodes2]
  "When given two enlive sequence of nodes, tests whether they are equal or not"
  (= (remove-html-whitespace (tmp/render nodes1))
     (remove-html-whitespace (tmp/render nodes2))))

(deftest test-enlive-html-equality
  (testing
      "Need to make sure that html strings that specify the same dom content are equal, even if the string may
       contain different whitespace characters that don't actually affect the dom structure"
    (let [s1 "<div id='id-5><div class='inner'>I am some content</div></div>"
          s2 "<div id='id-5>
                <div class='inner'>
                   I am some content
                </div>
             </div>"]
      (is (= (enlive-html-equality (tmp/render s1) (tmp/render s2)))))))

(deftest test-html-body
  (testing "Should extract the body content from an html file"
    (with-html-files [h "hello.html" "<html><body><div>The div content</div></body></html>"]
      (is (= (@#'tmp/html-body h)
             '({:tag :div :attrs nil :content ("The div content")})))))
  (testing "When there is no body or html in the file, it will just return the nodes if it's valid html"
    (with-html-files [h "hello.html" "<div>The div content</div>"]
      (is (= (@#'tmp/html-body h)
             '({:tag :div :attrs nil :content ("The div content")}))))))

(deftest test-load-html
  (testing "Should load a plain bit of html without any includes"
    (with-html-files [h "hello.html" "<html>
                                         <body>
                                             <div>The div content</div>
                                         </body>
                                       </html>"]
      (is (html-string-equality (tmp/load-html h)
                                "<html>
                 <body>
                   <div>The div content</div>
                 </body>
               </html>"))))
  (testing "Should be able to load a single include file.  The parent loads the child content"    
    (let [parent-content (format "<html>
                                     <body>
                                       <_include file='%s/child.html' />
                                     </body>
                                  </html>" relative-path-to-template-loc)
          child-content "<div>I am from the child</div>"]
      (with-html-files [parent "parent.html" parent-content child "child.html" child-content ]
        (is (html-string-equality (tmp/load-html parent)
                                  "<html>
                     <body>
                       <div>I am from the child</div>
                      </body>
                   </html>"))))) 
  (testing
      "Should be able to load a child file that has a within tag, referencing the parent.  The child will be expanded
       into the parent's content"
    (let [parent-content (format "<html>
                                    <body>
                                       <div id='content'></div>
                                    </body>
                                   </html>" relative-path-to-template-loc)
          child-content (format "<_within file='%s/parent.html'>
                                      <div id='content'>I am child content</div>
                                  </within>"
                                relative-path-to-template-loc)]
      (with-html-files [parent "parent.html" parent-content child "child.html" child-content ]
        (is (html-string-equality (tmp/load-html child)
                                  "<html>
                                         <body>
                                            <div id=\"content\">I am child content</div>
                                         </body>
                                     </html>")))))

  (testing
      "Load a child file, which has a within tag.  It references a parent.  The parent has a reference to an include tag.
       The include tag is a reference to a second child."
    (let [parent-content (format "<html>
                                     <body>
                                       <_include file='%s/child2.html' />
                                       <div id='content'></div>
                                     </body>
                                  </html>"
                                 relative-path-to-template-loc relative-path-to-template-loc)
          child-within-content (format "<_within file='%s/parent.html'>
                                           <div id='content'>I am child content 1</div>
                                        </within>"
                                       relative-path-to-template-loc)
          child-inner-content "<div>child 2 content</div>"]
      (with-html-files [parent "parent.html" parent-content
                        child-within "child.html" child-within-content
                        child-include "child2.html" child-inner-content]
        (is (html-string-equality (tmp/load-html child-within)
                                  "<html>
                       <body>
                           <div>child 2 content</div>
                           <div id=\"content\">I am child content 1</div>
                       </body>
                  </html>")))))

  (testing
      "Load a child file that has a within tag, and an include tag.  It will be loaded into the parent template, but it will
       also load the included child content"
    (let [parent-content (format "<html>
                                       <body>
                                         <div id='content'></div>
                                       </body>
                                    </html>"
                                 relative-path-to-template-loc relative-path-to-template-loc)
          child-within-content (format "<_within file='%s/parent.html'>
                                             <div id='content'>
                                                 I am child content 1
                                                 <_include file='%s/child2.html' />
                                              </div>
                                           </within>" relative-path-to-template-loc relative-path-to-template-loc)
          child-inner-content "<div>child 2 content</div>"]
      (with-html-files [parent "parent.html" parent-content
                        child-within "child.html" child-within-content
                        child-include "child2.html" child-inner-content]
        (is (html-string-equality (tmp/load-html child-within)
                                  "<html>
                       <body>
                           <div id=\"content\">
                             I am child content 1
                             <div>child 2 content</div>
                            </div>
                        </body>
                   </html>"))))))



(deftest test-tnodes
  (testing
      "Test that tnodes can find the matching template tags with the given template attribute name"
    (with-html-files [x "x.html" "<div><div template='t1'></div></div>"]      
      (is (= (tmp/tnodes x "t1")
             '({:tag :div, :attrs {:template "t1"}, :content nil}))))))

(deftest test-template-children
  (testing
      "Make sure that all the children of the template content are being found"
    (with-html-files [x "x.html" "<div><div template='t1'><div id='child'><ul><li>Bullet Point</li></ul></div></div></div>"]
      (is (= (tmp/template-children x "t1")
             '({:tag :div, :attrs {:id "child"},
                :content ({:tag :ul, :attrs nil,
                           :content ({:tag :li, :attrs nil,
                                      :content ("Bullet Point")})})})))))

  (testing
      "Load multiple children of the same template name"
    (with-html-files [x "x.html" "<div>
                                       <div template='t1'>
                                            <div id='child1'>child 1 content</div>
                                       </div>
                                       <div template='t1'>
                                            <div id='child2'>child 2 content</div>
                                       </div>
                                      </div>"]
      (is (= (tmp/template-children x "t1")
             '({:tag :div, :attrs {:id "child1"},:content ("child 1 content")}
               {:tag :div, :attrs {:id "child2"}, :content ("child 2 content")}))))))



(deftest test-field-pairs
  (testing "Should be able to extract a string field pair and convert it into a sequence of two elements
            where the first element is the field type, and the second element is the field identifier"
    (is (= (@#'tmp/field-pairs "id:id-5")
           '(("id" "id-5")))))

  (testing "Should be able to break up multiple field pairs that are delimited by commas"
    (is (= (@#'tmp/field-pairs "id:id-5,content:name")
           '(("id" "id-5") ("content" "name"))))))

(deftest test-extract-field-pairs
  (testing "Make sure that all the nodes containing template field pairs can be selected, and all these 
            pairs extracted"
    (let [nodes (tmp/html-parse "<div field='id:id-5'>
                                   <div field='content:name'></div>
                                  </div>")]     
      (is (= (@#'tmp/extract-field-pairs nodes)
             '("id:id-5" "content:name"))))))

(deftest test-field-to-symbol-mapping
  (testing "Make sure that all fields are mapped to a unique symbol"
    (let [nodes (tmp/html-parse "<div field='id:id-5'>
                                   <div field='content:name'></div>
                                  </div>")
          field-names (@#'tmp/extract-field-pairs nodes)]
      (testing "All the values should be symbols"
        (is (every? #(symbol? %) (vals (@#'tmp/field-to-symbol-mapping field-names))) ))      
      (testing "Two symbols should have been created"
        (is (= 2 (count (vals (@#'tmp/field-to-symbol-mapping field-names)))))))
    )

  (testing "Fields that specify the same field identifier, should map to the same symbol"
    (let [nodes (tmp/html-parse "<div field='id:id-5'>
                                   <div field='content:name'></div>
                                 </div>
                                 <div field='id:id-5'></div> ")
          field-names (@#'tmp/extract-field-pairs nodes)]
      (testing "There were three field attributes used"
        (is (= 3 (count field-names)))
        )
      (testing "However, only two symbols should have been created, because two are the same"
        (is (= 2 (count (vals (@#'tmp/field-to-symbol-mapping field-names)))))))))

(deftest test-convert-field-pair-to-map
  (testing
      "Test that a raw field pair string is being converted into a map.  The key is the
       field pair identifier, and the value is a map itself, which has three attributes,
       :field, :type and :attr-name"
    (let [fp-map (@#'tmp/convert-field-pair-to-map "content:name")]
      (testing "Make sure the field pair identifier is a keyword, and a key in the new map"
        (is (contains? fp-map :name)))
      (testing "The attr-name within the map should be equal to content"
        (is (= (get-in fp-map [:name :attr-name]) "content")))
      (testing "The type should be :content"
        (is (= (get-in fp-map [:name :type]) :content)))
      (testing "The field should be equal to the original field pair string"
        (is (= (get-in fp-map [:name :field]) "content:name")))
      (is (= {:name {:field "content:name" :type :content, :attr-name "content"}}))))
  (testing "Test that a field pair with a type that isn't content, should result in a map with a type of :attr"
    (is (= {:name {:field "x:name" :type :attr, :attr-name "x"}}
           (@#'tmp/convert-field-pair-to-map "x:name")))
    (is (= {:name {:field "id:name" :type :attr, :attr-name "id"}}
           (@#'tmp/convert-field-pair-to-map "id:name")))))

(deftest test-convert-field-pairs-to-map
  (testing
      "A sequence of field pairs should be able to be converted into a map, where the key is the field identifier
       and the values are a map determined by the field type, with the keys :field, :type and :attr-name"    
    (is (= (@#'tmp/convert-field-pairs-to-map '("id:id-5", "content:name"))
           {:name {:field "content:name" :type :content, :attr-name "content"}
            :id-5 {:field "id:id-5" :type :attr, :attr-name "id"}}))))

(deftest test-create-template-map
  "Given a sequence of field pairs, and a map containing a mapping of a field pair to a symbol name,
   should create a map that maps the field identifiers from the field pairs to a field template map.
   The field template map has four attributes, :field, :id, :attr-name and :type.
   :field is the raw field pair string
   :id is the symbol that the field pair mapped to in ts-syms
   :attr-name is the name of the field type identifier
   :type is either :attr, or :content"
  (let [ts ["id:id-5" "content:name"]
        ts-syms {"id:id-5" 'G_3001, "content:name" 'G_3002}]
    (is (= (@#'tmp/create-template-map ts ts-syms)
           {:id-5 {:field "id:id-5" :id 'G_3001 :attr-name "id" :type :attr}
            :name {:field "content:name" :id 'G_3002 :attr-name "content" :type :content}}
           ))))

(deftest test-remove-static-fields
  (testing
      (let [removed (@#'tmp/remove-static-fields #{:id-5}
                                           '{:id-5 {:field "id:id-5" :id 'G_3001 :attr-name "id" :type :attr}
                                             :name {:field "content:name" :id 'G_3002 :attr-name "content" :type :content}})]
        (testing "There should only be 1 item in the map now, since the value with :id-5 is now gone"
            (is (= (count removed)
                   1)))
        (testing "Only :name should remain as a key"
          (is (contains? removed :name))
          (is (not (contains? removed :id-5))))))
  (testing "A key that is not in the map will not effect the resulting map"
    (is (= (count (@#'tmp/remove-static-fields #{:id-6}
                                           '{:id-5 {:field "id:id-5" :id 'G_3001 :attr-name "id" :type :attr}
                                             :name {:field "content:name" :id 'G_3002 :attr-name "content" :type :content}}))
           2))))

(deftest test-make-dynamic-template
  (testing
      "The sequence of enlive nodes contains fields that reference a field pair.  Using the
       key :id-5, which is a field identifier, insert the string id:G3001 into the
       the attribute of the node that has the field id:id-5"
   (is (= (@#'tmp/make-dynamic-template '({:tag :div :attrs {:field "id:id-5"}})
                                        :id-5
                                        {:field "id:id-5" :id 'G_3001 :attr-name "id" :type :attr})
          '({:content (), :tag :div, :attrs {:field "id:id-5,id:G_3001"}})))))


(deftest test-insert-template-symbols-into-nodes
  (with-html-files [x "x.html" "<div template=\"t1\"><div field=\"id:id-5\"></div><div field=\"content:name\"></div></div>"]
    (let [ts ["id:id-5" "content:name"]
          ts-syms {"id:id-5" 'G_3001, "content:name" 'G_3002}
          t-map (@#'tmp/create-template-map ts ts-syms)
          nodes (tmp/tnodes x "t1")]
      (is (= (@#'tmp/insert-template-symbols-into-nodes t-map nodes)
             '({:tag :div, :attrs {:template "t1"},
                :content ({:tag :div, :attrs {:field "id:id-5,id:G_3001"}, :content ()} 
                          {:tag :div, :attrs {:field "content:name,id:G_3002"}, :content ()})}))))))

(deftest test-removed-unneeded-template-fields
  (testing
      "When given a sequence of template nodes and a template map, need to remove all field attributes from 
       the template map values"
    (is (= (@#'tmp/remove-unneeded-template-fields
          {:id-5 {:field "id:id-5" :id 'G_3001 :attr-name "id" :type :attr}
           :name {:field "content:name" :id 'G_3002 :attr-name "content" :type :content}})
          '{:id-5 {:attr-name "id", :type :attr, :id G_3001}, :name {:type :content, :id G_3002}}))))

(deftest test-template-output
  (testing
      "When this list statement is evaluated, it will create a let expression.  In the bindings
       is the usual vector binding, which maps ids to new symbols.  The let statement, and therefore,
       the symbols will not be evaluated right away.  It will just be the raw list form that is created
       the first type this expression is read"
    (let [ids ['G_33 'G_44]]
      (is (= (list 'let (vec (interleave ids (repeat (list 'gensym)))))
             '(let [G_33 (gensym) G_44 (gensym)]))))
    (let [t-map {:name {:id 'G_77 :type :content}}
          ids ['G_77]
          x (list 'let (vec (interleave ids (repeat (list 'gensym))))
                  [t-map])]
      (testing "Since the inside of the let expression has not been evaluated, the symbol referred
                  to by the t-map will still be G_77"
        (is (= x
               '(let [G_77 (gensym)] [{:name {:type :content, :id G_77}}]))))

      (testing "However, when evaluated, it will have a new symbol, because G_77
                  now points to a different symbol, generated when gensym was evaluated
                  in the let statement"
        (let [[new-t-map] (eval x)]
          (is (not= new-t-map t-map))
          (is (not= (get-in new-t-map [:name :id]) (get-in t-map [:name :id])))
          (is (symbol? (get-in new-t-map [:name :id]))))))))


(deftest test-field-map
  (testing " Set up the nodes that have field attrs and content in them"
   (let [nodes '({:tag :div, :attrs {:template "t1"},
                  :content ({:tag :div, :attrs {:field "id:id-5"},
                             :content ()} {:tag :div, :attrs {:field "content:name,id:G__85186"},
                             :content ()})})
         ts (@#'tmp/extract-field-pairs nodes)]
     (testing "Make sure that the field pairs are extracted, there should be two of them"
       (is (= (count ts) 2))
       (is (= '("id:id-5" "content:name,id:G__85186")
              ts)))    
     (testing "The field map should extract all the separate field pairs from the sequence
              In this case, there are actually 3 field pairs, because content:name,id:G__85186
              is actually 2 field pairs.  There is also id:id-5"
       (let [field-map (@#'tmp/field-map ts)]
         (is (= (count field-map) 3) )
         (is (= field-map
                {"G__85186" "id" "name" "content" "id-5" "id"}
                )))))))

(deftest test-field-map-to-index
  (testing ""
   (let [map-sym 'G_10001
         field-map {"G__85186" "id" "name" "content" "id-5" "id"}]
     (is (= (@#'tmp/field-map-to-index map-sym field-map)
            '{"~{name}" (get G_10001 :name)
              "~{G__85186}" (get G_10001 :G__85186)
              "~{id-5}" (get G_10001 :id-5)})))))


(deftest test-make-template
  (testing "A field that specifies an attribute value, such as an id, should have its node sequence
            altered so that the field identifier in the field pair shows up in the attrs part of
            the node"
    (is (= (@#'tmp/make-template '({:tag :div, :attrs {:template "t1"},
                                    :content ({:tag :div, :attrs {:field "id:id-5"}, :content ()})})
                                 "id:id-5"
                                 )
           ;;  id-5 now appears in the div attrs, and it's an id attribute
           '({:tag :div, :attrs {:template "t1"},
              :content ({:tag :div, :attrs {:id "~{id-5}", :field "id:id-5"}, :content ()})})
           )))  
  (testing "Content should show up in the content section of the nodes when specified by a field"
    (is (= (@#'tmp/make-template '({:tag :div, :attrs {:template "t1"},
                                    :content ({:tag :div, :attrs {:field "content:name"}, :content ("~{name}")})})
                                 "content:name"
                                 )
           '({:tag :div, :attrs {:template "t1"},
              :content ({:tag :div, :attrs {:field "content:name"},:content ("~{name}")})})
           )))
  (testing "Multiple field pairs in the same field attribute should be expanded in the nodes
            if they exist.  Here there is both an attribute, and a content field pair."
    (is (= (@#'tmp/make-template '({:tag :div, :attrs {:template "t1"},
                                    :content ({:tag :div, :attrs {:field "content:name,id:G__85186"},
                                               :content ()})})
                                 "content:name,id:G__85186"
                                 )
           '({:tag :div, :attrs {:template "t1"},
              :content ({:tag :div, :attrs {:id "~{G__85186}", :field "content:name,id:G__85186"},
                         :content ("~{name}")})})))))



(deftest test-prepare-node-template
  (testing "Make sure that all nodes that specify a field pair, and that are in the ts sequence
            are prepared for template value insertion   This is done by having their content/attribute section
            appended with a field identifier wrapped in ~{}."    
    (is (= (@#'tmp/prepare-node-template '({:tag :div, :attrs {:template "t1"},
                                            :content ({:tag :div, :attrs {:field "id:id-5"}, :content ()}
                                                      {:tag :div, :attrs {:field "content:name,id:G__85186"},
                                                       :content ()})})
                                         '("id:id-5" "content:name,id:G__85186"))

           '({:tag :div, :attrs {:template "t1"},
              :content ({:tag :div, :attrs {:id "~{id-5}", :field "id:id-5"}, :content ()}
                        {:tag :div, :attrs {:id "~{G__85186}", :field "content:name,id:G__85186"}, :content ("~{name}")})})))))

(deftest test-combine-field-map-index-with-nodes
  (testing
      "Given some nodes and a field map index, need to combine them so that the field and template
       attributes are removed, and what they specify for their values are inserted into the nodes
       In this case, the :id and content areas have template identifiers wrapped in ~{} expressions.
       These will be replaced by a get expression value in the field map index."
    (let [nodes '({:tag :div, :attrs {:template "t1"},
                   :content ({:tag :div, :attrs {:id "~{id-5}", :field "id:id-5"}, :content ()}
                             {:tag :div, :attrs {:id "~{G__85186}", :field "content:name,id:G__85186"}, :content ("~{name}")})})
          field-map-index {"~{name}" '(get G_10001 :name)
                           "~{G__85186}" '(get G_10001 :G__85186)
                           "~{id-5}" '(get G_10001 :id-5)}]        
      (is (=  (@#'tmp/combine-field-map-index-with-nodes field-map-index nodes)
              '("<div><div id=\"" (get G_10001 :id-5) "\"></div><div id=\"" (get G_10001 :G__85186) "\">" (get G_10001 :name) "</div></div>"))))))


(deftest test-convert-nodes-to-template-seq
  (testing
      "When given a sequence of nodes that contain field pairs, and that have had a template
       map symbol inserted into them, these can be converted into a enlive string sequence,
       which will ultimately allow a string of html to be returned. "
    (let [nodes '({:tag :div, :attrs {:template "t1"},
                   :content ({:tag :div, :attrs {:field "id:id-5,id:G_3001"}, :content ()} 
                             {:tag :div, :attrs {:field "content:name,id:G_3002"}, :content ()})})]
      (testing
          "Should contain get calls where the original field pairs were."
       (is (= (#'tmp/convert-nodes-to-template-seq nodes 'G_xx)
              '("<div><div id=\"" (get G_xx :G_3001) "\"></div><div id=\"" (get G_xx :G_3002) "\">"
                (get G_xx :name) "</div></div>")))))))

(deftest test-tfn
  (testing
      "When given a sequence of nodes that contain field pairs and template symbols, need to create a function
       that takes one parameter.  That parameter will be a map.  The map is used to populate the content
       of the html string that is generated by the function.  In this case, the map symbol needs to be
       explictly passed into tfn, so that what comes out will be a literal function that can be evaluated later."
    (let [nodes '({:tag :div, :attrs {:template "t1"},
                   :content ({:tag :div, :attrs {:field "id:id-5,id:G_3001"}, :content ()} 
                             {:tag :div, :attrs {:field "content:name,id:G_3002"}, :content ()})})
          result '(fn [G_101] (str "<div><div id=\"" (get G_101 :G_3001) "\"></div><div id=\"" (get G_101 :G_3002) "\">" 
                                   (get G_101 :name) "</div></div>"))
          ]
      (testing
          "Make sure that tfn is generating a function that takes one parameter, and generates a string that
           will output the html required for the template"
          (is (= (@#'tmp/tfn nodes 'G_101)
                 result)))

      (testing
          "Make sure that tfn is generating a function that takes one parameter, and generates a string that
           will output the html required for the template"
        (let [f (eval result)]
          (is (=  (f {:name "The name content" :G_3001 "id-five" :G_3002 "name-id"})
                  "<div><div id=\"id-five\"></div><div id=\"name-id\">The name content</div></div>")
               ))))))

(deftest test-template-output
  (testing
      "When given a template map and a sequence of nodes, and two map symbols, generate a literal function that will be used to
       generate a template string"
      )
  (let [t-map '{:id-5 {:attr-name "id", :type :attr, :id G_3001}, :name {:type :content, :id G_3002}}
        nodes '({:tag :div, :attrs {:template "t1"},
                 :content ({:tag :div, :attrs {:field "id:id-5,id:G_3001"}, :content ()} 
                           {:tag :div, :attrs {:field "content:name,id:G_3002"}, :content ()})})]
    (testing
        "A literal function will be generated, it contains a let statement.  This will return a vector of
         two elements.  The first element is the original template map.  The second element is a function that
         returns a function."
     (is (= (#'tmp/template-output t-map nodes 'outer-map 'inner-map)
            '(fn []
               (let [G_3001 (gensym) G_3002 (gensym)]
                 [{:id-5 {:attr-name "id", :type :attr, :id G_3001}, :name {:type :content, :id G_3002}}
                  (fn [outer-map]
                    ((fn [inner-map]
                       (str "<div><div id=\"" (get inner-map :G_3001) "\"></div><div id=\"" (get inner-map :G_3002) "\">"
                            (get inner-map :name) "</div></div>"))
                     (assoc outer-map :G_3001 G_3001 :G_3002 G_3002)))]))
            )))
    (let [f (eval (#'tmp/template-output t-map nodes 'outer-map 'inner-map))]
      (testing
          "When evaluated, it returns a function"
        (is (function? f)))
      (let [[template html] (f)]
        (testing
            "When the function is ran, it returns a vector of two elements, a template map, and an html template function"
          (is (map? template))
          (is (function? html)))))))

(deftest test-dtfn
  (with-html-files [x "x.html" "<div template=\"t1\"><div field=\"id:id-5\"></div><div field=\"content:name\"></div></div>"]
    (let [mk-tmp (tmp/dtfn (tmp/tnodes x "t1") #{:id-5})
          mk-tmp (eval mk-tmp)
          [template html] (mk-tmp)]
      (testing "The id that was specified to belong to id-5 should now contain the id id5"
       (is (-> (enlive/select (tmp/html-parse (html {:id-5 "id5"}))
                              [:#id5])
               first
               :attrs
               :id              
               )
           "id5"))      
      (testing "There should be one element with the content 'The new content'"
        (is (= 1
             (count (enlive/select (tmp/html-parse (html {:name "The new content"}))
                                   [(enlive/text-pred #(re-matches #"The new content" %))]))))))))

