(ns io.pedestal.app.test.templates
  (:require [io.pedestal.app.templates :as tmp]
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
                                (spit absf# ~(bindings 2))
                                (try
                                  (with-html-files ~(subvec bindings 3)                                    
                                    ~@body)
                                  (finally
                                    (io/delete-file absf#)))))
    :else (throw (IllegalArgumentException.
                   "with-open only allows Symbols in bindings"))))

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

(deftest test-html-body
  (testing "Should extract the body content from an html file"
    (with-html-files [h "hello.html" "<html><body><div>The div content</div></body></html>"]
      (is (= (@#'tmp/html-body h)
             '({:tag :div :attrs nil :content ("The div content")})))))
  (testing "When there is no body or html in the file, it will just return the nodes if it's valid html"
    (with-html-files [h "hello.html" "<div>The div content</div>"]
      (is (= (@#'tmp/html-body h)
             '({:tag :div :attrs nil :content ("The div content")}))))))



"<div>I am from the child</div>"
"<html><body><div>I am from the child</div></body></html>"

(deftest test-load-html
  (testing "Should load a plain bit of html without any includes"
    (with-html-files [h "hello.html" "<html><body><div>The div content</div></body></html>"]
      (is (= (tmp/load-html h)
             "<html><body><div>The div content</div></body></html>"))))
  (testing "Should be able to load a single included file.  The parent loads the child content"    
    (let [parent-content (format "<html><body><_include file='%s/child.html' /></body></html>" relative-path-to-template-loc)
          child-content "<div>I am from the child"]
        (with-html-files [parent "parent.html" parent-content child "child.html" child-content ]
          (is (= (tmp/load-html parent)
                 "<html><body><div>I am from the child</div></body></html>")))))  
  (testing
      "Should be able to load a child file that has a within tag, referencing the parent.  The child will be expanded
       into the parent's content"
    (let [parent-content (format "<html><body><div id='content'></div></body></html>" relative-path-to-template-loc)
          child-content (format "<_within file='%s/parent.html'><div id='content'>I am child content</div></within>"
                                relative-path-to-template-loc)]
        (with-html-files [parent "parent.html" parent-content child "child.html" child-content ]
          (is (= (tmp/load-html child)
                 "<html><body><div id=\"content\">I am child content</div></body></html>")))))

  (testing
      "Load a child file, which has a within tag.  It references a parent.  The parent  has a reference to an include tag.
       The include tag is a reference to a second child."
    (let [parent-content (format "<html><body><_include file='%s/child2.html' /><div id='content'></div></body></html>"
                                 relative-path-to-template-loc relative-path-to-template-loc)
          child-within-content (format "<_within file='%s/parent.html'><div id='content'>I am child content 1</div></within>"
                                relative-path-to-template-loc)
          child-inner-content "<div>child 2 content"]
        (with-html-files [parent "parent.html" parent-content
                          child-within "child.html" child-within-content
                          child-include "child2.html" child-inner-content]
          (is (= (tmp/load-html child-within)
                 "<html><body><div>child 2 content</div><div id=\"content\">I am child content 1</div></body></html>")))))

  (testing
      "Load a child file that has a within tag, and an include tag.  It will be loaded into the parent template, but it will
       also load the included child content"
      (let [parent-content (format "<html><body><div id='content'></div></body></html>"
                                   relative-path-to-template-loc relative-path-to-template-loc)
            child-within-content (format "<_within file='%s/parent.html'><div id='content'>I am child content 1<_include file='%s/child2.html' /></div></within>" relative-path-to-template-loc relative-path-to-template-loc)
            child-inner-content "<div>child 2 content"]
        (with-html-files [parent "parent.html" parent-content
                          child-within "child.html" child-within-content
                          child-include "child2.html" child-inner-content]
          (is (= (tmp/load-html child-within)
                 "<html><body><div id=\"content\">I am child content 1<div>child 2 content</div></div></body></html>")))))
  
  )



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
