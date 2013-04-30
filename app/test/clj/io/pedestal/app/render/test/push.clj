; Copyright 2013 Relevance, Inc.

; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0)
; which can be found in the file epl-v10.html at the root of this distribution.
;
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
;
; You must not remove this notice, or any other, from this software.

(ns io.pedestal.app.render.test.push
  "Test rendering code with an artificial DOM."
  (:require [io.pedestal.app.render.test.dom :as d]
            [io.pedestal.app.protocols :as p]
            [io.pedestal.app.messages :as msg])
  (:use io.pedestal.app.render.push
        clojure.test))

;; Handlers
;; ================================================================================

(deftest test-add-handler
  (is (= (add-handler {} :* [:a :b :c] :x)
         {:* {:children
              {:a {:children
                   {:b {:children
                        {:c {:handler :x}}}}}}}}))
  (is (= (-> {}
             (add-handler :node-create [] :a)
             (add-handler :node-create [:**] :b))
         {:node-create {:handler :a
                       :children {:** {:handler :b}}}})))

(deftest test-add-handlers
  (is (= (add-handlers [[:* [:a :b :c] :x]])
         {:* {:children
              {:a {:children
                   {:b {:children
                        {:c {:handler :x}}}}}}}}))
  (is (= (add-handlers [[:node-create [] :a]
                        [:node-create [:**] :b]])
         {:node-create {:handler :a
                       :children {:** {:handler :b}}}})))

(deftest test-find-handler
  (let [handlers
        (-> {}
            (add-handler :node-create      []                :_node-enter-root)
            (add-handler :node-create      [:**]             :_node-enter-any)
            (add-handler :node-create      [:a :b :*]        :_node-enter-a-b-any)
            (add-handler :transform-enable [:a :b :*]        :_transform-enter-a-b-any)
            (add-handler :transform-*          [:a :b :*]    :_transform-any-a-b-any)
            (add-handler :transform-enable [:a :b :* :d :e]  :_transform-enter-a-b-any-d-e)
            (add-handler :transform-enable [:* :* :* :d :*]  :_transform-enter-any-any-any-d-any)
            (add-handler :transform-enable [:* :* :* :f :**] :_transform-enter-any-any-any-f-any)
            (add-handler :transform-enable [:a :b :* :d :e]  :_transform-enter-a-b-any-d-e))]
    (is (= (find-handler handlers :node-create [])
           :_node-enter-root))
    (is (= (find-handler handlers :node-create [:a :b :c :d])
           :_node-enter-any))
    (is (= (find-handler handlers :transform-enable [:a :b :c])
           :_transform-enter-a-b-any))
    (is (= (find-handler handlers :transform-enable [:a :b :g])
           :_transform-enter-a-b-any))
    (is (= (find-handler handlers :transform-disable [:a :b :g])
           :_transform-any-a-b-any))
    (is (= (find-handler handlers :transform-enable [:a :b :c :d :e])
           :_transform-enter-a-b-any-d-e))
    (is (= (find-handler handlers :transform-enable [:a :b :c :d :e])
           :_transform-enter-a-b-any-d-e))
    (is (= (find-handler handlers :transform-enable [:z :b :c :d :e])
           :_transform-enter-any-any-any-d-any))
    (is (= (find-handler handlers :transform-enable [:a :b :c :f :e])
           :_transform-enter-any-any-any-f-any))
    (is (= (find-handler handlers :node-create [:a :b :c])
           :_node-enter-a-b-any))))

;; Rendering
;; ================================================================================

(deftest test-render-add
  ;; d/*dom* is dynamically bound, and represents the dom that the functions will
  ;; work on.  If the functions change *dom*, then it will remain changed
  (binding [d/*dom* (d/test-dom)]    
    (let [;; create handlers
          ;; Handlers take three arguments
          ;; The first is a DOM renderer object.  
          ;; The second argument is the delta value that it is designed to work with
          ;; The third argument is the input queue

          ;; This will handle :node-create deltas
          ;; It uses set-attrs! to alter the *dom* and sets its class attribute
          ;; to active
          r-enter (fn [_ _ _] (d/set-attrs! :root {:class "active"}))
          ;; This will handle :value deltas.  It will create a new element
          ;; as a child of :root, with a value of v, and with an id attribute
          ;; of :a
          a-enter (fn [_ [o p _ v] _] (d/append! :root {:content v :attrs {:id :a}}))
          ;; create listeners
          ls (-> {}
                 (add-handler :node-create [:a] r-enter)
                 (add-handler :value [:a] a-enter))
          ;; create renderer
          r (renderer :root ls)]

      (testing
          "Make sure that the renderer returned a function"
       (is (function? r)))

      ;; Run the render function with two deltas, a node creation, and a value change
      (r [[:node-create [:a] :map]
          [:value [:a] nil 1]]
         nil)
     
      ;; The r-enter should alter the root id to be active when the :node-create delta occurred
      ;; The a-enter should have changed the child node with the id :a to be 1
      (is (= (:root @d/*dom*)
             {:content nil
              :attrs {:id :root :class "active"}
              :children [{:content 1
                          :attrs {:id :a}}]})))))

(deftest test-working-with-ids
  ;; Set the root id to be root
  (let [r (->DomRenderer (atom {:id :root}))]
    (is (nil? (get-parent-id r [])))
    (is (= :root (get-parent-id r [:a])))
    ;; new-id! creates a new id if a third parameter is not specified
    ;; It will return the new id.
    ;; This creates a new root id
    (let [root-id (new-id! r [])]      
      (is (= root-id (get-parent-id r [:a])))
      ;; Create a new id for the element at :a
      (let [a-id (new-id! r [:a])
            ;; Assert that :b is the id :b
            b-id (new-id! r [:b] :b)
            ;; Create a new id for :c, which is a child of :b
            c-id (new-id! r [:b :c])]
        (is (= :b b-id))
        (is (= a-id (get-id r [:a])))
        (is (= :b (get-id r [:b])))
        (is (= c-id (get-id r [:b :c])))
        (is (= :b (get-id r [:b]) (get-parent-id r [:b :c])))
        ;; Remove the id from :a
        (delete-id! r [:a])

        (is (nil? (get-id r [:a])))
        (is (= c-id (get-id r [:b :c])))
        ;; Remove the id from :b
        (delete-id! r [:b])
        (is (nil? (get-id r [:a])))
        (is (nil? (get-id r [:b :c])))))))

(deftest test-render-build-up-tear-down

  (binding [d/*dom* (d/test-dom)]
    
    (let [;; create handlers
          ;; A handler for :node create [:a]
          a-enter (fn [r [_ path] _]
                    ;; Get the parent id so that append can insert
                    ;; the child into it                    
                    (let [parent (get-parent-id r path)
                          ;; Create a new id for the node
                          id (new-id! r path :a)]
                      ;; Insert the new node into the parent
                      ;; Set the child's id in the dom
                      (d/append! parent {:content nil :attrs {:id id}})))
          ;; A handler for :node-create [:a :b]
          b-enter (fn [r [_ path] _]                    
                    (let [parent (get-parent-id r path)
                          id (new-id! r path :b)
                          list-id (new-id! r (conj path :c) :attr-list)]
                      ;; Append the :b content to the parent
                      (d/append! parent {:content "Attributes List"
                                         :attrs {:id id}})
                      ;; Append the :c content to :b
                      (d/append! id {:content nil
                                     :attrs {:id list-id :class "list"}
                                     :children []})))
          ;; Handles :value [:a :b :c]
          c-update (fn [r [_ path _ v] _]
                     ;; Get the id to the path, which will be [:a :b :c]
                     ;; in this case
                     (let [id (get-id r path)]
                       ;; Destroy the old content there
                       (d/destroy-children! id)
                       ;; Sort new value, and append the content
                       ;; one by one
                       (doseq [x (sort v)]
                         (d/append! id {:content x}))))
          ;; Handles :node-destroy [:a :b :c]
          c-exit (fn [r [_ path] _]
                   (d/destroy-children! (get-id r path)))
          ;; create listeners
          ls (-> {}
                 (add-handler :node-create [:a] a-enter)
                 (add-handler :node-create [:a :b] b-enter)
                 (add-handler :value [:a :b :c] c-update)
                 (add-handler :node-destroy [:a :b :c] c-exit)
                 (add-handler :node-destroy [:**] d/default-exit))
          ;; create renderer
          r (renderer :root ls)]

      ;; Create the node that :a
      (r [[:node-create [:a] :map]] nil)
      ;; The node is created with an id of :a
      (is (= (:root @d/*dom*)
             {:content nil
              :attrs {:id :root}
              :children [{:content nil :attrs {:id :a}}]}))

      ;; Create the :b node, which is a child of :a
      (r [[:node-create [:a :b] :map]] nil)

      (is (= (:root @d/*dom*)
             {:content nil
              :attrs {:id :root}
              :children
              [{:content nil
                :attrs {:id :a}
                :children
                [{:content "Attributes List"
                  :attrs {:id :b}
                  :children
                  [{:content nil
                    :attrs {:id :attr-list :class "list"}
                    :children []}]}]}]}))
      
      ;; Create the list node, and fill it with new values
      (r [[:node-create [:a :b :c] :map]
          [:value [:a :b :c] nil [:x :y]]]
         nil)

      ;; The list node is now populated with those values, sorted order
      (is (= (:root @d/*dom*)
             {:content nil
              :attrs {:id :root}
              :children
              [{:content nil
                :attrs {:id :a}
                :children
                [{:content "Attributes List"
                  :attrs {:id :b}
                  :children
                  [{:content nil
                    :attrs {:id :attr-list :class "list"}
                    :children [{:content :x}
                               {:content :y}]}]}]}]}))
      ;; Insert new list values
      (r [[:value [:a :b :c] nil [:z :x :y]]]
         nil)

      ;; The items were inserted in sorted order in the node
      (is (= (:root @d/*dom*)
             {:content nil
              :attrs {:id :root}
              :children
              [{:content nil
                :attrs {:id :a}
                :children
                [{:content "Attributes List"
                  :attrs {:id :b}
                  :children
                  [{:content nil
                    :attrs {:id :attr-list :class "list"}
                    :children [{:content :x}
                               {:content :y}
                               {:content :z}]}]}]}]}))

      ;; Destroy the list content
      (r [[:node-destroy [:a :b :c]]] nil)
      
      (is (= (:root @d/*dom*)
             {:content nil
              :attrs {:id :root}
              :children
              [{:content nil
                :attrs {:id :a}
                :children
                [{:content "Attributes List"
                  :attrs {:id :b}
                  :children
                  [{:content nil
                    :attrs {:id :attr-list :class "list"}
                    :children []}]}]}]}))

      ;; Destroy the :b node
      (r [[:node-destroy [:a :b]]] nil)
      
      (is (= (:root @d/*dom*)
             {:content nil
              :attrs {:id :root}
              :children [{:content nil
                          :attrs {:id :a}
                          :children []}]}))

      ;; Destroy the :a node
      (r [[:node-destroy [:a]]] nil)
      
      (is (= (:root @d/*dom*)
             {:content nil
              :attrs {:id :root}
              :children []})))))

;; A test queue that will act as an input queue to the app
(defrecord TestQueue [action]
  p/PutMessage
  (put-message [this message]
    ;; Reset the queue so that only this message is on it
    (reset! action message)))

(defn fill-messages [messages transform-name env]
  (assert (contains? messages transform-name) (str "There is no transform named " transform-name))
  (map (partial msg/add-message-type transform-name) (transform-name messages)))

(deftest test-render-timeline
  
  (binding [d/*dom* (d/test-dom)]
    
    (let [ ;; create handlers
          ;; Works with :node-create [:t]
          ;; Create a new timeline
          t-enter (fn [r [_ path] _]
                    (let [parent (get-parent-id r path)
                          id (new-id! r path :timeline)]
                      (d/append! parent {:content nil :attrs {:id id}})))

          ;; Handles :node-create [:t :chart]
          ;; Creates the timeline chart in the timeline
          chart-enter (fn [r [_ path] d]
                        (let [parent (get-parent-id r path)
                              id (new-id! r path :chart)]
                          (d/append! parent {:content "Timeline Chart" :attrs {:id id}})))

          ;; Handles :transform-enable [:t :chart]
          ;; The key it is listening for is :group-selected
          ;; The message will be [{msg/topic :timeline (msg/param :group-id) {}}]
          ;; When The chart is clicked on, sends a message that a certain group
          ;; of items were selected
          chart-transform-enable (fn [r [_ path transform-name msgs] d]
                              (let [id (get-id r path)]
                                ;; Listen for chart clicks
                                (d/listen! id
                                           :click
                                           (fn [e]
                                             ;; Put a new message on the test queue
                                             (p/put-message d (fill-messages {transform-name msgs}
                                                                              :group-selected
                                                                              {}))))
                                ;; When the chart element is destroyed, stop listening for it
                                (on-destroy! r path #(d/unlisten! id :click))))
          ;; Handles :node-create [:t :chart :data]
          ;; Creates new chart data nodes
          data-enter (fn [r [_ path] _]
                       (let [parent (get-parent-id r path)
                             id (new-id! r path :chart-content)]
                         (d/append! parent {:content nil :attrs {:id id}})))
          
          ;; Handles :node-create [:t :chart :data :*]
          ;; Creates the new content in the chart data
          add-chart-data-node (fn [r [_ path] _]
                                (let [parent (get-parent-id r path)
                                      id (new-id! r path (keyword (str "chart-data-" (last path))))]
                                  (d/append! parent {:content nil :attrs {:id id}})))

          ;; Handles :value [:t :chart :data :*]
          ;; Handles updating chart data values
          data-update (fn [r [_ path _ v] _]
                        (let [parent (get-parent-id r path)
                              id (d/nth-child-id parent (last path))]
                          (d/set-content! id v)))

          ;; Handles everything related to the path [:t :chart :back-button]
          ;; In this example, the key is :nav, and the messages are [{:page :attributes}]
          bb-enter (fn [r delta d]
                     (let [[op path] delta
                           parent (get-parent-id r path)
                           id (get-id r (conj path :back-button))
                           id (or id (new-id! r path :back-button))]
                       (condp = op
                         ;; When the operation is :node-create
                         ;; Create the back button
                         :node-create (d/append! parent {:attrs {:id id :class :button}})
                         ;; When the operation is :value,
                         ;; Set the button's text to be equal to the value
                         :value (d/set-content! id (last delta))
                         ;; When the operation is :transform-enable
                         ;; Listen for clicks on the back button
                         :transform-enable
                         (let [[_ _ transform-name msgs] delta]
                           (d/listen! id
                                      :click
                                      (fn [e]
                                        ;; Put a new message on the test queue about navigation
                                        (p/put-message d (fill-messages {transform-name msgs} :nav {}))))
                           ;; When the back button is destroyed, stop listening for its clicks
                           (on-destroy! r path #(d/unlisten! id :click))))))
          
          ;; create listeners
          ls (-> {}
                 (add-handler :node-create [:t] t-enter)
                 (add-handler :node-create [:t :chart] chart-enter)
                 (add-handler :node-create [:t :chart :data] data-enter)
                 (add-handler :node-create [:t :chart :data :*] add-chart-data-node)
                 (add-handler :value [:t :chart :data :*] data-update)
                 (add-handler :transform-enable [:t :chart] chart-transform-enable)
                 (add-handler :* [:t :chart :back-button] bb-enter)
                 (add-handler :node-destroy [:**] d/default-exit))
          ;; create a mock input-queue
          last-user-action (atom nil)
          input-queue (->TestQueue last-user-action)
          ;; create renderer
          r (renderer :root ls)]

      ;; render the page
      (r [[:node-create [:t] :map]] input-queue)
      
      (is (= (:root @d/*dom*)
             {:content nil
              :attrs {:id :root}
              :children [{:content nil :attrs {:id :timeline}}]}))

      ;; check that no transforms are available
      (d/click! :back-button)
      
      (is (= @last-user-action nil))

      ;; render the chart
      (r [[:node-create [:t :chart] :map]
          [:transform-enable [:t :chart] :group-selected [{msg/topic :timeline (msg/param :group-id) {}}]]
          [:node-create [:t :chart :data] :vector]
          [:node-create [:t :chart :back-button] :map]
          [:value [:t :chart :back-button] nil "Back to Index"]
          [:transform-enable [:t :chart :back-button] :nav [{:page :attributes}]]]
         input-queue)
      
      (is (= (:root @d/*dom*)
             {:content nil
              :attrs {:id :root}
              :children [{:content nil
                          :attrs {:id :timeline}
                          :children [{:content "Timeline Chart"
                                      :attrs {:id :chart}
                                      :children [{:content nil
                                                  :attrs {:id :chart-content}}
                                                 {:content "Back to Index"
                                                  :attrs {:id :back-button :class :button}}]}]}]}))

      ;; check that transforms are hooked up
      (d/click! :back-button)

      ;; The back button click caused a new message to be inserted onto the
      ;; input queue
      (is (= @last-user-action [{msg/type :nav :page :attributes}]))

      ;; The chart was clicked, so generated a new message for the input queue
      (d/click! :chart)
      ;; The message should be on the queue
      (is (= @last-user-action [{msg/type :group-selected msg/topic :timeline (msg/param :group-id) {}}]))

      ;; add a group to the chart
      (r [[:node-create [:t :chart :data 0] :map]
          [:value [:t :chart :data 0] nil {:group-id 0 :tx-count 1}]]
         input-queue)
      
      ;; The dom should now contain a timeline chart, one chart data object, and a back button
      (is (= (:root @d/*dom*)
             {:content nil
              :attrs {:id :root}
              :children [{:content nil
                          :attrs {:id :timeline}
                          :children [{:content "Timeline Chart"
                                      :attrs {:id :chart}
                                      :children [{:content nil
                                                  :attrs {:id :chart-content}
                                                  :children [{:attrs {:id :chart-data-0}
                                                              :content {:group-id 0 :tx-count 1}}]}
                                                 {:content "Back to Index"
                                                  :attrs {:id :back-button :class :button}}]}]}]}))

      ;; update a group
      (r [[:value [:t :chart :data 0] nil {:group-id 0 :tx-count 2}]]
         input-queue)

      ;; The chart data content should have been updated
      (is (= (:root @d/*dom*)
             {:content nil
              :attrs {:id :root}
              :children [{:content nil
                          :attrs {:id :timeline}
                          :children [{:content "Timeline Chart"
                                      :attrs {:id :chart}
                                      :children [{:content nil
                                                  :attrs {:id :chart-content}
                                                  :children [{:attrs {:id :chart-data-0}
                                                              :content {:group-id 0 :tx-count 2}}]}
                                                 {:content "Back to Index"
                                                  :attrs {:id :back-button :class :button}}]}]}]}))

      ;; add another group
      (r [[:node-create [:t :chart :data 1] :map]
          [:value [:t :chart :data 1] nil {:group-id 1 :tx-count 3}]]
         input-queue)
      ;; Another chart data group should have been added
      (is (= (:root @d/*dom*)
             {:content nil
              :attrs {:id :root}
              :children [{:content nil
                          :attrs {:id :timeline}
                          :children [{:content "Timeline Chart"
                                      :attrs {:id :chart}
                                      :children [{:content nil
                                                  :attrs {:id :chart-content}
                                                  :children [{:attrs {:id :chart-data-0}
                                                              :content {:group-id 0 :tx-count 2}}
                                                             {:attrs {:id :chart-data-1}
                                                              :content {:group-id 1 :tx-count 3}}]}
                                                 {:content "Back to Index"
                                                  :attrs {:id :back-button :class :button}}]}]}]}))

      ;; remove a group
      (r [[:node-destroy [:t :chart :data 0]]]
         input-queue)
      ;; The original chart data group should have been removed
      (is (= (:root @d/*dom*)
             {:content nil
              :attrs {:id :root}
              :children [{:content nil
                          :attrs {:id :timeline}
                          :children [{:content "Timeline Chart"
                                      :attrs {:id :chart}
                                      :children [{:content nil
                                                  :attrs {:id :chart-content}
                                                  :children [{:attrs {:id :chart-data-1}
                                                              :content {:group-id 1 :tx-count 3}}]}
                                                 {:content "Back to Index"
                                                  :attrs {:id :back-button :class :button}}]}]}]}))

      ;; remove the timeline chart
      (r [[:node-destroy [:t :chart]]]
         input-queue)
      ;; Remove the entire timeline chart
      (is (= (:root @d/*dom*)
             {:content nil
              :attrs {:id :root}
              :children [{:content nil :attrs {:id :timeline} :children []}]}))
      
      ;; check that all transforms have been removed      
      (p/put-message input-queue nil)

      ;; None of the dom click listenrs should be working, because the
      ;; elements they were listening for, were destroyed
      (is (nil? @last-user-action))
      (d/click! :back-button)
      (is (nil? @last-user-action))
      (d/click! :chart)
      (is (nil? @last-user-action))

      ;; remove the page
      (r [[:node-destroy [:t]]]
         input-queue)
      
      (is (= (:root @d/*dom*)
             {:content nil
              :attrs {:id :root}
              :children []})))))
