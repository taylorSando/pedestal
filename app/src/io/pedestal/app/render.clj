; Copyright 2013 Relevance, Inc.

; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0)
; which can be found in the file epl-v10.html at the root of this distribution.
;
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
;
; You must not remove this notice, or any other, from this software.

(ns ^:shared io.pedestal.app.render
  (:require [io.pedestal.app.protocols :as p]
            [io.pedestal.app.util.platform :as platform]
            [io.pedestal.app.tree :as tree]))

(defn- consume-app-model-queue [queue in-queue app-model render-fn]
  "Take the next item from the app queue and process it using the render-fn
This function is recursive, and will continue to check if there are more items on the queue.
It processes the items using the render-fn.  
The render-fn takes two arguments.  The first is a list of deltas, which represent what has changed in the app-model,
as a result  of the last message on the app-model queue.  The second item is the input queue.  The render-fn needs to be
able to place messages on this queue so that it can pass back information to the application"
  (p/take-message queue
                  ;; This is the function that will process the next message on the app queue
                  (fn [message]
                    ;; Save the current app-model
                    (let [old-app-model @app-model
                          ;; Apply the next message to the app-model, creating a new
                          ;; app-model
                          new-app-model (swap! app-model tree/apply-deltas (:deltas message))
                          ;; Get a list of all the deltas that actually changed the state
                          ;; This may not be the same as (:deltas message), because some of the
                          ;; deltas could be duplicates, or invalid
                          deltas (tree/since-t new-app-model (tree/t old-app-model))]
                      ;; Call the render function with the list of deltas and the input queue
                      ;; Need to include the input queue, because the render function needs a way to
                      ;; Pass information back to the application to update it, based on changes in the
                      ;; view .  It does so by passing back messages to the input queue
                      (render-fn deltas in-queue)
                      ;; Recursive call to get the next item on the queue
                      (consume-app-model-queue queue in-queue app-model render-fn)))))

(defn consume-app-model [app render-fn]
  "Given an app and a render-fn, this function will call the render-fn whenever there are new
messages on the app-model queue"
  ;; Create the initial app model, which holds the entire application state and its history/transactions
  (let [app-model (atom tree/new-app-model)]
    ;; The :app-model in app is the internal app state
    ;; The input queue is the way of changing the application by sending its messages
    (consume-app-model-queue (:app-model app) (:input app) app-model render-fn)
    app-model))

(defn log-fn [deltas]
  (platform/log-group
   "<----------------------------------------------------------------------"
   "---------------------------------------------------------------------->"
   deltas))
