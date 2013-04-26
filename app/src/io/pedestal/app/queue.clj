; Copyright 2013 Relevance, Inc.

; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0)
; which can be found in the file epl-v10.html at the root of this distribution.
;
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
;
; You must not remove this notice, or any other, from this software.

(ns ^:shared io.pedestal.app.queue
  "A very simple application message queue implementation which can be used from both
  Clojure and ClojureScript. In the future, there will be both Clojure and ClojureScript
  implementations of this queue which will take advantage of the capabilities of each
  platform."
  (:require [io.pedestal.app.protocols :as p]
            [io.pedestal.app.util.platform :as platform]))

(defn- pop-message [queue-state]
  "Get the next message off of the queue"
  (let [{:keys [queue]} queue-state]
    ;; Is there anything in the queue?
    (if (seq queue)
      ;; Return both the next item from the queue, and the rest of the queue
      ;; in separate keys but in the same map
      (assoc queue-state
        :item (first queue)
        :queue (vec (rest queue)))
      ;; Indicate that there are is no message to take, because the queue is empty
      (assoc queue-state :item nil))))

(defn- process-next-item [queue f]
  "Determine if there are any messages to take from the queue.  
Call function f on the item if there is, otherwise, the function will recursively call itself
every 10ms until there is an item to process on the queue"
  ;; Are there any items on the queue?
  (if (seq (:queue @queue))
    ;; There are, so get the next message
    (if-let [item (:item (swap! queue pop-message))]
      ;; Use the function argument on the message
      (f item)
      ;; There was no message to process, so wait 10ms and try again
      (platform/create-timeout 10 (fn [] (process-next-item queue f))))
    ;; There were no more items on the queue, so wait 10ms and try again
    (platform/create-timeout 10 (fn [] (process-next-item queue f)))))

(defrecord AppMessageQueue [state]
  p/PutMessage
  (put-message [this message]
    ;; Place a new message on the queue
    (let [q (swap! state update-in [:queue] conj message)]
      ;; Return the number of items that are now on the queue
      (count (:queue q))))
  p/TakeMessage
  (take-message [this f]
    ;; Find out if there are any messages to take from the queue
    ;; and attempt to use the function f to process that message
    ;; if there is a message
    (process-next-item state f)))

(defn queue [name]
  "A factory function for creating an AppMessageQueue
Takes a single argument, name, which is the name of the queue"
  (->AppMessageQueue (atom {:queue [] :item nil :name name})))
