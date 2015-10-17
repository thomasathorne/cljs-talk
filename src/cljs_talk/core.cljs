(ns ^:figwheel-always cljs-talk.core
    (:require [om.core :as om :include-macros true]
              [om.dom :as dom :include-macros true]
              [sablono.core :as html :refer-macros [html]]
              [cljs.core.async :as a :refer [chan <! >! put! close! timeout]]
              [goog.events :as events]
              [goog.events.EventType])
    (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(def app-state
  (atom {:page 0}))

(defn show-partial
  [html-vec n]
  (let [f (first html-vec)]
    (cond
     (and (integer? f) (<= f n)) (html (mapv #(if (vector? %) (show-partial % n) %)
                                             (second html-vec)))
     (and (integer? f) (> f n))  nil
     (keyword? f)                (html (mapv #(if (vector? %) (show-partial % n) %)
                                             html-vec)))))

(def title-page
  [0 [:div.twelve.columns
      [:h1 "ClojureScript:"]
      [:h1]
      [1 [:h3 "Programming UI in Functional Style"]]]])

(def other-page
  [:div
   [0 [:div.six.columns
       [:h4 "Here is a big piece of text describing some stuff that the talk is about."]
       [1 [:h4 [:code "[0 1 2 3 4 5 6]"]]]]]
   [2 [:div.six.columns
       [:h4 "Here is another big piece of text describing some other
             stuff that the talk is about."]
       [3 [:h4 [:code "[0 1 2 3 4 5 6]"]]]]]])

(defn make-page
  [page]
  (loop [views-so-far [(show-partial page 0)]
         n            1]
    (if (= (js->clj (show-partial page n))
           (js->clj (last views-so-far)))
      views-so-far
      (recur (conj views-so-far (show-partial page n)) (inc n)))))

(defn make-talk
  [& pages]
  (apply concat (map make-page pages)))

(defn screen
  [data owner]
  (reify
    om/IRender
    (render [_]
      (html [:div
             [:p.left (str (:page data))]
             [:div.screen
              (nth (make-talk title-page other-page) (:page data))]]))))

(om/root
 screen
 app-state
 {:target (. js/document (getElementById "app"))})

(defn handle-keypress
  [e]
  (case (.-keyCode e)
    78 (swap! app-state update-in [:page] inc)
    80 (swap! app-state update-in [:page] dec)
    (.log js/console e)))

(let [c (chan)]

  (events/listen js/document goog.events.EventType.KEYDOWN #(put! c %))

  (go
    (while true
      (let [e (<! c)]
        (handle-keypress e)))))
