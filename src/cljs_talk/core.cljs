(ns ^:figwheel-always cljs-talk.core
    (:require [om.core :as om :include-macros true]
              [om.dom :as dom :include-macros true]
              [sablono.core :as html :refer-macros [html]]
              [cljs.core.async :as a :refer [chan <! >! put! close! timeout]]
              [goog.events :as events]
              [goog.events.EventType])
    (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(defonce app-state
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

(def title
  [:div.big-margin
   [:h1 "ClojureScript"]])

(def contents
  [:div
   [:h1 "Talk plan"]
   [:div.offset-by-two.ten.columns
    [:h2.left [:ol
               [1 [:li "Why talk about ClojureScript?"]]
               [2 [:li "Programs and Functions"]]
               [3 [:li "Om"]]
               [4 [:li "Examples"]]]]]])

(def why-1
  [:div
   [:h2.left "1. Why talk about ClojureScript?"]
   [:div.offset-by-one.eleven.columns
    [:h4.left [:ul
               [1 [:li "a lot of the LoB tools are using it"]]
               [2 [:li "it combines the very old " [:code "(lisp)"] "..."]]
               [3 [:li "...with the very new:"
                  [:ul
                   [4 [:li "Clojure: the 21st century Lisp"]]
                   [5 [:li "JavaScript as \"machine code of the internet\" (a target for compilation)"]]]]]]]]
   [6 [:div.offset-by-three.nine.columns [:img {:src "images/lisp_cycles.png"}]]]])

(defn progs-n-fns-title
  [content]
  [:div
   [:h2.left "2. Programs and Functions"]
   content])

(def progs-n-fns-1
  (progs-n-fns-title
   [1 [:div.big-margin
       [:h3 "what is a computer program?"]]]))

(def progs-n-fns-2
  (progs-n-fns-title
   [:div.offset-by-one.eleven.columns.margin
    [:h4.left "A sequence of instructions that a computer can follow?"]]))

(def thank-you
  [:div.big-margin [:h2 "Thanks."]])

(def talk
  (make-talk title
             contents
             why-1
             progs-n-fns-1
             progs-n-fns-2
             thank-you))

(defn screen
  [data owner]
  (reify
    om/IRender
    (render [_]
      (html [:div
             [:p.left (str (:page data))]
             [:div.small-margin
              (nth talk (:page data))]]))))

(om/root
 screen
 app-state
 {:target (. js/document (getElementById "app"))})

(defn handle-keypress
  [e]
  (case (.-keyCode e)
    78 (swap! app-state update-in [:page] #(min (dec (count talk)) (inc %)))
    80 (swap! app-state update-in [:page] #(max 0 (dec %)))
    (.log js/console (.-keyCode e))))

(let [c (chan)]

  (defonce listener
    (events/listen js/document goog.events.EventType.KEYDOWN #(put! c %)))

  (go
    (while true
      (let [e (<! c)]
        (handle-keypress e)))))

(defn on-js-reload [] nil)
