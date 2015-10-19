(ns ^:figwheel-always cljs-talk.core
    (:require [om.core :as om :include-macros true]
              [om.dom :as dom :include-macros true]
              [sablono.core :as html :refer-macros [html]]
              [cljs.core.async :as a :refer [chan <! >! put! close! timeout]]
              [goog.events :as events]
              [goog.events.EventType])
    (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(def SIZE 8)
(def MIN_SIGHT 2)

(defonce app-state
  (atom {:page 0
         :page-data {61 {:you     [6 3]
                         :view    [3 1]
                         :enemies [[1 0] [11 3]]}}}))

(def terrain
  (mapv vec ["   ######WWWWWW"
             "   #T    WWWWWW"
             "#### T T  WWW W"
             "W   T   T     W"
             "WWW     TT    W"
             "WWWW    TT  WWW"
             "WWWWW   TT    W"
             "WWWWW    T    W"
             "WWWWW  T      W"
             "WWWW         WW"
             "WWW#########WWW"]))

(def terrain-size [(dec (count (first terrain))) (dec (count terrain))])

;;;;;;;;;;;;;;;;;;;

(def keycode->direction
  {37 :left
   38 :up
   39 :right
   40 :down})

(defn superpose
  [terrain [x y] object]
  (assoc-in terrain [y x] object))

(defn world-view
  [{:keys [you view enemies]}]
  (let [[left top] view
        terrain    (reduce
                    (fn [accum enemy] (superpose accum enemy "M"))
                    (superpose terrain you "H")
                    enemies)]
    (map #(subvec % left (+ SIZE left)) (subvec terrain top (+ SIZE top)))))

(defn tile
  [s]
  (case s
    " " [:div.square.green]
    "H" [:div.square.green
         [:img.square {:src "images/stick-figure.png"}]]
    "T" [:div.square.green
         [:img.square {:src "images/tree.png"}]]
    "#" [:div.square.green
         [:img.square {:src "images/rocks.png"}]]
    "W" [:div.square.blue]
    "M" [:div.square.green
         [:img.square {:src "images/monster.png"}]]))

(defn row
  [s]
  (vec
   (concat
    [:tr] (mapv (fn [s] [:td (tile s)]) (vec s)))))

(defn grid
  [ss]
  [:table (vec (concat [:tbody] (mapv row ss)))])

(defn passable?
  [[x y]]
  (and (or (= " " (get-in terrain [y x]))
           (= "W" (get-in terrain [y x])))
       (<= 0 x (get terrain-size 0))
       (<= 0 y (get terrain-size 1))))

(defn move
  [[x y] direction]
  (case direction
    :left  [(dec x) y]
    :right [(inc x) y]
    :up    [x (dec y)]
    :down  [x (inc y)]
    [x y]))

(defn move-character
  [state character direction]
  (if (:dead? state)
    state
    (let [old-pos (get-in state character)
          new-pos (move old-pos direction)]
      (if (passable? new-pos)
        (assoc-in state character new-pos)
        state))))

(defn one-axis-reset-view
  [{:keys [you view] :as data} axis]
  (let [old (get view axis)
        y   (get you axis)
        new (cond
             (< (- y MIN_SIGHT) old)   (max 0 (- y MIN_SIGHT))
             (< (+ old SIZE)
                (inc (+ y MIN_SIGHT))) (min (- (inc (get terrain-size axis)) SIZE)
                                            (- (inc (+ y MIN_SIGHT)) SIZE)))]
    (assoc-in data [:view axis] (or new old))))

(defn reset-view
  [data]
  (-> data
      (one-axis-reset-view 0)
      (one-axis-reset-view 1)))

(defn move-you-and-update-view
  [state direction]
  (reset-view (move-character state [:you] direction)))

(defn check-if-you-are-eaten
  [state]
  (if (some #(= % (:you state)) (:enemies state))
    (assoc state :dead? true)
    state))

(defn move-enemies
  [{:keys [enemies] :as state}]
  (-> state
      (move-character [:enemies 0] (rand-nth [:left :right :up :down]))
      (move-character [:enemies 1] (rand-nth [:left :right :up :down]))
      (check-if-you-are-eaten)))

;;;;;;;;;;;;;;;;;;;

(defn show-partial
  [html-vec n]
  (let [f (first html-vec)
        s (second html-vec)]
    (cond
     (keyword? f)
     (html (mapv #(if (vector? %) (show-partial % n) %)
                 html-vec))

     (and (integer? f) (> f n))
     nil

     (and (integer? f) (<= f n) (string? s))
     s

     (and (integer? f) (<= f n))
     (html (mapv #(if (vector? %) (show-partial % n) %)
                 (second html-vec))))))

(defn make-page
  [frames page]
  (if (fn? page)
    [(fn [data]
       (html (page (get (:page-data data) (:page data)))))]
    (mapv (fn [frame]
            (constantly (show-partial page frame)))
          (range frames))))

(defn make-talk
  [& pages]
  (apply concat (map #(apply make-page %) pages)))

(def title
  [1 [:div.big-margin
      [:h1 "ClojureScript"]]])

(def contents
  [5 [:div
      [:h1 "Talk plan"]
      [:div.offset-by-two.ten.columns
       [:h2.left [:ol
                  [1 [:li "Why talk about ClojureScript?"]]
                  [2 [:li "Programs and Functions"]]
                  [3 [:li "Om"]]
                  [4 [:li "Example App"]]]]]]])

(def why-1
  [7 [:div
      [:h2.left "1. Why talk about ClojureScript?"]
      [:div.offset-by-one.eleven.columns
       [:h4.left [:ul
                  [1 [:li "a lot of the LoB tools are using it"]]
                  [2 [:li "it combines the very old " [:code "(lisp)"] "..."]]
                  [3 [:li "...with the very new:"
                      [:ul
                       [4 [:li "Clojure: the 21st century Lisp"]]
                       [5 [:li "JavaScript as \"machine code of the internet\" (a target for compilation)"]]]]]]]]
      [6 [:div.offset-by-three.nine.columns [:img {:src "images/lisp_cycles.png"}]]]]])

(defn progs-n-fns-title
  [content]
  [:div
   [:h2.left "2. Programs and Functions"]
   content])

(def progs-n-fns-1
  [2 (progs-n-fns-title
      [1 [:div.big-margin
          [:h3 "what " [:i "is"] " a computer program?"]]])])

(def progs-n-fns-2
  [3 (progs-n-fns-title
      [:div.offset-by-one.ten.columns.margin
       [:h4.left [:b "Definition:"] " A " [:i "computer program"] " is a sequence of instructions (taken from a limited, predefined, set of possible instructions) that a computer can follow."]
       [:div.small-margin
        [1 [:div.six.columns
            [:h4
             [:pre [:code.left
                    "set x = 0;\n"
                    "for i in [1 .. 100000]:\n"
                    "  set x = x + i;\n"
                    "print x;"]]]]]
        [2 [:div.six.columns
            [:h4
             [:pre [:code.left
                    "wait 20 milliseconds;\n"
                    "print 4999950000;"]]]]]]])])

(def progs-n-fns-3
  [5 (progs-n-fns-title
      [:div.margin
       [:div.offset-by-one.ten.columns
        [:h3 "Every lisp program is built out of expressions of the form: "
         [:code "(f a b c ...)"]]
        [:div.margin
         [1 [:h4 [:code "(+ 2 5) -> 7"]]]
         [2 [:h4 [:code "(* (- 12 5) (/ 10 2)) -> 35"]]]
         [3 [:h4 [:code "(filter even? [0 1 2 3 4 5]) -> [0 2 4]"]]]
         [4 [:h4 [:code "(map (fn [x] (+ x 2)) [10 6 2]) -> [12 8 4]"]]]]]])])

(def progs-n-fns-4
  [11 (progs-n-fns-title
       [:div.margin
        [:h3 "Spot the difference:"]
        [:div.small-margin
         [1 [:div.six.columns
             [:h4
              [:pre [:code.left
                     "(fn [a b c]\n"
                     [2 "  (if (< a b c)\n"]
                     [3 "    \"they are in order\"\n"]
                     [4 "    \"they are not in order\"))"]]]]
             [9 [:h3 "- is a " [:i "pure function"]]]]]
         [5 [:div.six.columns
             [:h4
              [:pre [:code.left
                     "(fn [a b c]\n"
                     [6 "  (if (< a b c)\n"]
                     [7 "    \"they are in order\"\n"]
                     [8 "    (launch-the-nukes!)))"]]]]
             [10 [:h3 "- has " [:i "effects"]]]]]]])])

(defn om-title
  [content]
  [:div
   [:h2.left "3. Om"]
   content])

(def om-1
  [4 (om-title
      [:div.margin.ten.columns
       [:h3.left
        [:ul
         [1 [:li "Based on Facebook's React framework"]]
         [2 [:li "Idea: put all the changeable data the UI needs in a single mutable " [:i "atom"] ", " [:code "app-state"]]]
         [3 [:li "Then implement the UI as a pure function:"]]]]
       [3 [:h3 [:code "app-state -> HTML"]]]])])

(defn app-title
  [content]
  [:div
   [:h2.left "4. Example App"]
   content])

(defn table-row
  [s]
  [:tr
   [:td [:div.square [:h4 [:code (pr-str s)]]]] [:td [:div.square]]
   [:td [:img.square {:src "images/arrow.png"}]] [:td [:div.square]]
   [:td (tile s)]])

(def app-1
  [8 (app-title
      [:div.margin
       [1 [:h3.left "Let's implement a function called " [:code "tile"] ":"]]
       [2 [:div.margin
           [:div.six.columns
            [:table
             [:tbody
              (table-row " ")
              [4 (table-row "T")]
              [6 (table-row "W")]]]]
           [3 [:div.six.columns
               [:table
                [:tbody
                 (table-row "H")
                 [5 (table-row "#")]
                 [7 (table-row "M")]]]]]]]])])

(def app-2
  [2 (app-title
       [:div.margin
        [:h3.left "Now we can use " [:code "(map tile \"T# TH W M\")"] " to get:"]
        [1 [:div.margin (grid ["T# TH W M"])]]])])

(def app-3
  [2 (app-title
      [:div.margin
       [:h3.left "or using " [:code "(map (fn [row] (map tile row)) data-array)"] ":"]
       [1 [:div.margin
           [:div.eight.columns
            [:h4.left [:pre [:code.left
                             "(def data-array [\" M #T    WWW\"\n"
                             "                 \"#### T T  WW\"\n"
                             "                 \"WWW T   T   \"\n"
                             "                 \"WWW   H TT M\"\n"
                             "                 \"WWWWW   TT  \"]"]]]]]]])])

(def app-4
  [1 (app-title
      [:div.margin
       [:h3.left "or using " [:code "(map (fn [row] (map tile row)) data-array)"] ":"]
       [:div.eight.columns.margin
        (grid [" M #T    WWW"
               "#### T T  WW"
               "WWW T   T   "
               "WWW   H TT M"
               "WWWWW   TT  "])]])])

(def app-5
  [3 (app-title
       [:div.margin
        [:h3.left "Now we need an initial value for the mutable state atom:"]
        [:h4 [:pre [:code.left
                    "(def app-state\n"
                    [1 "  (atom {:your-position     [6 3]\n"]
                    [2 "         :monster-positions [[1 0]\n"]
                    [2 "                             [11 3]]}))"]]]]])])

(def app-6
  [7 (app-title
      [:div.margin
       [:h4 [:pre [:code.left
                   "(defn move-character\n"
                   [1 "  [state character direction]\n"]
                   [2 "  (let [old-pos (get-in state character)\n"]
                   [3 "        new-pos (move old-pos direction)]\n"]
                   [4 "    (if (passable? new-pos)\n"]
                   [5 "      (assoc-in state character new-pos)\n"]
                   [6 "      state)))"]]]]])])

(def app-7
  [0 (fn [state]
       (app-title
        [:div.margin
         [(if (:dead? state)
            :div.offset-by-two.ten.columns.grey
            :div.offset-by-two.ten.columns)
          (grid (world-view state))]]))])

(def thank-you
  [1 [:div.big-margin [:h2 "Thanks."]]])

(def talk
  (make-talk title
             contents
             why-1
             progs-n-fns-1
             progs-n-fns-2
             progs-n-fns-3
             progs-n-fns-4
             om-1
             app-1
             app-2
             app-3
             app-4
             app-5
             app-6
             app-7
             thank-you))

(defn screen
  [data owner]
  (reify
    om/IRender
    (render [_]
      (html [:div
             [:p.left (str (:page data))]
             [:div.small-margin
              ((nth talk (:page data)) data)]]))))

(om/root
 screen
 app-state
 {:target (. js/document (getElementById "app"))})

(defn handle-keypress
  [e]
  (case (.-keyCode e)
    78 (swap! app-state update-in [:page] #(min (dec (count talk)) (inc %)))
    80 (swap! app-state update-in [:page] #(max 0 (dec %)))
    32 (swap! app-state assoc-in [:page-data 61 :dead?] false)
    (case (:page @app-state)
      61 (swap! app-state update-in [:page-data 61]
                move-you-and-update-view (keycode->direction (.-keyCode e)))
      (.log js/console (str (.-keyCode e) " - " (:page @app-state))))))

(let [c (chan)]
  (defonce listener
    (do
      (events/listen js/document goog.events.EventType.KEYDOWN #(put! c %))
      (go
        (while true
          (let [e (<! c)]
            (handle-keypress e)))))))

#_(go
  (while (= (:page @app-state) 61)
    (<! (timeout (rand-int 500)))
    (swap! app-state update-in [:page-data 61] move-enemies)))

(defn on-js-reload [] nil)
