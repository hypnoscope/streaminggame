(ns streaminggame.level
  (:require [play-clj.core :refer :all]
            [play-clj.math :refer :all]
            [clojure.pprint :as pprint]))

(def level-design-1 (str "x        xx        x"
                         "xx                xx"
                         "x                  x"
                         "x        xx        x"
                         "x                  x"
                         "x                  x"
                         "x                  x"
                         "xxx              xxx"
                         "x  x            x  x"
                         "x      x           x"
                         "x        x         x"
                         "x           xxxxxxxx"
                         "x                  x"
                         "xxxxx    xx        x"
                         "x                  x"
                         "x                  x"
                         "xxxxxxxx           x"
                         "x                xxx"
                         "x              xxxxx"
                         "xxx              xxx"))


(def level-design-2 (str "x       xxxxx      x"
                         "x      xx   xx     x"
                         "xx                xx"
                         "xxx               xx"
                         "xxxx             xxx"
                         "xxxxx           xxxx"
                         "xxxxxx         xxxxx"
                         "xxxxxxx       xxxxxx"
                         "xxxxxxx       xxxxxx"
                         "xxxxx        xxxxxxx"
                         "xxxx        xxxxxxxx"
                         "xxx        xxxxxxxxx"
                         "xx       xxxxxxxxxxx"
                         "xxx     xxxxxxxxxxxx"
                         "xxxx        xxxxxxxx"
                         "xxx          xxxxxxx"
                         "xxxxx           xxxx"
                         "xxxxxx           xxx"
                         "xxxx       x      xx"
                         "xxxxx     xxx      x"))

(def blocks-wide 20)
(def blocks-high 20)

(defn block-size [screen]
  (/ (* 0.66 (width screen)) blocks-wide))

(defn mk-solid-block [screen x y]
  (let [block-size (block-size screen) 
        block-x (* block-size x)
        block-y (* block-size y)]
    (assoc (shape :filled
                  :set-color (color :blue)
                  :rect 0 0 block-size block-size)
           :x block-x 
           :y block-y
           :w block-size
           :h block-size
           :is :platform
           :hitbox (rectangle block-x block-y block-size block-size)))) 

(defn draw-row [screen y row]
  (map-indexed (fn [x b] (case b
                           \x (mk-solid-block screen x y)
                           nil)) row))

(defn- remove-new-lines [string]
  (remove (partial identical? \newline) string))

(defn- get-nth-row [string row-width n]
  (take row-width (drop (* n row-width) (remove-new-lines string))))

(defn str->chunk [design y-block-offset screen]
  (let [raw-rows (map (partial get-nth-row design blocks-wide) (range blocks-high))
        rows (reverse raw-rows)]
    (map-indexed (fn [y row] (draw-row screen (+ y y-block-offset) row))
                 rows)))

