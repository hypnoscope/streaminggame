(ns streaminggame.core
  (:require [play-clj.core :refer :all]
            [play-clj.ui :refer :all]
            [play-clj.math :refer :all]
            [streaminggame.movement :as mv]
            [streaminggame.level :as level]
            [streaminggame.utils :as utils]))

;(def seed 100)
;(def random-number-generator (java.util.Random. seed))

;(defn random-n! [max-n]
;  (.nextInt random-number-generator max-n))

(defn mk-hero [x y w h]
  (assoc (shape :filled
                :set-color (color :red)
                :rect 0 0 w h)
         :x x
         :y y
         :w w
         :h h
         :velocity {:x 0 :y 0}
         :can-jump? false
         :is :hero
         :current-plat :no
         ))

(defn update-camera! [{:keys [x y] :as hero} screen]
  (let [screen-pos (input->screen screen x y)
        half-screen-height (/ (height screen) 2)
        ]
    (position! screen (/ (width screen) 2) (if (> y (/ (height screen) 2))
                                             y 
                                             half-screen-height 
                                             )))
  hero)

(defn get-hero-pos [entities]
  (let [hero (first (filter (partial utils/is? :hero) entities))]
    {:x (:x hero) :y (:y hero)}
    )
  )

(def min-delta 0.016)
(def max-delta (* 3 min-delta))

(defscreen main-screen
  :on-show
  (fn [screen entities]

    (update! screen
             :renderer (stage)
             :camera (orthographic))
    (into [] [(mk-hero 300
                       300
                       (level/block-size screen)
                       (* 1.6 (level/block-size screen)))
              (level/draw-row screen 0 "xxxxxxxxxxxxxxxxxxxx")
              (level/str->chunk level/level-design-1 1 screen) 
              (level/str->chunk level/level-design-2 (+ 1 level/blocks-high) screen) 
              (assoc (label "FPS ??" (color :green)) :x 0 :y 0 :is :fps)]))

  :on-render
  (fn [screen entities]
    (clear!)
    (let [delta-time (utils/clamp (:delta-time screen) min-delta max-delta)]
      (render! screen
               (map (fn [ent]
                      (cond
                        (utils/is? :hero ent) (-> ent
                                                  (mv/update-velocity entities delta-time)
                                                  (mv/apply-y-velocity entities delta-time)
                                                  (mv/apply-x-velocity entities delta-time)
                                                  (update-camera! screen))
                        (utils/is? :fps ent) (do (doto ent (label! :set-text (str "Position - "
                                                                                  (get-hero-pos entities)
                                                                                  " FPS "
                                                                                  (game :fps)
                                                                                  " dt "
                                                                                  delta-time)))
                                                 ent)
                        :else ent))
                    entities))))

  :on-resize
  (fn [screen entities]
    (height! screen 600)
    (width! screen 800)))

(defgame streaminggame-game
  :on-create
  (fn [this]
    (set-screen! this main-screen)))

;; use in repl to restart game
;; (on-gl  (set-screen! streaminggame-game  main-screen))
