(ns streaminggame.core
  (:require [play-clj.core :refer :all]
            [play-clj.ui :refer :all]
            [play-clj.math :refer :all]))

(def max-fall-speed -4)
(def gravity-acceleration -0.33)
(def movement-acceleration 0.66)
(def max-movement-speed 3)
(def jump-velocity 4)

(defn mk-hero [x y w h]
  (assoc (shape :filled
                :set-color (color :red)
                :rect 0 0 w h)
         :x x
         :y y
         :w w
         :h h
         :velocity {:x 0 :y 0}
         :is :hero))

(defn mk-platform [x y w h]
  (assoc (shape :filled
                :set-color (color :green)
                :rect 0 0 w h)
         :x x
         :y y
         :is :platform
         :hitbox (rectangle x y w h)))

(defn clamp [n min-n max-n]
  (cond
    (< n min-n) min-n
    (> n max-n) max-n
    :else n))

(defn is? [named ent]
  (= (:is ent) named))

(defn update-velocity [hero entities]
  (let [x-velocity (:x (:velocity hero))
        y-velocity (:y (:velocity hero))
        new-y-velocity (+ y-velocity gravity-acceleration)]
    (assoc hero :velocity {:x (cond
                               (key-pressed? :a) (clamp (- x-velocity movement-acceleration) (* -1 max-movement-speed) 0)
                               (key-pressed? :d) (clamp (+ x-velocity movement-acceleration) 0 max-movement-speed)
                               :else 0)
                          :y (if (< new-y-velocity max-fall-speed)
                               max-fall-speed
                               new-y-velocity)})))

(defn do-jumping [{:keys [velocity] :as hero}]
  (if (key-pressed? :space)
    (assoc hero :velocity (assoc velocity :y 3))
    hero))

(defn collisions? [x y {:keys [w h] :as hero} entities]
  (let [hero-hitbox (rectangle x y w h)]
    (some (fn [platform]
            (rectangle! hero-hitbox :overlaps (:hitbox platform)))
          (filter (partial is? :platform) entities))))

(defn apply-velocity [{:keys [x y] :as hero} entities]
  (let [x-velocity (:x (:velocity hero))
        y-velocity (:y (:velocity hero))
        new-y-position (+ y-velocity y)
        new-x-position (+ x-velocity x) ]
    (assoc hero
           :y (if (collisions? x new-y-position hero entities)
                y
                new-y-position)
           :x (if (collisions? new-x-position y hero entities)
                x
                new-x-position))))

(defscreen main-screen
  :on-show
  (fn [screen entities]
    (update! screen :renderer (stage))
    [(mk-hero 100 700 16 48)
     (mk-platform 10 10 400 5)
     (mk-platform 100 100 400 5)
     (mk-platform 200 200 400 5)
     (mk-platform 300 300 400 5)])

  :on-render
  (fn [screen entities]
    (clear!)
    (render! screen
             (map (fn [ent]
                    (if (is? :hero ent)
                      (-> ent
                          (update-velocity entities)
                          do-jumping
                          (apply-velocity entities))
                      ent)) entities))))

(defgame streaminggame-game
  :on-create
  (fn [this]
    (set-screen! this main-screen)))

