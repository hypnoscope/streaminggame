(ns streaminggame.core
  (:require [play-clj.core :refer :all]
            [play-clj.ui :refer :all]
            [play-clj.math :refer :all]))

(def max-fall-speed -4)
(def gravity-acceleration -1)
(def movement-acceleration 0.66)
(def max-movement-speed 3)
(def jump-velocity 8)

(defn mk-hero [x y w h]
  (assoc (shape :filled
                :set-color (color :red)
                :rect 0 0 w h)
         :x x
         :y y
         :w w
         :h h
         :velocity {:x 0 :y 0}
         :is :hero
         :can-jump? false))

(defn mk-platform [x y w h]
  (assoc (shape :filled
                :set-color (color :green)
                :rect 0 0 w h)
         :x x
         :y y
         :w w
         :h h
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
                          :y (if (and (key-pressed? :space) (:can-jump? hero))
                               jump-velocity
                               (clamp new-y-velocity max-fall-speed jump-velocity))})))

(defn collisions? [x y {:keys [w h] :as hero} entities]
  (let [hero-hitbox (rectangle x y w h)]
    (some (fn [platform]
            (when (rectangle! hero-hitbox :overlaps (:hitbox platform)) platform))
          (filter (partial is? :platform) entities))))

(defn update-y-for-collision [{:keys [x y h velocity] :as hero} platform]
  (let [platform-top (+ (:y platform) (:h platform))
        platform-bottom (:y platform)
        above-platform? (not (> platform-top y))]
   (assoc hero
          :y (if above-platform?
               platform-top
               (- platform-bottom h))
          :can-jump? above-platform?
          :velocity (assoc velocity :y 0))))

(defn update-y-position [{:keys [x y velocity] :as hero} entities]
  (let [new-y-position (+ y (:y velocity))
        collider (collisions? x new-y-position hero entities)]
    (if collider
      (update-y-for-collision hero collider)
      (assoc hero
             :y new-y-position
             :can-jump? false))))

(defn update-x-position [{:keys [x y velocity] :as hero} entities]
  (let [new-x-position (+ x (:x velocity))
        collision? (collisions? new-x-position y hero entities)]
    (-> hero
        (assoc :x (if collision? x new-x-position)))))

(defn apply-velocity [{:keys [x y] :as hero} entities]
  (-> hero
      (update-y-position entities)
      (update-x-position entities)))

(defscreen main-screen
  :on-show
  (fn [screen entities]
    (update! screen :renderer (stage))
    [(mk-hero 100 700 16 48)
     (mk-platform 10 10 400 50)
     (mk-platform 100 100 400 50)
     (mk-platform 200 200 400 50)
     (mk-platform 300 300 400 50)])

  :on-render
  (fn [screen entities]
    (clear!)
    (render! screen
             (map (fn [ent]
                    (if (is? :hero ent)
                      (-> ent
                          (update-velocity entities)
                          (apply-velocity entities))
                      ent)) entities))))

(defgame streaminggame-game
  :on-create
  (fn [this]
    (set-screen! this main-screen)))

