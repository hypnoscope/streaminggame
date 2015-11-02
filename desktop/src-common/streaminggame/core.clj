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
                          :y (if (< new-y-velocity max-fall-speed)
                               max-fall-speed
                               new-y-velocity)})))

(defn do-jumping [{:keys [velocity] :as hero}]
  (if (key-pressed? :space)
    (assoc hero :velocity (assoc velocity :y 3))
    hero))

(defn find-colliding-platform [x y {:keys [w h] :as hero} entities]
  (let [hero-hitbox (rectangle x y w h)]
    (some (fn [platform]
            (when (rectangle! hero-hitbox :overlaps (:hitbox platform)) platform))
          (filter (partial is? :platform) entities))))

(defn apply-y-velocity-for-colliding-platform [{:keys [h x y velocity] :as hero} platform]
  (let [top-of-platform (+ (:y platform) (:h platform))
        bottom-of-platform (- (:y platform) (:h platform))
        hero-is-above-platform (> (+ y 1) top-of-platform)]
    (assoc hero
           :y (if hero-is-above-platform
                     top-of-platform
                     (- bottom-of-platform h))
           :velocity (assoc velocity :y 0))))

(defn apply-y-velocity [{:keys [x y] :as hero} entities]
  (let [y-velocity (:y (:velocity hero))
        new-y-position (+ y-velocity y)
        platform (find-colliding-platform x new-y-position hero entities)]
    (if platform
      (apply-y-velocity-for-colliding-platform hero platform)
      (assoc hero :y new-y-position))))

(defn apply-x-velocity-for-colliding-platform [{:keys [x y w] :as hero} platform]
  (let [left-of-platform (:x platform)
        right-of-platform (+ left-of-platform (:w platform))
        hero-is-left-of-platform (< x left-of-platform)]
    (assoc hero :x (if hero-is-left-of-platform
                     (- left-of-platform w)
                     right-of-platform))))

(defn apply-x-velocity [{:keys [x y] :as hero} entities]
  (let [x-velocity (:x (:velocity hero))
        new-x-position (+ x-velocity x)
        platform (find-colliding-platform new-x-position y hero entities)]
    (if platform
      (apply-x-velocity-for-colliding-platform hero platform)
      (assoc hero :x new-x-position))))

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
                          (apply-x-velocity entities)
                          (apply-y-velocity entities)
                          )
                      ent)) entities))))

(defgame streaminggame-game
  :on-create
  (fn [this]
    (set-screen! this main-screen)))

