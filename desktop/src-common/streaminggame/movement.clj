(ns streaminggame.movement
  (:require [play-clj.core :refer :all]
            [play-clj.math :refer :all]
            [streaminggame.utils :as utils]))

(def max-fall-speed -500)
(def gravity-acceleration -1000)
(def movement-acceleration 800)
(def max-movement-speed 200)
(def jump-velocity 600)

(defn find-colliding-platform [x y {:keys [w h] :as hero} entities]
  (let [hero-hitbox (rectangle x y w h)]
    (some (fn [platform]
            (when (rectangle! hero-hitbox :overlaps (:hitbox platform))
              (do
                (shape! platform :set-color (color :yellow))
              platform)))
          (filter (partial utils/is? :platform) entities))))

(defn update-velocity [hero entities dt]
  (let [x-velocity (:x (:velocity hero))
        y-velocity (:y (:velocity hero))
        new-y-velocity (+ y-velocity (* dt gravity-acceleration))
        change-in-x-acceleration (* dt movement-acceleration)]
    (assoc hero :velocity {:x (cond
                                (key-pressed? :a) (utils/clamp (- x-velocity change-in-x-acceleration) (* -1 max-movement-speed) 0)
                                (key-pressed? :d) (utils/clamp (+ x-velocity change-in-x-acceleration) 0 max-movement-speed)
                                :else 0)
                           :y (if (and (key-pressed? :space) (:can-jump? hero))
                                jump-velocity ; TODO - use delta?
                                (if (< new-y-velocity max-fall-speed) max-fall-speed new-y-velocity))})))

(defn apply-y-velocity-for-colliding-platform [{:keys [h x y velocity] :as hero} platform]
  (let [top-of-platform (+ (:y platform) (:h platform))
        bottom-of-platform (:y platform)
        hero-is-above-platform (> (+ y 1) top-of-platform)]
    (assoc hero
           :y (if hero-is-above-platform
                     top-of-platform
                     (- bottom-of-platform h))
           :velocity (assoc velocity :y 0)
           :can-jump? hero-is-above-platform)))

(defn apply-y-velocity [{:keys [x y] :as hero} entities delta-time]
  (let [y-velocity (:y (:velocity hero))
        new-y-position (+ (* delta-time y-velocity) y)
        platform (find-colliding-platform x new-y-position hero entities)]
    (if platform
      (apply-y-velocity-for-colliding-platform hero platform)
      (assoc hero
             :y new-y-position
             :can-jump? false))))

(defn apply-x-velocity-for-colliding-platform [{:keys [x y w] :as hero} platform]
  (let [left-of-platform (:x platform)
        right-of-platform (+ left-of-platform (:w platform))
        bottom-of-platform (:y platform)
        top-of-platform (+ bottom-of-platform (:h platform))
        hero-is-in-line-with-platform (and (> y bottom-of-platform)
                                           (<= y top-of-platform))
        hero-is-left-of-platform (< x left-of-platform)]
    (assoc hero
           :x (if hero-is-left-of-platform
                (- left-of-platform w)
                right-of-platform))))

(defn apply-x-velocity [{:keys [x y] :as hero} entities delta-time]
  (let [x-velocity (:x (:velocity hero))
        new-x-position (+ (* delta-time x-velocity) x)
        platform (find-colliding-platform new-x-position y hero entities)]
    (if platform
      (apply-x-velocity-for-colliding-platform hero platform)
      (assoc hero :x new-x-position))))

