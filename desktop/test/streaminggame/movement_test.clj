(ns streaminggame.movement-test
  (:require [midje.sweet :refer :all]
            [streaminggame.movement :as m]
            [play-clj.math :refer :all]))

(defn mk-fake-platform [x y w h]
  {:is :platform
   :hitbox (rectangle x y w h)})

(def fake-hero {:x 0
                :y 0
                :w 16
                :h 16
                :velocity {:x 0 :y 0}})

(def fake-entities [(mk-fake-platform 0 0 16 16)])

(def dt-for-one-frame 0.016)

(facts "Updating the velocity"
       (fact "If no keys are pressed sets x velocity to 0"
             (m/update-velocity fake-hero fake-entities dt-for-one-frame)
             => (contains {:velocity (contains {:x 0})})
             (provided
               (m/get-keys) => {:a false :d false :space false}))
       (fact "If no keys are pressed hero should fall"
             (m/update-velocity fake-hero fake-entities dt-for-one-frame)
             => (contains {:velocity (contains {:y -16.0})})
             (provided
               (m/get-keys) => {:a false :d false :space false}))
       (fact "Can't fall faster than max falling speed"
             (m/update-velocity fake-hero fake-entities 100)
             => (contains {:velocity (contains {:y m/max-fall-speed})})
             (provided
               (m/get-keys) => {:a false :d false :space false}))
       (fact "If a is pressed and change in x-velocity is small enough we minus it from x velocity"
             (m/update-velocity fake-hero fake-entities dt-for-one-frame)
             => (contains {:velocity (contains {:x -12.8})})
             (provided
               (m/get-keys) => {:a true :d false :space false}))
       (fact "If d is pressed and change in x-velocity is small enough we add it to the x velocity"
             (m/update-velocity fake-hero fake-entities dt-for-one-frame)
             => (contains {:velocity (contains {:x 12.8})})
             (provided
               (m/get-keys) => {:a false :d true :space false}))
       (fact "If a is pressed and dvx + vx > max x velocity, then x = max x velocity"
             (m/update-velocity fake-hero fake-entities 100)
             => (contains {:velocity (contains {:x (* -1 m/max-movement-speed)})})
             (provided
               (m/get-keys) => {:a true :d false :space false}))
       (fact "If d is pressed and dvx + vx > max x velocity, then x = max x velocity"
             (m/update-velocity fake-hero fake-entities 100)
             => (contains {:velocity (contains {:x m/max-movement-speed})})
             (provided
               (m/get-keys) => {:a false :d true :space false}))

       (fact "If space is pressed and hero can jump we set y velocity to jump velocity"
             (m/update-velocity (assoc fake-hero
                                       :can-jump? true) fake-entities dt-for-one-frame)
             => (contains {:velocity (contains {:y m/jump-velocity})})
             (provided
               (m/get-keys) => {:a false :d false :space true})))

(facts "About finding colliding platforms"
      (fact "If the player overlaps with a platform, return that platform"
            (m/find-colliding-platform 0 0 fake-hero fake-entities)
            => (contains {:is :platform})))

