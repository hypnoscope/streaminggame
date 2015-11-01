(ns streaminggame.core.desktop-launcher
  (:require [streaminggame.core :refer :all])
  (:import [com.badlogic.gdx.backends.lwjgl LwjglApplication]
           [org.lwjgl.input Keyboard])
  (:gen-class))

(defn -main
  []
  (LwjglApplication. streaminggame-game "streaminggame" 800 600)
  (Keyboard/enableRepeatEvents true))
