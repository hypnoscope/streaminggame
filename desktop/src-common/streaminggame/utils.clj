(ns streaminggame.utils)

(defn is? [named ent]
  (= (:is ent) named))

(defn clamp [n min-n max-n]
  (cond
    (< n min-n) min-n
    (> n max-n) max-n
    :else n))

