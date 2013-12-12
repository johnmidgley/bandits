(ns memebrane.algorithms.mab.arm.bernoulli
  (:require [memebrane.algorithms.mab.arm :as a]))

(defn bernoulli [p]
  {:arm-type :bernoulli
   :p p})

(defmethod a/draw :bernoulli [arm]
  (if (> (rand) (:p arm)) 0 1))