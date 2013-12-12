(ns memebrane.algorithms.mab.bandit.annealing-softmax
  (:require [memebrane.algorithms.mab.bandit :as b]
            [memebrane.algorithms.mab.bandit.softmax :as s]
            [incanter.core :as i]))

(defn annealing-softmax [n-arms]
  (b/bandit :annealing-softmax n-arms))

(defmethod b/select-arm :annealing-softmax [bandit]
  (let [{:keys [arm-stats]} bandit
        t (->> arm-stats (map :count) (apply +) inc)
        temperature (/ 1 (i/log (+ t 0.0000001)))]
    (s/select-arm-by-temperature arm-stats temperature)))