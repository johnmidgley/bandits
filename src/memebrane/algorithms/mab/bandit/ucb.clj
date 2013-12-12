(ns memebrane.algorithms.mab.bandit.ucb
  (:require [memebrane.algorithms.mab.bandit :as b]
            [memebrane.algorithms.mab.util :as u]
            [incanter.core :as i]))

(defn ucb [n-arms]
  (b/bandit :ucb n-arms))

(defn ucb-value [total-counts {:keys [count value]}]
  (let [bonus (i/sqrt (/ (* 2 (i/log total-counts)) count))]
    (+ value bonus)))

(defmethod b/select-arm :ucb [bandit]
  (let [arm-stats (:arm-stats bandit)
        unexplored-arm (first (b/unexplored-arms bandit))]
    (if unexplored-arm
      unexplored-arm
      (let [total-counts (reduce + (map :count arm-stats))]
        (u/index-max (map (partial ucb-value total-counts) arm-stats))))))