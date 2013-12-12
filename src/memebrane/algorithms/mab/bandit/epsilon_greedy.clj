(ns memebrane.algorithms.mab.bandit.epsilon-greedy
  (:require [memebrane.algorithms.mab.bandit :as b]
            [memebrane.algorithms.mab.util :as u]))

(defn epsilon-greedy [epsilon n-arms]
  (merge (b/bandit :epsilon-greedy n-arms)
         {:epsilon epsilon}))

(defmethod b/select-arm :epsilon-greedy [bandit]
  (let [{:keys [arm-stats epsilon]} bandit]
    (if (> (rand) epsilon)
      (u/index-max (map :value arm-stats))
      (rand-int (count arm-stats)))))