(ns memebrane.algorithms.mab.bandit.softmax
  (:require [memebrane.algorithms.mab.bandit :as b]
            [memebrane.algorithms.mab.util :as u]
            [incanter.core :as i]))

(defn softmax [temperature n-arms]
  (merge (b/bandit :softmax n-arms)
         {:temperature temperature}))

(defn select-arm-by-temperature [arm-stats temperature]
  (let [values (mapv #(-> % :value (/ temperature) (i/exp)) arm-stats)
        z (apply + values)
        probs (map #(/ % z) values)]
    (u/categorical-draw probs)))

(defmethod b/select-arm :softmax [e]
  (let [{:keys [arm-stats temperature]} e]
    (select-arm-by-temperature arm-stats temperature)))