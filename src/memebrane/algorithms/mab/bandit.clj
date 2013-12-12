(ns memebrane.algorithms.mab.bandit)

(defn arm-stats [n-arms]
  (vec (repeat n-arms {:count 0 :value 0})))

(defn bandit [bandit-type n-arms]
  {:bandit-type bandit-type
   :arm-stats (arm-stats n-arms)})

(defn unexplored-arms [bandit]
  (let [arm-stats (:arm-stats bandit)]
    (keep-indexed (fn [i stats] 
                    (when (= 0 (:count stats)) i)) arm-stats)))

(defmulti select-arm :bandit-type)

(defn update [bandit arm reward]
  (update-in bandit 
             [:arm-stats arm]
             (fn [{:keys [count value]}]
               (let [new-count (inc count)]
                 {:count new-count
                  :value (/ (+ (* count value) reward) new-count)}))))














