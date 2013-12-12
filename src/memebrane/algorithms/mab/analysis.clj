(ns memebrane.algorithms.mab.analysis
  (:require [memebrane.algorithms.mab.bandit :as b]
            [memebrane.algorithms.mab.arm :as a]
            [memebrane.algorithms.mab.util :as u]))


(defn simulate-step [arms state t] 
       (let [{:keys [bandit results]} state
             chosen-arm (b/select-arm bandit)
             reward (a/draw (arms chosen-arm))]
         {:bandit (b/update bandit chosen-arm reward)
          :results (conj results 
                         {:t t
                          :chosen-arm chosen-arm
                          :reward reward
                          :cumulative-reward (+ reward 
                                                (u/last-fn :cumulative-reward 0 results))})}))

(defn simulate 
  ([algo-fn arms horizon]
   (repeatedly 
     #(reduce 
        (partial simulate-step arms)
        {:bandit (algo-fn (count arms)) 
         :results []}
        (range horizon)))))

(defn correct-arms [i-best results]
  (let [chosen-arms (map :chosen-arm results)]    
    (map #(if (= i-best %) 1 0) 
         chosen-arms)))

(defn p-best-arm [i-best simulations]
  (let [results (map :results simulations)
        correct-arms (map (partial correct-arms i-best) results)]
   (apply map u/average correct-arms)))

(defn average-reward [simulations]
  (let [results (map :results simulations)
        rewards (map #(map :reward %) results)]
   (apply map u/average rewards)))

(defn cumulative-reward [simulations]
  (let [results (map :results simulations)
        rewards (map #(map :reward %) results)
        cumulative-rewards (map (partial reductions +) rewards)]
   (apply map u/average cumulative-rewards)))

(defn analyze [i-best-arm n-simulations & args]
  (let [simulations (vec (take n-simulations (apply simulate args)))]
    {:bandit (-> simulations first :bandit)
     :p-best-arm (p-best-arm i-best-arm simulations)
     :average-reward (average-reward simulations)
     :cumulative-reward (cumulative-reward simulations)}))

(defn compare-bandits [n-simulations horizon arms bandits] 
  (let [i-best (u/index-max (map :p arms))]
    (mapv #(analyze i-best n-simulations % arms horizon) bandits)))