(ns memebrane.algorithms.mab.util)

(defn average [& ns]
  (/ (apply + ns)
     (count ns)))

(defn index-max [xs]
  ; This is suboptimal, in that during early stages, when all values are 0, the last arm always gets chosen. 
  ; It would be better to pick at random in this case.	
  (first (apply max-key second (map-indexed vector xs))))

(defn last-fn [f default coll]
  (if (empty? coll)
    default
    (f (last coll))))

(defn categorical-draw [probs] 
  (let [p (rand)]
    (count (take-while #(< % p) (reductions + probs)))))