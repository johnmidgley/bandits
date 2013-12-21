(ns memebrane.algorithms.mab.bandit.thompson-sampling
  (:require [memebrane.algorithms.mab.bandit :as b]
            [memebrane.algorithms.mab.util :as u]
            [incanter.distributions :as d]))

(defn thompson-sampling [alpha beta n-arms]
  (println "thompson-sampling")
  (merge (b/bandit :thompson-sampling n-arms)
         {:alpha alpha :beta beta}))

(defn draw-beta [alpha beta] 
  (d/draw (d/beta-distribution alpha beta)))

(defn arm-stats->s-and-f [arm-stats]
  (let [{:keys [count value]} arm-stats
        s (* count value)
        f (- count s)]
    [s f]))

(defn draw-arm [alpha beta arm-stats]
  (let [[s f] (arm-stats->s-and-f arm-stats)]
    (draw-beta (+ alpha s) (+ beta f))))

(defmethod b/select-arm :thompson-sampling [bandit]
  (let [{:keys [arm-stats alpha beta]} bandit]
    (u/index-max (map (partial draw-arm alpha beta) arm-stats))))

