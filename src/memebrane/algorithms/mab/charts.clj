(ns memebrane.algorithms.mab.charts
  (:require [memebrane.algorithms.mab.bandit.epsilon-greedy :as e]
            [memebrane.algorithms.mab.bandit.softmax :as sm]
            [memebrane.algorithms.mab.bandit.annealing-softmax :as asm]
            [memebrane.algorithms.mab.bandit.ucb :as u]
            [memebrane.algorithms.mab.bandit.thompson-sampling :as ts]
            [memebrane.algorithms.mab.arm.bernoulli :as b]
            [memebrane.algorithms.mab.analysis :as a]
            [incanter.core :as i]
            [incanter.charts :as c]))

(defn plot-lines [title x-label y-label lines]
  (let [{:keys [xs ys series-label]} (first lines)
        plot (c/xy-plot
               xs
               ys
               :title title
               :x-label x-label
               :y-label y-label
               :series-label series-label
               :legend true)]
    (doseq [line (rest lines)]
      (let [{:keys [xs ys series-label]} line]
        (c/add-lines plot xs ys :series-label series-label)))
    (i/view plot)))

(defn analysis->line [y-fn label-fn analysis]
  (let [ys (y-fn analysis)]
    {:ys ys
     :xs (range (count ys))
     :series-label (label-fn analysis)}))

(defn analysis->bandit-name [analyses]
  (-> analyses first :bandit :bandit-type))

(defn analyses->line-labels [y-fn label-fn analyses]
  (map (partial analysis->line y-fn label-fn) analyses))

(defn plot-best-arm-comparison [label-fn analyses]
  (plot-lines 
    (str "Accuracy of the " (analysis->bandit-name analyses) " Algorithm")
    "Time"
    "Probability of Selecting Best Arm"
    (analyses->line-labels :p-best-arm label-fn analyses)))

(defn plot-average-reward-comparison [label-fn analyses]
  (plot-lines 
    (str "Performance of the " (analysis->bandit-name analyses) " Algorithm")
    "Time"
    "Average Reward"
    (analyses->line-labels :average-reward label-fn analyses)))

(defn plot-cumulative-reward-comparison [label-fn analyses]
  (plot-lines 
    (str "Cumulative Reward of the " (analysis->bandit-name analyses) " Algorithm")
    "Time"
    "Cumulative Reward of the Chosen Arm"
    (analyses->line-labels :cumulative-reward label-fn analyses)))

(def comparison-plot-fns [plot-best-arm-comparison 
                          plot-average-reward-comparison
                          plot-cumulative-reward-comparison])

(defn plot-comparison [plot-fns label-fn comparison]
  (doseq [plot-fn plot-fns]
    (plot-fn label-fn comparison)))

(def arms (mapv b/bernoulli [1/10 1/10 1/10 9/10 1/10]))
(def n-simulations 1000)
(def horizon 250)

(defn comparison [bandits]
  (future (a/compare-bandits n-simulations horizon arms bandits)))

(def epsilons [1/10 2/10 3/10 4/10 5/10])

(def epsilon-comparison 
  (comparison (map #(partial e/epsilon-greedy %) epsilons)))

(defn epsilon-label [line]
  (-> line :bandit :epsilon float))

(defn plot-epsilon-comparison [comparison]
  (plot-comparison comparison-plot-fns epsilon-label comparison))

(def temperatures [1/10 2/10 3/10 4/10 5/10])

(def softmax-comparison
  (comparison (map #(partial sm/softmax %) temperatures)))

(defn softmax-label [line]
  (-> line :bandit :temperature float))

(defn plot-softmax-comparison [comparison]
  (plot-comparison comparison-plot-fns softmax-label comparison))

(def annealing-softmax-comparison 
  (comparison [asm/annealing-softmax]))

(defn plot-annealing-softmax-comparison [comparison]
  (plot-comparison comparison-plot-fns (constantly "Annealing") comparison))

(def ucb-comparison
  (comparison [u/ucb]))

(defn plot-ucb-comparison [comparison]
  (plot-comparison comparison-plot-fns (constantly "UCB") comparison))

(def epsilon-2arms-comparison
  (future
    (a/compare-bandits 
      n-simulations 
      horizon
      (mapv b/bernoulli [1/10 9/10]) 
      (map #(partial e/epsilon-greedy %) epsilons))))

(defn plot-epsilon-2arms-comparison []
  (plot-comparison comparison-plot-fns epsilon-label @epsilon-2arms-comparison))


(def epsilon-200arms-comparison
  (future
    (a/compare-bandits 
      n-simulations 
      horizon
      (mapv b/bernoulli (conj (vec (repeat 199 1/10)) 9/10)) 
      (map #(partial e/epsilon-greedy %) epsilons))))

(defn plot-epsilon-200arms-comparison []
  (plot-comparison comparison-plot-fns epsilon-label @epsilon-200arms-comparison))

(def thompson-sampling-comparison
  (comparison [(partial ts/thompson-sampling 1 1)]))

(defn plot-thompson-sampling-comparison [comparison]
  (plot-comparison comparison-plot-fns (constantly "Thompson Sampling") comparison))














