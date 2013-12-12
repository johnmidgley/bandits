(ns memebrane.algorithms.mab.analysis-test
  (:require [memebrane.algorithms.mab.analysis :as a]
            [memebrane.algorithms.mab.bandit.epsilon-greedy :as e]
            [memebrane.algorithms.mab.bandit.softmax :as sm]
            [memebrane.algorithms.mab.bandit.annealing-softmax :as asm]
            [memebrane.algorithms.mab.bandit.ucb :as u]
            [memebrane.algorithms.mab.arm.bernoulli :as b])
  (:use clojure.test))

(def arms (mapv b/bernoulli [1/10 1/10 1/10 9/10 1/10]))
(def n-simulations 10)
(def horizon 100)

(defn compare-bandits [bandits]
  (a/compare-bandits n-simulations horizon arms bandits))

(deftest test-epsilon-greedy
  (is (compare-bandits [(partial e/epsilon-greedy 1/10)])))

(deftest test-softmax
  (is (compare-bandits [(partial sm/softmax 1/10)])))

(deftest test-annealing-softmax
  (is (compare-bandits [asm/annealing-softmax])))

(deftest test-ucb
  (is (compare-bandits [u/ucb])))

