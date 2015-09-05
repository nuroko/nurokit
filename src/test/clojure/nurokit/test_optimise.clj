(ns nurokit.test-optimise
  (:use [clojure test])
  (:use [nurokit.optimise])
  (:require [clojure.core.matrix :as m])
  (:require [mikera.cljutils.error :refer [error error?]]))

(deftest test-map-model
  (let [SIZE 10
        a (adadelta SIZE)
        g (m/new-vector :vectorz SIZE)
        x (m/new-vector :vectorz SIZE)]
    (is (= [SIZE] (m/shape (:msdx a))))
    (update-parameters a g x)))