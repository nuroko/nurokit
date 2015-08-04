(ns nurokit.test_impl
  (:use [clojure test])
  (:require [nurokit.impl :as imp])
  (:require [nurokit.protocols :as np]))

(deftest test-map-model
  (let [m (imp/->MapModel {1 2 2 1})]
    (is (thrown? Error (np/output m)))
    (let [mt (np/think m 1)]
      (is (= 2 (np/output mt)))
      (let [mtc (np/clean mt)]
        (is (thrown? Error (np/output m)))))))

