(ns nurokit.test-impl
  (:use [clojure test])
  (:require [nurokit.impl :as imp])
  (:require [nurokit.protocols :as np])
  (:require [mikera.cljutils.error :refer [error error?]]))

(deftest test-map-model
  (let [m (imp/->MapModel {1 2 2 1})]
    (is (error? (np/output m)))
    (let [mt (np/think m 1)]
      (is (= 1 (:input mt)))
      (is (= 2 (np/output mt)))
      (let [mtc (np/clean mt)]
        (is (error? (np/output m)))))
    (is (error? (np/think m 3)))
    (let [mt (assoc m :create-fn (fn [_] :new))]
      (is (= :new (np/output (np/think mt 3)))))))

;