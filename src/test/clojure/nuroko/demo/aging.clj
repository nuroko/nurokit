(ns nuroko.demo.aging
  (:use [nuroko.lab core charts])
  (:use [nuroko.gui visual])
  (:use [clojure.repl])
  (:use [clojure.core.matrix])
  (:require [task.core :as task])
  (:import [mikera.vectorz Op Ops])
  (:import [mikera.vectorz.ops ScaledLogistic Logistic Tanh])
  (:import [nuroko.coders CharCoder])
  (:import [mikera.vectorz AVector Vectorz]))

(set-current-implementation :vectorz)

(def ROW-LENGTH 5)

;; construct a data row ( 5 elements enough?)
(defn row ([& ds] 
            (let [res (new-vector ROW-LENGTH)]
              (assign! res (concat ds (repeat 0.0))))))

;; training data
(def TDATA (atom []))

(dotimes [i 100]
  (swap! TDATA (fn [old] (conj old (row (Math/sin (* i 0.1)) (Math/sin  (* i 0.06) ) (Math/sin (* i 0.05)))))))