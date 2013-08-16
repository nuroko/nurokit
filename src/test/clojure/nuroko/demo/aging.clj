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
(def DISPLAY-LENGTH 100)

;; construct a data row ( 5 elements enough?)
(defn row ([& ds] 
            (let [res (new-vector ROW-LENGTH)]
              (assign! res (concat ds (repeat 0.0))))))

(defn rand-row ([] (row (rand) (rand) (rand))))

;; training data
(def TDATA (atom []))

(def DATA (atom []))

(defn reset []
  (reset! TDATA [])
  (dotimes [i 1000]
    (swap! TDATA (fn [old] (conj old (row (Math/sin (* i 0.1)) (Math/sin  (* i 0.06) ) (Math/sin (* i 0.05))))))))

(defn data-chart 
  ([data] (data-chart data (- (count data) DISPLAY-LENGTH)))
  ([data start] (data-chart data start (+ start DISPLAY-LENGTH)))
  ([data start end]
    (let [r (range (max 0 start) (min (count data) end))]
      (xy-chart-multiline r [(map #(.get ^AVector (data %) 0) r) 
                             (map #(.get ^AVector (data %) 1) r)
                             (map #(.get ^AVector (data %) 2) r)]))))

(defn append-data [row]
  (swap! DATA (fn [old] (conj old row))))

(reset)

;; =================== DEMO CODE FOLLOWS =====================

(defn demo []
  (reset)
  
  (show (data-chart @TDATA 0 1000))
  
  (dotimes [i 100]
     (append-data (rand-row))
     (show (data-chart @DATA)))


)