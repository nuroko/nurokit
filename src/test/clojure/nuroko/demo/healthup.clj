(ns nuroko.demo.healthup
  (:use [nuroko.lab core charts])
  (:use [nuroko.gui visual])
  (:use mikera.cljutils.error) 
  (:require [clj-time.core :as time]) 
  (:use [incanter core io charts stats]) 
  (:require [clojure.core.matrix :as m]) 
  (:require [mikera.cljutils [vectors :refer [vector-without]]]) 
  (:require [clojure.data.csv :as csv]) 
  (:require [mikera.vectorz.core :as v]) 
  (:require [mikera.vectorz.matrix-api]) 
  (:require [clojure.java.io :as io])
  (:require [task.core :as task])
  (:import [mikera.vectorz Vectorz Ops AVector]))

;;(with-open [in-file (io/reader (io/resource "temp/medtronic-carelink-export.csv"))]
;;     (count (csv/read-csv in-file)))

(def blank-row [nil 0.0 0.0 nil])

(def DATE-FORMATTER (clj-time.format/formatter "MM/dd/yyyy HH:mm"))
(def TIME-FORMATTER (clj-time.format/formatter "HH:mm"))
(def START-DATE (clj-time.format/parse DATE-FORMATTER "9/7/2009 18:00"))
(def END-DATE (clj-time.format/parse DATE-FORMATTER "05/06/2013 18:00"))

(defn time-period 
  ([date]
    (let [interval (time/interval START-DATE date)
          mins (time/in-minutes interval)]
      (quot mins 5))))

(defn period-to-time 
  ([period]
    (time/plus START-DATE (time/minutes (* period 5)))))

(defn period-to-str
  ([period]
    (clj-time.format/unparse TIME-FORMATTER (period-to-time period))))

(def DATA 
  (atom 
    (vec (map 
           #(conj blank-row 
                  (period-to-str %)
                  (clj-time.format/unparse DATE-FORMATTER (period-to-time %))
                  ) (range 420480)))))


(with-open [in-file (io/reader (io/resource "temp/medtronic-carelink-export.csv"))]
  (let [data (drop 11 (csv/read-csv in-file))
        title-row (first data)
        data (next data)
        ;; data (take 10000 data)
        ]
    (def HEADINGS (reduce (fn [v s] (conj v s)) [] title-row))
    (def HEADING-MAP (into (sorted-map) (map-indexed (fn [i s] [s i]) HEADINGS)))
    (def TYPE-COL (or (HEADING-MAP "Raw-Type") (error "col not found!"))) 
    (def TIMESTAMP-COL (or (HEADING-MAP "Timestamp") (error "col not found!"))) 
    (def BG-COL (or (HEADING-MAP "Sensor Glucose (mg/dL)") (error "col not found!"))) 
    (def CARB-COL (or (HEADING-MAP "BWZ Carb Input (grams)") (error "col not found!"))) 
    (def INSULIN-COL (or (HEADING-MAP "Bolus Volume Delivered (U)") (error "col not found!"))) 
    (doseq [r data]
      (swap! DATA (fn [vc r]
                    (try
                      (let [date-string (nth r TIMESTAMP-COL)
                          ;; _ (println date-string)
                          type-string (nth r TYPE-COL)
                          date (clj-time.format/parse DATE-FORMATTER date-string)
                          period (time-period date)
                          old-dr (nth vc period)]
                        (cond
                          (= type-string "GlucoseSensorData") 
                            (assoc vc period (assoc old-dr 0 (double (java.lang.Integer/parseInt (nth r BG-COL)))))
                          (= type-string "BolusNormal") 
                            (assoc vc period (assoc old-dr 1 (java.lang.Double/parseDouble (nth r INSULIN-COL))))
                          (= type-string "BolusWizardBolusEstimate") 
                            (assoc vc period (assoc old-dr 2 (double (java.lang.Integer/parseInt (nth r CARB-COL)))))
                          :else vc))
                      (catch Throwable t
                        vc))) r)
      )))

(with-open [out-file (io/writer "out-file.csv")]
     (csv/write-csv out-file
                    (map #(nth @DATA %) (range 104000 120000))))
(show (vector-bars (map #(or (nth (nth @DATA %) 0) 0.0) (range 104000 104300))))

(def FIELDS [:BG :insulin :carb])

(def data)

(def INPUT-SIZE 240)
(def OUTPUT-SIZE 1)

(defn norm-bg ^double [^double v]
  (* 0.02 (- v 100.0)))

(defn lag-bg [data pos]
  (let [v (nth (nth data pos) 0)
        vl (nth (nth data (dec pos)) 0)]
    (try 
      (if (and v vl)
        (double (- v vl))
        nil)
      (catch Throwable t
        (println pos)
        (throw t)))))

(defn feature-vector 
  (^AVector [pos] (feature-vector @DATA pos))
  (^AVector [dat pos]
	  (let [v (Vectorz/newVector INPUT-SIZE)]
	    (dotimes [i 72]
	      (let [;; bg (norm-bg (nth (nth dat (+ i (- pos 72))) 0))
	            bg (or (lag-bg dat (+ i (- pos 72))) 0.0)
	            ]
	        (.set v i (double (if bg bg 0.0))))) ;; blood glucose
	    (dotimes [i 72]
	      (.set v (+ i 72) (nth (nth dat (+ i (- pos 72))) 1))) ;; insulin
	    (dotimes [i 72]
	      (.set v (+ i 144) (nth (nth dat (+ i (- pos 72))) 2))) ;; carbs
	    (let [t (.set v (int (+ 216 (Integer/parseInt (.substring (nth (nth dat pos) 4) 0 2)))) 1.0)])
	    v)))



(defn target-vector ^AVector [pos]
  (let [dat @DATA
        v (Vectorz/newVector OUTPUT-SIZE)
        ;; tg (norm-bg (double (nth (nth dat pos) 0 java.lang.Double/NaN)))
        tg (or (lag-bg dat pos) 0.0)
        ]
    (.set v 0 (double tg))
    v))

(def net
    (stack
      (neural-network :inputs INPUT-SIZE  
                      :max-links 100
                      :output-op Ops/LINEAR
                      :outputs 1
                      :layers 3)))

(show (network-graph net :line-width 2) 
        :title "Neural Net : HealthUp")
  
(dotimes [i 100000]
  (let [dat @DATA
        pos (+ 73 (rand-int (- (count dat) 73)))]
    (.fill (.getGradient net) 0.0)
    (when (not (nil? (lag-bg dat pos)))
      (.train net 
        ^AVector (feature-vector pos) 
        ^AVector (target-vector pos) 
        ^nuroko.module.loss.LossFunction nuroko.module.loss.SquaredErrorLoss/INSTANCE 
        (double 1.0)))
    (when (== 0 (mod i 100)) 
      (.addMultiple (.getParameters net) (.getGradient net) 0.001)
      (println i))
    ))

(let [rg (range 7000 8000)]
  (show (scatter-plot
          (map #(.get (think net (feature-vector %)) 0) rg) 
          (map #(.get (target-vector %) 0) rg))))

(defn prediction [dat pos]
  (let [net (.clone net)
        past-rg (range (- pos 144) pos)
        pred-rg (range pos (+ pos 72))
        past-bg (mapv #(or (nth (nth dat %) 0) 0.0) past-rg)]
    (reduce    
      (fn [v pos] (conj v (+ (last v) (.get (think net (feature-vector dat pos)) 0))))
      past-bg
      pred-rg)))

;; (prediction 3000)

;; nice predicted vs. actual
;; (prediction @DATA 304200)
;; (map #(or (nth (nth @DATA %) 0) 0.0) (range 304056 304272))

