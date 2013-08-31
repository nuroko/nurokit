(ns nuroko.demo.city
  (:use [nuroko.lab core charts])
  (:use [nuroko.gui visual])
  (:use [clojure.repl])
  (:use [clojure.core.matrix])
  (:require [mikera.cljutils.text :as text])
  (:require [mikera.image.core :as im])
  (:require [mikera.image.colours :as col])
  (:require [task.core :as task])
  (:require [clj-time.core :as time] )
  (:require [server.socket :as ss])
  (:require [clojure.data.csv :as csv]) 
  (:require [mikera.cljutils.mouse :as ms])
  (:require [clojure.java.io :as io])
  (:import [java.awt.image BufferedImage]) 
  (:import [java.io InputStream OutputStream DataInputStream DataOutputStream])
  (:import [mikera.vectorz Op Ops])
  (:import [mikera.vectorz.ops ScaledLogistic Logistic Tanh])
  (:import [nuroko.coders CharCoder])
  (:import [nuroko.core IComponent])
  (:import [mikera.vectorz Vector AVector Vectorz])
  (:import [mikera.matrixx AMatrix Matrixx]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(set-current-implementation :vectorz)

;; ======================================================
;; DATA LOADING

(def START (time/date-time 2012 05 14 0 0 00 000))

;; time period in mins
(def PERIOD 15)

;; total number of periods
(def PERIODS (dec (* 24 (/ 60 PERIOD))))

(defn pint ^long [^String s]
  (Long/parseLong s))

(defn pd ^double [^String s]
  (Double/parseDouble s))

(defn parse-tstamp 
  "Parses a timstamp from two string in \"yyyyMMdd\" and \"hhmmss\" format"
  ([^String date-string ^String time-string]
    (time/date-time (pint (.substring date-string 0 4))
                    (pint (.substring date-string 4 6))
                    (pint (.substring date-string 6 8))
                    (pint (.substring time-string 0 2))
                    (pint (.substring time-string 2 4))
                    (pint (.substring time-string 4 6))
                    0)))

(def GW 100)
(def GH 80)

(def SCALE (* GW (/ (- 104.1 103.6)) ))

(defn long2g ^long [^double d]
  (long (* (- d 103.6) SCALE)))

(defn lat2g ^long [^double d]
  (long (* (- 1.57 d) SCALE)))

(defn ts2g 
  "Coverts a timestamp to a period number"
  (^long [timestamp]
    (if (not (time/before? timestamp START))
      (long (quot (time/in-minutes (time/interval START timestamp)) PERIOD))
      -1)))

(def ^AMatrix mmap (new-matrix GH GW))

(defonce data (vec (take PERIODS (repeatedly #(new-matrix GH GW)))))
(def loaded (atom 0))
(defn process-rec [ts lat long]
  (let [p (ts2g ts)
        x (long2g long)
        y (lat2g lat)]
    ;; (println (str ts ": " lat " , " long " = [" p "," x "," y "]"))
    (when (and (< -1 x GW) (< -1 y GH) (< -1 p PERIODS))
      (.addAt ^AMatrix (data p) (int y) (int x) 1.0)
      (swap! loaded inc))))

(defn clear-data []
  (reset! loaded 0)
  (doseq [m data]
    (scale! m 0.0)))

(defn load-data 
  ([fname] (load-data fname nil))
  ([fname len]
	  (clear-data) 
    (with-open [rdr (clojure.java.io/reader fname)]
	    (let [lseq (line-seq rdr)
	          lines (if len (take len lseq) lseq)]
	      (loop [lines (seq lines)]
	        (when lines
	          (let [line (first lines)
	                flds (first (csv/read-csv line))
	                ts (parse-tstamp (flds 0) (flds 3))
	                lat (pd (flds 6))
	                long (pd (flds 7))]
	            (process-rec ts lat long))
	          (recur (next lines))))))))

(defonce pdata (atom []))

(defn load-pdata [fname]
  (reset! pdata [])
  (with-open [rdr (clojure.java.io/reader fname)]
	    (let [lseq (line-seq rdr)
	          lines (next lseq) ;; skip titles
           ]
	      (loop [lines (seq lines)]
	        (when lines
	          (let [line (first lines)
	                flds (first (csv/read-csv line))
	                congestion (pd (flds 1))]
	            (swap! pdata conj congestion))
	          (recur (next lines)))))))

;; ======================================================
;; VISUALISATION

(defn col ^long [^double val]
  (let [lval (* (Math/log10 (+ 1.0 val)) 0.9)]
    (cond 
    (<= lval 0.0) 0xFF000000
    (<= lval 1.0) (let [v (- lval 0.0)] (col/rgb 0.0 0.0 v))
    (<= lval 2.0) (let [v (- lval 1.0)] (col/rgb v 0.0 (- 1.0 v)))
    (<= lval 3.0) (let [v (- lval 2.0)] (col/rgb 1.0 v 0.0))
    (<= lval 4.0) (let [v (- lval 3.0)] (col/rgb 1.0 1.0 v))
    :else 0xFFFFFFFFF)))

(defn city-image ^BufferedImage [^AMatrix data]
  (let [^BufferedImage bi (im/new-image GW GH)]
    (dotimes [y GH]
      (dotimes [x GW]
        (.setRGB bi (int x) (int y) (int (col (.get data (int y) (int x)))))))
    bi)) 

;;(Vectorz/fillRandom (.asVector mmap))
;;(scale! mmap 100)
;;:OK

;; ======================================
;; Neural Net

(def INPUT-SIZE (+ (* GW GH)))
(def SYNTH-SIZE 40)
(def OUTPUT-SIZE 4)
(def TRAFFIC_FACTOR 0.01)

(defn norm-quant ^double [^double x]
  (Math/log10 (+ 1.0 x)))

(defn feature-vector [data pos]
  (let [pos (int pos)
        n (int INPUT-SIZE)
        ^Vector v (Vector/createLength n)]
    (.set v (.asVector ^AMatrix (data pos)))
    (dotimes [i n]
      (let [i (int i)]
        (.set v i (norm-quant (.get v i))))) 
    v))

(defn result-vector [pdata pos]
  (let [pos (int pos)
        n (int OUTPUT-SIZE)
        ^Vector v (Vector/createLength n)]
    (dotimes [i n]
      (.set v (int i) (double (* TRAFFIC_FACTOR 0.25 (+
                                                  (pdata (+ pos i))
                                                  (pdata (+ pos i 1))
                                                  (pdata (+ pos i 2))
                                                  (pdata (+ pos i 3)))))))
    v))

(def up
    (neural-network :inputs INPUT-SIZE  
                    :max-links SYNTH-SIZE
                    :output-op Ops/TANH
                    :outputs SYNTH-SIZE
                    :layers 1))

(def down
    (neural-network :inputs SYNTH-SIZE  
                    :max-links INPUT-SIZE
                    :output-op Ops/LINEAR
                    :outputs INPUT-SIZE
                    :layers 1))

(def synth (stack up down))

(def rec
    (neural-network :inputs SYNTH-SIZE  
                    :max-links SYNTH-SIZE
                    :hidden-op Ops/TANH
                    :output-op Ops/LINEAR
                    :outputs OUTPUT-SIZE
                    :layers 1))

(def net (stack up rec)) 

(defn train [n]
  (dotimes [i n]
    (let [^IComponent net net
          data data
          pos (rand-int PERIODS)
          ^AVector input (feature-vector data pos)
          ^AVector target (result-vector @pdata pos)]
      (.train net 
        ^AVector input
        ^AVector target 
        ^nuroko.module.loss.LossFunction nuroko.module.loss.SquaredErrorLoss/INSTANCE 
        (double 1.0))
      (when (== 0 (mod i 100)) 
        (.addMultiple (.getParameters net) (.getGradient net) 0.0001)
        (.fill (.getGradient net) 0.0)
        (println i))))) 

;; =====================================================
;; Demo Code

(def running (atom true))

(defn demo []
  (load-pdata "C:/Users/Mike/Desktop/Buuuk_Traffic_Transformed_Data.csv")
  (show (xy-chart 
          (scale 0.25 (vec (range PERIODS)))
          @pdata) :title "Congestion for 2013-05-14")
  
  (load-data "C:/Users/Mike/Desktop/singtel-call_2012-05-14.csv")
  (show (im/zoom 8 (city-image (data 30))))
  
  (train 100000) 
  (think net (feature-vector data 10))
  (show (xy-chart-multiline 
          (scale  0.25 (vec (range PERIODS)))
          [[0 0]
           (scale 100 (vec (for [i (range PERIODS)] (.get ^AVector (think net (feature-vector data i)) 0))))]) :title "Predicted congestion")
  
  (show (vector-bars (array :vectorz (for [i (range PERIODS)] (esum (mul 0.25 ^AVector (think net (feature-vector data i)) 0))))))

  (while @running 
    (dotimes [i PERIODS] 
      (Thread/sleep 50)
      ;;(show (vector-bars (think net (feature-vector data i))) :title "Congestion Prediction")
      (show (im/zoom 10 (city-image (data i))) :title "Mobile Activity")))
  
  (show (xy-chart-multiline 
         (scale  0.25 (vec (range PERIODS)))
         [(take PERIODS (drop 2 @pdata))
         (scale 100 (vec (for [i (range PERIODS)] (.get ^AVector (think net (feature-vector data i)) 0))))]) :title "Predicted vs actual congestion")
  
  )
