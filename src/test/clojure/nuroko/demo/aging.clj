(ns nuroko.demo.aging
  (:use [nuroko.lab core charts])
  (:use [nuroko.gui visual])
  (:use [clojure.repl])
  (:use [clojure.core.matrix])
  (:require [task.core :as task])
  (:require [server.socket :as ss])
  (:require [clojure.data.csv :as csv]) 
  (:require [mikera.cljutils.mouse :as ms])
  (:require [clojure.java.io :as io])
  (:import [java.io InputStream OutputStream DataInputStream DataOutputStream])
  (:import [mikera.vectorz Op Ops])
  (:import [mikera.vectorz.ops ScaledLogistic Logistic Tanh])
  (:import [nuroko.coders CharCoder])
  (:import [mikera.vectorz AVector Vectorz]))

(set-current-implementation :vectorz)

(def ROW-LENGTH 5)
(def DISPLAY-LENGTH 100)
(def PORT 9000)

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

(defn norm 
  "Normalise byte to -1,1 range"
  ([x]
    (let [b (byte x)]
      (- (* (/ 1.0 128.0) b) 1.0))))

(defn prob 
  "Normalise probability to -1,1 range"
  ([x]
    (double (max 0.0 (min 1.0 (double x))))))

(defn ^long pint [^String s]
  (long (Integer/parseInt s)))

(defn load-data []
  (reset! TDATA [])
  (with-open [in-file (io/reader (io/resource "temp/medtronic-carelink-export.csv"))]
    (let [data (csv/read-csv in-file)]
      (doseq [r data]
        (swap! TDATA
               (fn [old] (conj old (row (norm (pint (nth r 0))) 
                                        (norm (pint (nth r 1))) 
                                        (norm (pint (nth r 2))) 
                                        (prob (pint (nth r 3))))))))))) 

;; (load-data) 
(reset)

(defn show-row [row]
  (append-data row)
  (show (data-chart @DATA) :title "Mouse"))

(defn read-byte [^DataInputStream dis]
  (norm (byte (.read dis))))

(defn read-row [^DataInputStream dis]
  (row (read-byte dis) (read-byte dis) (read-byte dis) 0 0))

(defn server-fn [^InputStream input-stream ^OutputStream output-stream]
  (let [dis (DataInputStream. input-stream)
        dos (DataOutputStream. output-stream)]
    (try
      (loop []
        (let [row (read-row dis)]
          (.writeByte dos (unchecked-byte 13))
          (show-row row))
        (recur))
      (catch Throwable t (println (str t))))))

(defonce server (atom nil))
(defn start []
  (swap! server
         (fn [old] 
           (when old (ss/close-server old))
           (ss/create-server PORT server-fn))))
(start) 

;; =================== DEMO CODE FOLLOWS =====================

(defn demo []
  (reset)
  
  (show (data-chart @TDATA 0 1000))
  
  (dotimes [i 100]
     (append-data (rand-row))
     (show (data-chart @DATA)))
  
  (task/run 
    {:sleep 40 :repeat 1000} ;; sleep used to slow 
    (do 
      (append-data (row (ms/mouse-x) (ms/mouse-y) (* 1000 (+ 1 (Math/sin (* (System/currentTimeMillis) 0.001))))))
      (show (data-chart @DATA) :title "Mouse")))
  
  (task/stop-all)


)