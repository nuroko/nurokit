(ns nuroko.demo.city
  (:use [nuroko.lab core charts])
  (:use [nuroko.gui visual])
  (:use [clojure.repl])
  (:use [clojure.core.matrix])
  (:require [mikera.cljutils.text :as text])
  (:require [mikera.image-matrix :as imm])
  (:require [task.core :as task])
  (:require [clj-time.core :as time] )
  (:require [server.socket :as ss])
  (:require [clojure.data.csv :as csv]) 
  (:require [mikera.cljutils.mouse :as ms])
  (:require [clojure.java.io :as io])
  (:import [java.io InputStream OutputStream DataInputStream DataOutputStream])
  (:import [mikera.vectorz Op Ops])
  (:import [mikera.vectorz.ops ScaledLogistic Logistic Tanh])
  (:import [nuroko.coders CharCoder])
  (:import [nuroko.core IComponent])
  (:import [mikera.vectorz Vector AVector Vectorz]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(set-current-implementation :vectorz)

(def starttime (time/date-time 2012 05 13 0 0 00 000))

;; time period in mins
(def PERIOD 15)

;; total number of periods
(def PERIODS (* 7 24 (/ 60 PERIOD)))

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

(def mmap (new-matrix GH GW))

(defn process-rec [ts lat long]
  (let [x (long2g long)
        y (lat2g lat)]
    (println (str ts ": " x " , " y))))

(defn load-data [fname len]
  (with-open [rdr (clojure.java.io/reader fname)]
    (let [lines (take len (line-seq rdr))]
      (loop [lines (seq lines)]
        (when lines
          (let [line (first lines)
                flds (first (csv/read-csv line))
                ts (parse-tstamp (flds 0) (flds 3))
                lat (pd (flds 6))
                long (pd (flds 7))]
            (process-rec ts lat long))
          (recur (next lines)))))))