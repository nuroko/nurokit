(ns nuroko.demo.healthup
  (:use [nuroko.lab core charts])
  (:use [nuroko.gui visual])
  (:use [incanter core io charts stats]) 
  (:require [clojure.core.matrix :as m]) 
  (:require [mikera.cljutils [vectors :refer [vector-without]]]) 
  (:require [mikera.vectorz.core :as v]) 
  (:require [mikera.vectorz.matrix-api]) 
  (:require [clojure.java.io :as io])
  (:require [task.core :as task]))

