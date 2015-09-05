(ns nurokit.optimise
  "Namespace for optimisation algorithms, based around ADADELTA gradient descent"
  (:require [nurokit.protocols :as np])
  (:require [clojure.core.matrix :as m])
  (:require [mikera.cljutils.error :refer [error]]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defrecord AdaDelta [msgrad  ;; mean squared gradient
                     msdx    ;; mean squared delta update
                     dx      ;; latest delta update
                     ])

(defn adadelta 
  "Constructs a new AdaDelta optimiser of the given size (parameter length)"
  ([size]
    (let [msgrad (m/mutable (m/new-vector size))
          msdx (m/mutable (m/new-vector size))
          dx (m/mutable (m/new-vector size))]
      (m/assign! msgrad 1.0)
      (m/assign! msdx 1.0)
      (m/assign! dx 0.0)
      (AdaDelta. msgrad msdx dx))))

(defn update-parameters
  "Updates parameters using the given gradient.

   Rerturns the updated adadelta object"
  ([adadelta gradient parameters]
    (let [learn-rate (double (or (:learn-rate adadelta) 1.0))
          decay-rate (double (or (:decay-rate adadelta) 0.05))
          epsilon (double (or (:epsilon adadelta) 0.000001))
          msgrad (:msgrad adadelta)
          msdx (:msdx adadelta)
          dx (:msdx adadelta)]
      
      ;; apply decay rate to the previous mean squared gradient
      (m/mul! msgrad (- 1.0 decay-rate))
      ;; accumulate the latest gradient
      (m/add-scaled-product! msgrad gradient gradient decay-rate)
      
      ;; compute the parameter update
      (m/assign! dx msdx)
      (m/div! dx msgrad)
      (m/sqrt! dx)
      (m/mul! dx gradient)
      
      ;; apply decay rate to the previous mean squared update
      (m/mul! msdx (- 1.0 decay-rate))
      ;; accumulate the latest update
      (m/add-scaled-product! msdx dx dx decay-rate)
      
      (m/add-scaled! parameters dx (* -1.0 learn-rate))
      
      ;; return the updated adadelta record. Mutable gradients have been updated
      adadelta)))