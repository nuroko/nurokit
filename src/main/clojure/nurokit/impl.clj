(ns nurokit.impl
  "Implementation namespace for common default models"
  (:require [nurokit.protocols :as np])
  (:require [mikera.cljutils.error :refer [error]]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

;; A model that implements a simple direct mapping of values. Inputs can be mapped
;; to any non-null output values
;;
;; may provide optional fields:
;;  - a :create-fn that produces a default output given new input
(defrecord MapModel [value-map]
  np/PObject
    (clean [o]
     (-> o
       (dissoc :output)
       (dissoc :input)))
    (copy [o]
      (MapModel. value-map))
  np/PModel
    (think [m input]
      (let [recognised? (contains? value-map input)
            m (if recognised? 
                m 
                (assoc-in m [:value-map input] 
                        ((or (:create-fn m) 
                             (error "No :create-fn defined, for input " input))
                          input)))
            result ((:value-map m) input)]
        (when (nil? result) (error "No output value for input: " input))
        (-> m
          (assoc :output result)
          (assoc :input input))))
    (output [m]
      (let [result (:output m)] 
        (when (nil? result) (error "Output not calculated in MapModel" m))
        result))
    )

