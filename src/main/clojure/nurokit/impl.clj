(ns nurokit.impl
  "Implementation namespace for common default models"
  (:require [nurokit.protocols :as np]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

;; A model that implements a simple direct mapping of values
(defrecord MapModel [value-map]
  np/PObject
    (clean [o]
     (dissoc o :output))
    (copy [o]
      (MapModel. value-map))
  np/PModel
    (think [m input]
      (assoc m :output (value-map input)))
    (output [m]
      (or (:output m) (throw (Error. "Output not calculated"))))
    )

