(ns nurokit.protocols)

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defprotocol PModel
  "A model is an abstraction for objects that can take an input, and return an output"
  (think [m input]
     "Runs the model computation on the provided input. Returns a new model with the output value available")
  (output [m]
     "Gets the output for a model. Throws an exception if the model has not been run."))