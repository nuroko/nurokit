(ns nurokit.protocols
  "Namespace of protocols for Nurokit objects. Implementations may extend these to participate
  in Nurokit model structures.")

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defprotocol PObject
  "Protocol for the Nurokit object lifecycle."
  (copy [o]
     "Clones an object. Ensures copies are made of any mutable sub-components")
  (clean [o]
     "Cleans an object's state. Output attributes e.g. output values are cleared"))

(defprotocol PModel
  "A model is an abstraction for objects that can take an input, and return an output"
  (think [m input]
     "Runs the model computation on the provided input. Returns a new model with the output value available")
  (output [m]
     "Gets the output for a model. Throws an exception if the model has not been run."))