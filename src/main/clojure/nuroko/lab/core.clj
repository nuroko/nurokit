(ns nuroko.lab.core
  (:use [clojure.core.matrix])
  (:import [nuroko.core NurokoException IParameterised ITrainable Components])  
  (:import [nuroko.module AWeightLayer NeuralNet])
  (:require [mikera.vectorz.matrix-api]) 
  (:import [mikera.vectorz Op Ops AVector Vectorz]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(set-current-implementation :vectorz)

(defmacro error [& msgs]
  `(throw (NurokoException. (str ~@msgs))))

;; ==================================
;; Thinkers and main modules

(defprotocol PInputOutput
  (input-length [thinker])
  (output-length [thinker]))


(extend-protocol PInputOutput
  nuroko.core.IThinker
    (input-length [thinker]
      (.getInputLength thinker))
    (output-length [thinker]
      (.getOutputLength thinker))
  nuroko.core.ITask
    (input-length [task]
      (.getInputLength task))
    (output-length [task]
      (.getOutputLength task))
  nuroko.module.AWeightLayer
    (input-length [wl]
      (.getInputLength wl))
    (output-length [wl]
      (.getOutputLength wl)))

(defprotocol PThinker
  (think-impl [thinker input output]))

(extend-protocol PThinker
  nuroko.core.IThinker
    (think-impl [thinker ^AVector input ^AVector output]
      (.think thinker input output)))

(defn think 
  ([thinker input]
    (let [output (Vectorz/newVector (output-length thinker))]
      (think thinker input output)
      output))
  ([thinker input output]
    (think-impl thinker input output)
    output)) 

(defprotocol PParameterised
  (get-parameters [thinker])
  (get-gradient [thinker])
  (parameter-length [thinker]))

(extend-protocol PParameterised
  nuroko.core.IParameterised
    (parameter-length [thinker]
      (.getParameterLength thinker))
    (get-parameters [thinker]
      (.getParameters thinker))
    (get-gradient [thinker]
      (.getGradient thinker)))

(defn parameters
  "Returns the parameter vector for a given thinker"
  (^AVector [thinker]
    (get-parameters thinker))
  (^AVector [thinker ^AVector params-out]
    (let [v (Vectorz/newVector (parameter-length thinker))]
      (.set v (parameters thinker))
      v)))

(defprotocol PSynthesiser
  (regen-impl [thinker input output]))

(extend-protocol PSynthesiser
  nuroko.core.ISynthesiser
    (regen-impl [thinker ^AVector input ^AVector output]
      (.generate thinker input output)))

(defn generate 
  ([thinker output]
    (let [regen (Vectorz/newVector (input-length thinker))]
      (think thinker regen output)
      regen))
  ([thinker input output]
    (regen-impl thinker input output)))

;; ===========================================
;; task

(defprotocol PTask
  (task-input-length [task])
  (task-output-length [task])
  (reset-state [task])
  (get-input-impl [task input-vector])
  (get-evaluation [task input-vector output-vector])
  (get-target [task input-vector target-vector]))

(extend-protocol PTask
  nuroko.core.ITask
    (task-input-length [task]
      (.getInputLength task))
    (task-output-length [task]
      (.getOutputLength task))
    (reset-state [task]
      (.reset task))
    (get-input-impl [task input-vector]
      (.getInput task ^AVector input-vector))
    (get-evaluation [task input-vector output-vector]
      (.getEvaluation task ^AVector input-vector ^AVector output-vector))
    (get-target [task input-vector target-vector]
      (.getTarget task ^AVector input-vector ^AVector target-vector)))

(defn get-input 
  ([task]
    (let [isize (task-input-length task)
          v (Vectorz/newVector isize)]
      (get-input task v)
      v))
  ([task ^AVector v]
    (get-input-impl task v))
  ([^nuroko.task.ExampleTask task ^AVector v i]
    (if (instance? nuroko.task.ExampleTask task)
      (.getInput task v i)
      (error "Getting indexed input only works on ExampleTask"))))

;; ===========================================
;; training session

(defprotocol PAlgorithm
  (run-batch [session]))

;; ===========================================
;; Encoders and decoders


(defprotocol PDecoder
  (decode-impl [coder ^AVector dest-vector])
  (decode-length [coder]))

(extend-protocol PDecoder
  nuroko.core.IDecoder
    (decode-impl [coder ^AVector src-vector]
      (.decode coder src-vector 0))
    (decode-length [coder]
      (.codeLength coder)))

(defprotocol PEncoder
  (encode-impl [encoder source-object ^AVector dest-vector])
  (encode-length [encoder]))

(extend-protocol PEncoder
  nuroko.core.IEncoder
    (encode-impl [encoder source-object ^AVector dest-vector]
      (.encode encoder source-object dest-vector 0))
    (encode-length [encoder]
       (.codeLength encoder)))

(defn encode
  ([encoder source]
    (let [len (encode-length encoder)
          dest-vector (Vectorz/newVector len)]
      (encode encoder source dest-vector)
      dest-vector))
  ([encoder source ^AVector dest-vector]
    (encode-impl encoder source dest-vector)))

(defn decode
  ([coder source]
    (decode-impl coder source)))

;; ===================================================
;; Coder constructors

(defn ^nuroko.coders.FixedLongCoder int-coder
  ([& {:keys [bits] 
       :or {}
       :as options}]
    (if bits 
      (nuroko.coders.FixedLongCoder. bits)
      (error "Invaid input: " options))))

(defn ^nuroko.coders.IdentityCoder vector-coder
  ([size]
    (nuroko.coders.IdentityCoder. size)))

(defn ^nuroko.coders.ChoiceCoder class-coder
  ([& {:keys [values] 
       :or {}
       :as options}]
    (if (seq values) 
      (nuroko.coders.ChoiceCoder. ^java.util.Collection (into [] values))
      (error "Invaid input: " options))))

(defn default-coder
  "Create a default coder for a set of possible values"
  ([values]
    (class-coder :values values)))

;; ==================================================
;; Task constructors

(defn example-task [inputs outputs]
  (nuroko.task.ExampleTask. (vec inputs) (vec outputs)))

(defn identity-task
  "Creates a task that trains an identity function over the given input values"
  ([vectors]
    (example-task
      vectors
      vectors)))

(defn mapping-task
  "Creates a task from an input-output map with given coders"
  ([data-map
    & {:keys [input-coder output-coder] 
       :or {input-coder nil
            output-coder nil}
       :as options}]
    (example-task
      (map #(if input-coder (encode input-coder %) %) (keys data-map))
      (map #(if output-coder (encode output-coder %) %) (vals data-map)))))
    

;; ===================================================
;; Object constructors

(def DOUBLE-CLASS (Class/forName "[D"))

(defn avector
  "Converts x to an AVector instance. Works on doubles arrays and any collection."
  (^AVector [x]
    (cond 
      (instance? AVector x) (Vectorz/create ^AVector x)
      (instance? DOUBLE-CLASS x) (Vectorz/create ^doubles x)
      (vector? x) (Vectorz/create ^java.util.List x)
      :else (Vectorz/create ^java.util.List (vec x)))))

(defn weight-layer
  "Creates a weight layer for a neural network"
  (^nuroko.module.AWeightLayer [& {:keys [inputs outputs max-links] 
                                  :or {max-links Long/MAX_VALUE}}]
    (if-not outputs (error "Needs :outputs parameter (number of output values)"))
    (Components/weightLayer (int inputs) (int outputs) (int max-links))))


(defn neural-network 
  "Creates a standard neural network"
  (^nuroko.module.NeuralNet [& {:keys [inputs outputs layers max-links hidden-op output-op hidden-sizes] 
                                  :as options
                                  :or {layers 3
                                       output-op Ops/LOGISTIC
                                       hidden-op Ops/TANH}}]
 ;;   (println (str "NN: " options)) 
    (if-not inputs (error "No :inputs length specified!")) 
    (if-not outputs (error "No :outputs length specified!")) 
    (let [sizes (vec (concat [inputs]
                             (or hidden-sizes 
                                 (repeat (- layers 1) (max inputs outputs)))
                             [outputs]))
          layers (int (dec (count sizes)))
          layer-array ^"[Lnuroko.module.AWeightLayer;" (make-array AWeightLayer layers)]
      (dotimes [i layers]
        (let [] 
          (aset layer-array (int i) 
                (weight-layer :inputs (sizes i) 
                              :outputs (sizes (inc i)) 
                              :max-links (or max-links (sizes i))))))
      (let [nn (NeuralNet. layer-array hidden-op output-op)]
        (.initRandom nn)
        nn))))

;; ===========================================
;; Compositions

(defn stack 
  "Connects networks together sequentially (data flow from left to right)"
  ([& nets]
    (Components/stack ^java.util.List (vec nets))))

(def connect stack) 

;; ===========================================
;; Weight update algorithms


(defn backprop-updater
  "Creates a training algorithm that improves a neural network for the given task"
  ([^nuroko.core.IParameterised network
    & {:keys [momentum-factor learn-rate] 
       :or {momentum-factor 0.95
            learn-rate 0.0001}}]
    (let [parameter-length (.getParameterLength ^IParameterised network)
          ^AVector last-update (Vectorz/newVector parameter-length)
          momentum-factor (double momentum-factor)]
      (fn [^nuroko.core.IParameterised network]
        (let [^AVector gradient (get-gradient network)
              ^AVector parameters (get-parameters network)] 
	        (.multiply last-update momentum-factor)
	        (.addMultiple last-update gradient (* 1.0 learn-rate))
	        (.add parameters last-update)
	        
	        (.fill gradient 0.0))))))

(defn rmsprop-updater
  "Creates a trainer session that improves a neural network for the given task"
  ([^nuroko.core.IParameterised network
    & {:keys [momentum-factor learn-rate max-rms-factor rms-decay] 
       :or {momentum-factor 0.9
            learn-rate 0.0001
            max-rms-factor 20.0
            rms-decay 0.95}}]
    (let [parameter-length (.getParameterLength ^IParameterised network)
          last-update (Vectorz/newVector parameter-length)
          rms-total (Vectorz/newVector parameter-length)
          rms-decay (double rms-decay)
          temp (Vectorz/newVector parameter-length)
          momentum-factor (double momentum-factor)]
      (fn [^nuroko.core.IParameterised network]
        (let [^AVector gradient (get-gradient network)
              ^AVector parameters (get-parameters network)] 
          (.multiply rms-total rms-decay)
	        (.set temp gradient) 
	        
	        (.multiply temp temp) 
	        (.addMultiple rms-total temp (- 1.0 rms-decay)) 
	        (.set temp rms-total) 
	        (Vectorz/invSqrt temp) 
	        (.clampMax temp (double max-rms-factor)) 
	 
	        (.multiply last-update momentum-factor)
	        (.addProduct last-update gradient temp (* learn-rate 1.0))
	        (.add parameters last-update)
	        
	        (.fill gradient 0.0))))))

;; ===========================================
;; Trainer functions

(defn supervised-trainer 
    [^nuroko.core.ITrainable network task
     & {:keys [batch-size updater] 
        :or {batch-size 10}}]
    (let [input-length (input-length network)
          output-length (output-length network)
          updater (or updater (rmsprop-updater network)) 
          input (Vectorz/newVector input-length)
          target (Vectorz/newVector output-length)] 
      (fn [^nuroko.core.ITrainable network]
        (dotimes [i (long batch-size)]
          (get-input task input)
          (get-target task input target)
          (.train network input target))
        (updater network))))

;; ===========================================
;; Evaluation functions


(defn evaluate 
  "Evaluates a network gainst a given task"
  ([^nuroko.core.ITask task ^nuroko.core.IThinker network
    & {:keys [iterations] 
       :or {iterations 100}}]
    (let [network (.clone network)
          task (.clone task)
          result (atom 0.0)
          input-length (int (task-input-length task))
          output-length (int (task-output-length task))
          input ^AVector (Vectorz/newVector input-length)
          output ^AVector (Vectorz/newVector output-length)
          target ^AVector (Vectorz/newVector output-length)]
      (dotimes [i iterations]
        (get-input task input)
        (get-target task input target)
        (think network input output)
        (let [psum (Vectorz/totalValue output)
              correct-chance (/ (.dotProduct output target) psum)]
          (swap! result + correct-chance)))
      (- 1.0 (/ (double @result) iterations)))))

(defn evaluate-classifier 
  "Evaluates a network gainst a given task"
  ([^nuroko.core.ITask task ^nuroko.core.IThinker network
    & {:keys [iterations] 
       :or {iterations 100}}]
    (let [network (.clone network)
          task (.clone task)
          result (atom 0.0)
          input-length (int (task-input-length task))
          output-length (int (task-output-length task))
          input ^AVector (Vectorz/newVector input-length)
          output ^AVector (Vectorz/newVector output-length)
          target ^AVector (Vectorz/newVector output-length)]
      (dotimes [i iterations]
        (get-input task input i)
        (get-target task input target)
        (think network input output)
        (let [class-index (Vectorz/indexOfMaxValue output) 
              correctness (.get target class-index)]
          (swap! result + correctness)))
      (- 1.0 (/ (double @result) iterations)))))
