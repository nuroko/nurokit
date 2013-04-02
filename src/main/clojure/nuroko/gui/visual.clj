(ns nuroko.gui.visual
  (:import [javax.swing JComponent JLabel JPanel])
  (:import [java.awt Graphics2D Color GridLayout])
  (:import [java.awt.event ActionEvent ActionListener])
  (:import [java.awt.image BufferedImage])
  (:import [nuroko.module ALayerStack AWeightLayer])
  (:import [mikera.gui Frames JIcon])
  (:import [mikera.util Maths])
  (:import [mikera.vectorz AVector Vectorz])
  (:import [org.jfree.chart ChartPanel JFreeChart])
  (:require [incanter core charts])
  (:require [mc image])
  (:use [nuroko.lab core]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(declare component)
(declare grid)

;; Colour functions

(defmacro clamp-colour-value [val]
  `(let [v# (float ~val)]
     (Math/min (float 1.0) (Math/max (float 0.0) v#))))

(defn weight-colour 
  ([^double weight]
    (Color. 
      (clamp-colour-value (Math/tanh (- weight)))
      (clamp-colour-value (Math/tanh weight))
      0.0)))

(defn weight-colour-rgb 
  (^long [^double weight]
    (mc.image/rgb 
      (clamp-colour-value (Math/tanh (- weight)))
      (clamp-colour-value (Math/tanh weight))
      0.0)))

(defn weight-colour-mono 
  (^long [^double weight]
    (let [v (Maths/sigmoid (double weight))]
      (mc.image/rgb v v v))))

(defn mono-rgb 
  (^long [^double cv]
    (mc.image/rgb cv cv cv)))


(defn sigmoid-rgb 
  (^long [^double cv]
    (let [s (Maths/sigmoid cv)]
      (mc.image/rgb s s s))))

(defn activation-colour 
  ([^double x]
    (Color. 
      (clamp-colour-value x) 
      (clamp-colour-value (Math/abs x)) 
      (clamp-colour-value (- x)))))


;; image builder

(defn image-generator 
  "Creates an image generator that builds images from vector data"
  ([& {:keys [offset width height skip colour-function] 
       :or {offset 0}}]
    (let [width (int width)
          height (int height)
          offset (int offset)
          skip (int (or skip width))
          colour-function (or colour-function mono-rgb) 
          size (* width height)]
      (fn [v]
		      (let [^AVector v (if (instance? AVector v) v (nuroko.lab.core/avector v))
                ^BufferedImage bi (mc.image/buffered-image width height)
                ^ints data (int-array size)]
	         (dotimes [y height]
	           (let [yo (int (* y width))
                   vo (int (* y skip))] 
              (dotimes [x width]
                (aset data (+ x yo) (let [cv (.get ^AVector v (int (+ vo x)))] 
                                      (int (colour-function cv)))))))
	         (.setDataElements (.getRaster bi) (int 0) (int 0) width height data)
	         bi)))))


(defn label 
  "Creates a JLabel with the given content"
  (^JLabel [s]
    (let [^String s (str s)
          label (JLabel. s JLabel/CENTER)]
      (.setToolTipText label s)
      label)))


(defn component 
  "Creates a component as appropriate to visualise an object x" 
  (^JComponent [x]
    (cond 
      (instance? JComponent x) x
      (instance? BufferedImage x) (JIcon. ^BufferedImage x)
	    (instance? JFreeChart x) (ChartPanel. ^JFreeChart x)
      (sequential? x) (grid (seq x))
      :else (label x))))

(defn grid [things]
  (let [n (count things)
        size (int (Math/ceil (Math/sqrt n)))
        grid-layout (GridLayout. 0 size)
        grid (JPanel.)]
    (.setLayout grid grid-layout)
    (doseq [x things]
      (.add grid (component x)))
    grid))


(defn show 
  "Shows a component in a new frame"
  ([com 
    & {:keys [^String title]
       :as options
       :or {title nil}}]
  (let [com (component com)]
    (Frames/display com title))))


(defn default-dimensions
  "Returns the default dimensions for a new frame"
  (^java.awt.Dimension []
    (java.awt.Dimension. 400 300)))


(defn network-graph
  ([^ALayerStack nn
    & {:keys [border repaint-speed activation-size line-width] 
       :or {border 20
            repaint-speed 50
            line-width 1
            activation-size 5}}]
    (let [graph (proxy [javax.swing.JComponent java.awt.event.ActionListener] []
        (actionPerformed [^ActionEvent e]
          (.repaint ^JComponent this))
        (paintComponent [^Graphics2D g] 
          (let [border (double border)
                this ^JComponent this
                width (double (.getWidth this))
                height (double (.getHeight this))
                layers (.getLayerCount nn)
                max-size (max (input-length nn) (reduce max (map #(.getOutputLength ^AWeightLayer %) (.getLayers nn))))
                step (/ (double width) max-size)
                as (double activation-size)]
            (.setColor g (Color/BLACK))
            (.fillRect g 0 0 width height)
            (.setStroke g (java.awt.BasicStroke. (float line-width))) 
            (dotimes [i layers]
              (let [layer (.getLayer nn i)
                    layer-inputs (.getInputLength layer)
                    layer-outputs (.getOutputLength layer)
                    sy (int (+ border (* (- height (* 2 border)) (/ (- layers 0.0 i) layers))))
                    ty (int (+ border (* (- height (* 2 border)) (/ (- layers 1.0 i) layers))))
                    soffset (double border)
                    toffset (double border)
                    sskip (double (/ (- width (* 2 border)) (max 1.0 (dec layer-inputs))))
                    tskip (double (/ (- width (* 2 border)) (max 1.0 (dec layer-outputs))))]
                (dotimes [y layer-outputs]
                  (let [link-count (.getLinkCount layer y)
                        tx (int (+ toffset (* tskip y)))]
                    (dotimes [ii link-count] 
	                    (let [x (.getLinkSource layer y ii)
                            sx (int (+ soffset (* sskip x)))
                            ii (int ii)]
	                      (.setColor g ^Color (weight-colour (double (.getLinkWeight layer y ii))))
	                      (.drawLine g sx sy tx ty)))))))
            (dotimes [i (inc layers)]
              (let [data ^AVector (.getData nn i) 
                    len (.length data) 
                    ty (int (+ border (* (- height (* 2 border)) (/ (- layers i) layers))))
                    toffset (double border)
                    tskip (double (/ (- width (* 2 border)) (max 1.0 (dec len))))]
                (dotimes [y len]
                  (let [activation (.get data y)
                        tx (int (+ toffset (* tskip y)))]
                    (.setColor g ^Color (activation-colour activation))
                    (.fillRect g (- tx as) (- ty as) (* 2 as) (* 2 as))
                    (.setColor g Color/GRAY)
                    (.drawRect g (- tx as) (- ty as) (* 2 as) (* 2 as)))))))))
          timer (javax.swing.Timer. (int repaint-speed) graph)]
      (.start timer)
      (.setPreferredSize graph (default-dimensions))
      graph)))

(defn xy-chart ^JFreeChart [xs ys]
  (let [chart (incanter.charts/xy-plot xs ys)]
    ; (incanter.charts/set-y-range chart 0.0 1.0)
    chart))

(defn xy-chart-multiline ^JFreeChart [xs yss]
  (let [chart (xy-chart xs (first yss))]
    (doseq [ys (rest yss)] 
      (incanter.charts/add-lines chart xs ys)) 
    chart))

(defn time-chart 
  "Creates a continously updating time chart of one or more calculations, which should be functions with zero arguments."
  ([calcs
    & {:keys [repaint-speed time-periods y-min y-max] 
       :or {repaint-speed 250
            time-periods 240}}]
    (let [line-count (count calcs)
          start-millis (System/currentTimeMillis)
          times (atom '())
          values (atom (repeat line-count '()))
          next-chart  (fn []
                       (let [time (/ (- (System/currentTimeMillis) start-millis) 1000.0)]
                         (swap! times #(take time-periods (cons time %)))
                         (swap! values #(for [[calc ss] (map vector calcs %)]
                                         (take time-periods (cons (calc) ss))))
                         (let [chart (xy-chart-multiline @times @values)]
                           (if y-max (incanter.charts/set-y-range chart (double (or y-min 0.0)) (double y-max)))
                           chart)))
          panel (ChartPanel. ^JFreeChart (next-chart)) 
          timer (javax.swing.Timer. 
                  (int repaint-speed) 
                  (proxy [java.awt.event.ActionListener] []
                    (actionPerformed 
                      [^ActionEvent e]
                      (when (.isShowing panel) 
                        (.setChart panel ^JFreeChart (next-chart))
                        (.repaint ^JComponent panel)) )))]
      (.start timer)
      (.setPreferredSize panel (default-dimensions))
      panel)))


(defn scatter-outputs 
  "Shows a catter graph of 2 output values."
  ([data
    & {:keys [labels x-index y-index] 
       :or {x-index 0
            y-index 1}}]
    (let [res (map (fn [^mikera.vectorz.AVector v] [(.get v (int x-index)) (.get v (int y-index))]) data)
          xs (map first res)
          ys (map second res)
          scatter-chart (incanter.charts/scatter-plot xs ys :group-by labels)
          panel (ChartPanel. ^JFreeChart scatter-chart)]
     panel)))



(defn layer-feature-calc ^AVector [^nuroko.module.AWeightLayer wl ^AVector out-weights]
  (let [ol (output-length wl)
        in-weights (Vectorz/newVector (input-length wl))]
    (dotimes [i ol]
      (let [i (int i)
            y (.get out-weights i)]
        (.addMultiple in-weights (.getSourceWeights wl i) (.getSourceIndex wl i) y)))
    in-weights))

(defn stack-feature-calc ^AVector [^nuroko.module.AThinkStack stack ^AVector out-weights]
  (loop [i (dec (.getLayerCount stack))
         output out-weights]
    (if (< i 0)
      output
      (recur (dec i) (layer-feature-calc (.getLayer stack i) output)))))

(defn feature-maps
  "Returns feature map vecttps for an AThinkStack"
  ([^nuroko.module.AThinkStack stack
    & {:keys [scale] 
       :or {scale 1.0}}]
    (let [ol (output-length stack)
          scale (double scale)]
      (for [i (range ol)]
        (let [top-vector (Vectorz/axisVector (int i) ol)
              result (stack-feature-calc stack top-vector)]
          (.multiply result scale)
          result)))))

(defn spatio-map 
  ([^nuroko.module.AThinkStack stack
    examples]
    (let [stack (.clone stack)
          ig (image-generator :width (output-length stack)
                              :height (count examples)) 
          ]
      (ig (Vectorz/join ^java.util.List (vec (map (partial think stack) examples)))))))

(defn feature)

;; DEMO CODE

(fn []
  (def nn (neural-network :inputs 10 :outputs 3))
  (show (network-graph nn))
  
  (show (nuroko.gui.visual/time-chart [#(Math/random)]))
  )