(ns perlin-flow.core
  (:require [quil.core :as q :include-macros true]
            [active.clojure.cljs.record :as rec :include-macros true]
            [quil.middleware :as m]))



(rec/define-record-type Color
  make-color
  color?
  [red color-red
   green color-green
   blue color-blue
   alpha color-alpha])

(def colors [(make-color 255 73 71 5)])

(rec/define-record-type Particle
  make-particle
  particle?
  [id particle-id
   velocity-x particle-velocity-x
   velocity-y particle-velocity-y
   direction particle-direction
   x particle-x
   y particle-y
   color particle-color])



(defn init-particle [id initial-x initial-y color]
  (make-particle
   id
   1
   1
   0
   initial-x
   initial-y
   color))

(def particle-width 3)

(defn move-particle [height width particle]
  (let [new-x (mod (+ (particle-x particle)
                      (particle-velocity-x particle)) width)
        new-y (mod (+ (particle-y particle)
                      (particle-velocity-y particle)) height)]
    (-> particle
        (particle-x new-x)
        (particle-y new-y))))

(let [noise-scaling 0.002]

  (defn scale [v]
    (* noise-scaling v)))

(defn noise [x y]
  (q/noise (scale x) (scale y)))


(defn update-direction [particle]
  (let [direction (* 2 Math/PI (noise (particle-x particle) (particle-y particle)))]
    (particle-direction particle direction)))


(defn update-velocity [particle]
  (let [velocity-x (/ (+ (particle-velocity-x particle)
                         (Math/cos (particle-direction particle))) 2)
        velocity-y (/ (+ (particle-velocity-y particle)
                         (Math/sin (particle-direction particle))) 2)]
    (-> particle
        (particle-velocity-x velocity-x)
        (particle-velocity-y velocity-y))))


(defn setup [height width]
  (q/frame-rate 300)
  (q/color-mode :rgb)
  (q/background 127 10 32)
  (q/no-stroke)
  (map (fn [idx]
         (init-particle idx (q/random width) (q/random height) (rand-nth colors)))
       (range 0
              ; magic to derive num particles
              (/ (* height width) 400))))

(defn update-state [height width particles]
  (->> particles
       (map (partial move-particle height width))
       (map update-velocity)
       (map update-direction)
       ))

(defn draw-state [particles]

  (when (= 300 (q/frame-count))
    (println "Slowing down framerate")
    (q/frame-rate 30))

  (when (= 800 (q/frame-count))
    (println "Done drawing flow-field")
    (q/no-loop))

  (run! (fn [particle]
          (let [particle-color (particle-color particle)]
            (q/fill
             (color-red particle-color)
             (color-green particle-color)
             (color-blue particle-color)
             (color-alpha particle-color)
             )


            (q/ellipse
             (particle-x particle)
             (particle-y particle)
             particle-width
             particle-width)))
        particles))

; this function is called in index.html
(defn ^:export run-sketch [id]
  (let [elem (js/document.getElementById id)
        height (.-offsetHeight elem)
        width (.-offsetWidth elem)]
  (q/defsketch perlin-flow
    :host id
    :size [width height]
    ; setup function called only once, during sketch initialization.
    :setup (partial setup height width)
    ; update-state is called on each iteration before draw-state.
    :update (partial update-state height width)
    :draw draw-state
    ; This sketch uses functional-mode middleware.
    ; Check quil wiki for more info about middlewares and particularly
    ; fun-mode.
    :middleware [m/fun-mode])))

; uncomment this line to reset the sketch:
; (run-sketch)
