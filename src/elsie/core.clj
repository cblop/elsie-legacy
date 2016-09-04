(ns elsie.core
  (:require [overtone.live :refer :all]
            [quil.core :as q]))

(definst guitar []
  (sound-in 1))

(guitar)

(def SRATE (:sample-rate (server-info)))

(def default-buffer (buffer 44100))

(defsynth blop [frq 220]
  (out 0 (pan2 (* sin-osc frq (env-perc)))))

(demo (blip 440 5))
(demo (sin-osc))

(demo (blop))

(defn samples
  [num denom bpm bars]
  (let [secs-per-bar (* num (/ bpm 60))
        samples-per-bar (* SRATE secs-per-bar)]
    (* bars samples-per-bar)))

(def guitar-bus (audio-bus))
(def guitar-1 (buffer (samples 4 4 180 4)))

(defsynth guitar []
  (out guitar-bus (sound-in 1)))

(defsynth monitor [bus guitar-bus]
  (out 0 (in:ar bus)))

(defsynth record [buf default-buffer bus guitar-bus]
  (let [signal (in:ar bus)]
    (record-buf:ar [signal] buf)))

(defsynth play [buf default-buffer]
  (out 0 (play-buf:ar 1 buf)))

(defsynth pitch-monitor []
  (let [g (in:ar guitar-bus)
        p (pitch g)
        reading (tap :pitch 5 p)
        ]
    (out 0 10)))

(guitar)

(def pt (pitch-monitor))

(def standard {"E3" 329.63 "B3" 246.94 "G3" 196.0 "D3" 146.83 "A2" 110.0 "E2" 82.41})

(defn tune [freqs]
  (while true
    (do
      (Thread/sleep 1000)
      (let [freq @(get-in pt [:taps :pitch])]
        (doseq [f (keys freqs)]
          (if (= freq (get freqs f)) (println f) (println freq)))
        ))))

(defn semitones [f g]
  (* 1200 (/ (Math/log (/ g f)) (Math/log 2))))

(def hello-world (load-sample "sounds/hello-world.wav"))
(def greetings (load-sample "sounds/greetings.wav"))

(def sounds {:E4 hello-world :B3 greetings})

(defn duration [s]
  (float (* 1000 (/ (num-frames s) SRATE))))

(defn elsie [d]
  (while true
    (do
      (Thread/sleep 500)
      (let [f @(get-in pt [:taps :pitch])
            note-midi (hz->midi f)
            note-name (find-note-name note-midi)
            diff (midi->hz note-midi)
            cents (semitones f diff)]
        (do
          (println (str note-name " " cents))
          (if (get d note-name)
              (do
                (play (get d note-name))
                (Thread/sleep (duration (get d note-name))))))))))

(guitar)
(def pt (pitch-monitor))
(elsie sounds)
(stop)

(defn chromatic-tuner []
  (while true
    (do
      (Thread/sleep 100)
      (let [f @(get-in pt [:taps :pitch])
            note-midi (hz->midi f)
            diff (midi->hz note-midi)
            cents (semitones f diff)]
        (println (str (find-note-name note-midi) " " cents " " (cond (< 10 cents) "▲"
                                                                    (> -10 cents) "▼"
                                                                    :else "✓"))))))
  )

(chromatic-tuner)

(tune standard)

(while true
  (do
    (Thread/sleep 1000)
    (let [freq @(get-in pt [:taps :pitch])]
      (doseq [f (keys standard)]
        (if (= freq (get standard f)) (println "WOO") (println "YAY"))
        ))))

;; don't forget that recording wraps around: you must stop it
(record guitar-1)
(play guitar-1)
(stop)

(guitar)
(monitor)

(def drums1 (buffer (samples 5 4 180 2)))

(defsynth pitch-controlled-saws
  [out-bus 0]
  (let [p   (pitch (sound-in))
        ;; p   (* p (/ p 4))
        p (/ p 4)
        ;; p   (/ p 2)
        ;; p   (lag p 0.1)
        ]
    (out out-bus (square [p (+ p (* p 0.01))]))))

(defsynth pitch-controlled-saws-2
  [out-bus 0]
  (let [p   (pitch (sound-in))
        ;; p   (* p (/ p 4))
        ;; p (/ p 4)
        ;; p   (/ p 2)
        ;; p   (lag p 0.1)
        ]
    (out out-bus (square [p (+ p (* p 0.01))]))))


(do
  (guitar)
  (pitch-controlled-saws))

(guitar)
(monitor)

(pitch-controlled-saws)
(pitch-controlled-saws-2)
(stop)


(record-buf:ar [0] guitar-loop)
(play-buf 1 guitar-loop)

(def distortion (inst-fx! guitar fx-distortion-tubescreamer))

(ctl distortion :gain 20)

(guitar)
(inst-fx! guitar fx-distortion2)
(inst-fx! guitar fx-reverb)

(clear-fx guitar)

(stop)

;; sounds are in pre-sized buffers
;; this data structure describes a song:
;; (def mad-jam (then (play blips-5-4)
;;                    (then (play blips-5-4 2)
;;                          (record drum-1))
;;                    (then (play drum-1)
;;                          (record guitar-1))
;;                    (then (play drum-1 2)
;;                          (play guitar-1 2)
;;                          (then (record vox-1)
;;                                (record vox-2)))))

;; this triggers/plays the song:
;; (begin mad-jam)
;; (remember you can conj/cons song data together:)
;; (begin (conj mad-jam rude-beats))
;; Consider replacing def with defjam macro if needed!

