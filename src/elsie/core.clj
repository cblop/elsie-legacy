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

(defonce guitar-bus (audio-bus))
(defonce drum-bus (audio-bus))
(def guitar-1 (buffer (samples 4 4 180 4)))

(defsynth guitar []
  (out guitar-bus (sound-in 1)))

(defsynth monitor [bus guitar-bus]
  (out 0 (in:ar bus)))

(defsynth record [buf default-buffer bus guitar-bus]
  (let [signal (in:ar bus)]
    (record-buf:ar [signal] buf :loop false)))

(defn duration [s]
  (float (* 1000 (/ (num-frames s) SRATE))))

(definst splayer
  [note 60 level 1 rate 1 loop? 0 attack 0 decay 1 sustain 1 release 0.1 curve -4 gate 1]
  (let [buf (index:kr (:id index-buffer) note)
        env (env-gen (adsr attack decay sustain release level curve)
                     :gate gate
                     :action FREE)]
    (* env (scaled-play-buf 2 buf :level level :loop loop? :action FREE))))


(defsynth player [buf default-buffer bus 0]
  (let [env (env-gen (asr 0 1 0) :action FREE)]
    (out bus [(* (play-buf:ar 1 buf :action FREE) env) (* (play-buf:ar 1 buf :action FREE) env)])))

(defn play
  ([buf] (player buf))
  ([buf times] (doseq [i (range times)]
                 (player buf)
                 (if-not (= i (- times 1)) (Thread/sleep (duration buf))))))

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

(def kick (load-sample "sounds/Kickdrum10.wav"))
(def open-hat (load-sample "sounds/OHH01.wav"))
(def closed-hat (load-sample "sounds/CHH4.wav"))
(def snare (load-sample "sounds/hs14.wav"))


(def kick (sample "sounds/Kickdrum10.wav"))
(def open-hat (sample "sounds/OHH01.wav"))
(def closed-hat (sample "sounds/CHH4.wav"))
(def snare (sample "sounds/hs14.wav"))

(sample-player open-hat :release 0.1 :end 0.1)

(player kick)
(play open-hat)
(play closed-hat)
(play snare)

(midi-connected-devices)

;; (event-debug-on)
(event-debug-off)

(defsynth out-bus [in-bus 0]
  ;; (out 0 (pan2 in-bus))
  (out 0 in-bus)
  )

(on-event [:midi :note-on]
          (fn [e]
            (let [note (:note e)
                  vel  (:velocity e)]
              (case note
                36 (player kick :bus drum-bus)
                40 (player snare :bus drum-bus)
                41 (player closed-hat :bus drum-bus)
                37 (player open-hat :bus drum-bus)
                "default")))
          ::pad-handler)

(def sounds {:E4 hello-world :B3 greetings})


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


(play-buf 1 guitar-loop)

(def distortion (inst-fx! guitar fx-distortion-tubescreamer))

(ctl distortion :gain 20)

(guitar)
(inst-fx! guitar fx-distortion2)
(inst-fx! guitar fx-reverb)

(clear-fx guitar)

(stop)

(def blips-5-4 (buffer (samples 5 4 180 4)))
(def drum-1 (buffer (samples 5 4 180 4)))

(defn begin [song]
  (doseq [x song]))

(defn then [& args]
  (doseq [x args]
    (do
      (eval x))))

;; Thread/sleep is not the best choice at all, but might be good enough for now
(defmacro then [& args]
  (let [x (gensym 'x)]
    `(doseq [~x '~args]
       (do
         (eval ~x)
         (if-not (= (name (first ~x)) "together") (Thread/sleep (duration (var-get (resolve (second ~x))))))
         ))))

(defmacro together [& args]
  (let [x (gensym 'x)]
    `(doseq [~x (quote ~args)]
       (eval ~x))))

(guitar)
(duration `~(second `(play hello-world)))
(play greetings)

(duration (var-get (resolve 'hello-world)))
(resolve 'hello-world)
(duration ~(symbol (name (ns-name *ns*)) (name 'hello-world)))
(symbol (name (ns-name *ns*)) (name 'hello-world))

(then (play hello-world 4)
      (play greetings))

(then (play hello-world)
      (together (play greetings)
                (play hello-world)))

(together (play hello-world)
          (play greetings))

(together (play hello-world)
          (then (play greetings)
                (play hello-world)))

(record blips-5-4 :bus 0)
(play blips-5-4)

(macroexpand '(then (play blips-5-4)
                    (record drum-1)))

(macroexpand '(together (play blips-5-4)
                        (record drum-1)))

(defmacro both [& args])

;; do we really want to Thread/sleep? Is it not better to use a metronome?
;; let's see how it goes. I think metronomes could be a headache
;; also: might want to do core.async/futures

;; (macro-expand (then (play blips-5-4)
;;                     (record drum-1)))
;; (doseq [x '('(play blips-5-4) '(record drum-1))]
;;   (do
;;     (eval x)
;;     (Thread/sleep (duration (second x)))))

;; (macro-expand (both (play blips-5-4)
;;                     (record drum-1)))
;; (doseq [x '('play blips-5-4) '(record drum-1)]
;;   (eval x))

;; sounds are in pre-sized buffers
;; this data structure describes a song:
;; (def mad-jam (then (play blips-5-4)
;;                    (both (play blips-5-4 2)
;;                          (record drum-1))
;;                    (both (play drum-1)
;;                          (record guitar-1))
;;                    (both (play drum-1 2)
;;                          (play guitar-1 2)
;;                          (then (record vox-1)
;;                                (record vox-2)))))

;; these will need to be macros

;; this triggers/plays the song:
;; (begin mad-jam)
;; (remember you can conj/cons song data together:)
;; (begin (conj mad-jam rude-beats))
;; Consider replacing def with defjam macro if needed!

