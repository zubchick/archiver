(ns arch.core
  (:use [clojure.java.io :only [input-stream]]))

(defn symbol-prob
  "Create probability table from byte stream"
  [stream]
  (loop [c (.read stream)
         table (transient {})
         count 0]
    (if (== c -1)
      (into {} (for [[k v] (persistent! table)] [(char k) (/ v count)]))
      (recur (.read stream)
             (assoc! table c (+ 1 (get table c 0)))
             (inc count)))))

(defn intervals
  "Create intervals from probability table"
  ([prob-map]
     (intervals prob-map 0 1))

  ([prob-map start end]
     (let [k (- end start)]
       (loop [xs (transient {})
              current start
              table prob-map]
         (if (empty? table)
           (persistent! xs)
           (let [[chr prob] (first table)
                 new (+ current (* k prob))]
             (recur (assoc! xs chr [current new])
                    new
                    (rest table))))))))

(defn find-interval [imap num]
  (let [start (first (second (first imap)))
        iseq (cons start
                   (for [[_ [_ snd]] imap] snd))]
    (loop [xs iseq]
      (let [fst (first xs)
            snd (second xs)]
          (if (< num snd)
            [fst snd]
            (recur (rest xs)))))))

(defn encode [schema, stream]
  (loop [chr (.read stream)
         interval [0 1]]
    (let [[fst snd] interval]
      (if (== chr -1)
        (/  (+ fst snd) 2)             ; middle of interval
        (let [ints (intervals schema fst snd) ; new interval-map
              [start end] (ints (char chr))] ; find new interval
          (recur (.read stream) [start end]))))))
