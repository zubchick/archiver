(ns arch.core
  (:use [clojure.java.io :only [input-stream]]))

(defn symbol-prob
  "Create probability table from byte stream"
  [stream]
  (loop [c (.read stream)
         table (transient {})
         count 0]
    (if (== c -1)
      (list count
            (into {} (for [[k v] (persistent! table)]
                       [(char k) (/ v count)])))
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
  (loop [xs (lazy-seq imap)]
    (if-let [[char [start end]] (first xs)]
      (if (and (< num end)
               (>= num start))
        [char, start, end]
        (recur (rest xs))))))

(defn encode [schema, stream]
  (loop [chr (.read stream)
         fst 0, snd 1]
    (if (== chr -1)
      fst                               ; final number
      (let [ints (intervals schema fst snd) ; new interval-map
            [start end] (ints (char chr))] ; find new interval
        (recur (.read stream) start end)))))

(defn decode [schema number length]
  (defn- help-f [i fst snd]
    (when (< i length)
      (let [ints (intervals schema fst snd)
            [char start end] (find-interval ints number)]
        (cons char (lazy-seq (help-f (inc i) start end))))))

  (help-f 0 0 1))
