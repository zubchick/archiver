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
       (loop [xs (transient [start])
              current start
              table prob-map]
         (if (empty? table)
           (persistent! xs)
           (let [[_ prob] (first table)
                 new (+ current (* k prob))]
             (recur (conj! xs new)
                    new
                    (rest table))))))))

(defn find-interval [iseq num]
  (let [fst (first iseq)
        snd (second iseq)]
    (if (< num snd)
      [fst snd]
      (recur (rest iseq) num))))

