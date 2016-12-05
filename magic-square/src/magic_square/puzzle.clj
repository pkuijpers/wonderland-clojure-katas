(ns magic-square.puzzle
  (:require [clojure.math.combinatorics :as combo]))

(def values [1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5 5.0])

(def magic-number (/ (apply + values) (Math/sqrt (count values))))

(defn magic? [vals]
  (= magic-number (apply + vals)))

(defn magic-rows [vals]
  (let [rows (mapcat combo/permutations (combo/combinations vals 3))]
    (filter magic? rows)))

(defn magic-rowsets
  "Generates a lazy seq of all possible squares using"
  [values]
  (for [r1 (magic-rows values)
        r2 (magic-rows (remove (set r1) values))
        r3 (magic-rows (remove (set (concat r1 r2)) values))]
    [r1 r2 r3]))

(defn columns [square]
  (apply map vector square))

(defn diagonals [square]
  (let [left-to-right
        (for [i (range (count square))]
          (get-in square [i i]))
        right-to-left
        (for [i (range (count square))]
          (get-in square [i (- (count square) i 1)]))]
    [left-to-right right-to-left]))

(defn magic-square? [rows]
  (and (every? magic? rows)
       (every? magic? (columns rows))
       (every? magic? (diagonals rows))))

(defn magic-square [values]
  (first (filter magic-square? (magic-rowsets values))))
