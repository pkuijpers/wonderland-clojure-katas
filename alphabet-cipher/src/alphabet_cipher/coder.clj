(ns alphabet-cipher.coder)

(defn- rotate
  "Returns a new vector with the first element of coll appended to
  the rest of coll."
  [coll]
  (conj (vec (rest coll)) (first coll)))

(def chart
  (vec (take 26 (iterate rotate (vec "abcdefghijklmnopqrstuvwxyz")))))

;; ASCII value of 'a' char
(def ascii-a 97)

(defn- char-index
  "Returns the index in the alphabet of c"
  [c]
  (- (int c) ascii-a))

(defn- char-at
  "Returns the character at index asc in the alphabet"
  [asc]
  (char (+ ascii-a asc)))

(defn- chart-lookup [col-char row-char]
  ((chart (char-index row-char)) (char-index col-char)))

(defn- chart-inverse-lookup [col-char body-char]
  (let [row (chart (char-index col-char))]
    (char-at (.indexOf row body-char))))

(defn- use-chart [keyword message lookup]
  (let [msg (vec message)
        kw (vec (take (count message) (cycle (vec keyword))))]
    (apply str (map-indexed #(lookup (kw %1) %2) msg))))

(defn encode [keyword message]
  (use-chart keyword message chart-lookup))

(defn decode [keyword message]
  (use-chart keyword message chart-inverse-lookup))

(defn decipher [cipher message]
  (let [repeated-keyword (vec (use-chart message cipher chart-inverse-lookup))
        candidates (map #(take % repeated-keyword) (range 1 (count repeated-keyword)))]
    (apply str (first (filter #(= (take (count repeated-keyword) (cycle %))
                       repeated-keyword)
                   candidates)))))

