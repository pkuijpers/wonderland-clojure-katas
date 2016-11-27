(ns doublets.solver
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(def words (-> "words.edn"
               (io/resource)
               (slurp)
               (read-string)))

(defn link? [word1 word2]
  (and
    (= (count word1) (count word2))
    (let [letter-pairs (map vector word1 word2)
          diffs (filter #(not= (first %) (last %)) letter-pairs)]
      (= 1 (count diffs)))))

(defn links[doublet]
  (let [lw (last doublet)
        equal-length-words (filter #(= (count %) (count lw)) words)]
    (map #(conj doublet %)
         (filter #(and
                    (not-any? (partial = %) doublet)
                    (link? lw %))
                 equal-length-words))))

(defn doublets [word1 word2]
  (let [doublet
        (some #(when (= word2 (last %)) %)
              (tree-seq #(not= (last %) word2)
                        links
                        [word1]))]
    (or doublet [])))
