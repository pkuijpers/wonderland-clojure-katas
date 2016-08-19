(ns fox-goose-bag-of-corn.puzzle
  (:require [clojure.set :as s]))

(def start-pos [[[:fox :goose :corn :you] [:boat] []]])

(defn end-pos? [pos]
  (= (last (map set pos))
     #{:you :fox :goose :corn}))

(def hardcoded-plan
  (conj start-pos
        [[:fox :corn] [:boat :you :goose] []]
        [[:fox :corn] [:boat] [:you :goose]]
        [[:fox :corn] [:boat :you] [:goose]]
        [[:fox :corn :you] [:boat] [:goose]]
        [[:fox] [:boat :you :corn] [:goose]]
        [[:fox] [:boat] [:you :corn :goose]]
        [[:fox] [:boat :you :goose] [:corn]]
        [[:fox :you :goose] [:boat] [:corn]]
        [[:goose] [:boat :you :fox] [:corn]]
        [[:goose] [:boat] [:you :fox :corn]]
        [[:goose] [:boat :you] [:fox :corn]]
        [[:goose :you] [:boat] [:fox :corn]]
        [[] [:boat :you :goose] [:fox :corn]]
        [[] [:boat] [:you :goose :fox :corn]]
        ))

(defn valid-combination? [col]
  (let [c (set col)]
    (or
      (contains? c :you)
      (and
        (not (s/subset? #{:fox :goose} c))
        (not (s/subset? #{:goose :corn} c))))))

(defn move-to-boat [shore]
  (let [others (disj (set shore) :you)
        boat-moves (conj (map #(conj #{:boat} :you %) others) #{:you :boat})
        possible-moves (map
                         #(conj [] (s/difference others %) %)
                         boat-moves)]
    (filter #(valid-combination? (first %)) possible-moves)))

(defn move-to-shore [pos]
  (let [left (set (first pos))
        boat (set (second pos))
        right (set (last pos))
        move-left (vec (map vec [(s/union left (disj boat :boat)) #{:boat} right]))
        move-right (vec (map vec [left #{:boat} (s/union right (disj boat :boat))]))]
    [move-left move-right]))

(defn possible-moves [[left boat right :as pos]]
  (cond
    (contains? (set left) :you)
    (map #(conj [] (vec (first %)) (vec (last %)) right) (move-to-boat (set left) ))
    (contains? (set boat) :you) (move-to-shore pos)
    :else (map #(conj [] left (vec (last %)) (vec (first %))) (move-to-boat (set right) ))))

(defn river-crossing-plan []
  (loop [queue [start-pos]
         positions-seen #{}]
    (let [path (first queue)]
      (cond (end-pos? (last path)) path
            (contains? positions-seen (last path)) (recur (rest queue) positions-seen)
            :else (recur
                    (concat (rest queue) (map #(conj path %) (possible-moves (last path))))
                    (conj positions-seen (last path)))))))
