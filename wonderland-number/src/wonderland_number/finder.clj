(ns wonderland-number.finder)

(defn hasAllTheSameDigits? [n1 n2]
  (let [s1 (set (str n1))
        s2 (set (str n2))]
    (= s1 s2)))

(defn is-wonderland-number? [n]
  (and (hasAllTheSameDigits? n (* 2 n))
       (hasAllTheSameDigits? n (* 3 n))
       (hasAllTheSameDigits? n (* 4 n))
       (hasAllTheSameDigits? n (* 5 n))
       (hasAllTheSameDigits? n (* 6 n))))

(defn wonderland-number []
  (first (filter is-wonderland-number? (range 100000 (/ 999999 6)))))
