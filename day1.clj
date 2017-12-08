(defn sumx [x]
  (last
    (reduce
      (fn [[last total] n]
          (if (= n last)
              [n (+ total n)]
              [n total]))
      [0 0]
      (let [xs (clojure.string/split x #"")]
           (map read-string (conj xs (first xs)))))))

(sumx "1212")
