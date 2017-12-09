(defn split-numbers [x]
  (map read-string
       (clojure.string/split x #"")))

(defn next-index [length current]
  (let [i (+ (/ length 2) current)]
    (if (< i length) i (- i length))))

(defn sum [x] (reduce + x))

(defn sum1 [x]
  (last
    (reduce
      (fn [[last total] n]
          (if (= n last)
              [n (+ total n)]
              [n total]))
      [0 0]
      (let [xs (split-numbers x)]
           (concat xs (take 1 xs))))))

(defn sum2 [x]
  (let [xs (vec (split-numbers x))
        len (count xs)]
        (sum
            (map-indexed
                (fn [i item]
                  (let [j (next-index len i)
                        next-item (nth xs j)]
                    (if (= item next-item) item 0)))
              xs))))

(sum2 "1212")
