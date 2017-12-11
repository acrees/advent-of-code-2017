(require '[clojure.string :as str])

(defn split-rows [input]
  (->> input
      str/split-lines
      (map
        (comp #(map read-string %)
              #(str/split % #"\t")))))

(defn min-max [xs]
  (reduce
    (fn [[min-val max-val] n]
      [(min min-val n) (max max-val n)])
   [10000 0]
   xs))

(defn diff [[a b]]
  (Math/abs (- a b)))

(defn sum [x]
  (reduce + 0 x))

(defn checksum [input]
  (->> input
       split-rows
       (map (comp diff min-max))
       sum))

(defn pairs [coll]
  (when-let [s (next coll)]
    (lazy-cat (for [y s] [(first coll) y])
              (pairs s))))

(defn divisible-pair [row]
  (->> row
       pairs
       (map (fn [[a b]] (/ (max a b) (min a b))))
       (filter integer?)))

(defn divisble-sums [input]
  (->> input
       split-rows
       (mapcat divisible-pair)
       sum))

(divisble-sums "5	9	2	8
               9	4	7	3
               3	8	6	5")
