(#_"PART 1")

(defn spiral [n]
  (->> (concat
        (reverse (range n (* n 2)))
        (range (+ n 1) (+ 1 (* n 2))))
      (repeat 4)
      flatten))

(def infinite-spiral
  (flatten (lazy-cat [0] (map spiral (range)))))


(defn distance [n] (nth infinite-spiral (- n 1)))

(distance 1024)

(#_"PART 2")

(defn sum [x] (reduce + 0 x))

(defn previous-row [previous-spiral s o last-row]
  (let [row-scale (* 2 (- s 1))
         row-size (if-not last-row (- row-scale 1) row-scale)
        trim (* 2 o (- s 2))]
      (->> previous-spiral
           (drop trim)
           (take row-size))))

(defn mid-cells-range [s row]
  (let [x (+ (* 2 (- s 2)) 0)
        y (if (= row :last) (+ x 1) x)]
    (range 1 y)))

(defn next-row-diffs [prev s lst row]
  (let [fst (first prev)]
    (concat
      (if (= row :first)
        [(nth prev 1)]
        [fst (+ lst (sum (take 2 prev)))])
      (map
        #(+ (nth prev (- % 1)) (nth prev %) (nth prev (+ % 1)))
        (mid-cells-range s row))
      (if-not (= row :last)
        (->> prev
           reverse
           (take 2)
           sum
           (conj []))
        []))))

(defn next-row
  ([prev s o]
   (drop 1
        (reductions + (first prev)
                    (next-row-diffs (previous-row prev s o false)
                                    s
                                    (last prev)
                                    :first))))
  ([prev s o last-value last-row]
   (drop 1
        (reductions + last-value
                    (next-row-diffs (previous-row prev s o last-row)
                                    s
                                    last-value
                                    (if last-row :last :middle))))))

(defn next-spiral [prev s]
  (let [x (concat [(last prev)] prev)
        right (next-row x s 0)
        top (next-row x s 1 (last right) false)
        left (next-row x s 2 (last top) false)
        bottom (next-row (concat x [(first right)]) s 3 (last left) true)
        last-value (+ (last prev) (first right) (last bottom))]
    (concat right top left bottom [last-value])))

(def infinite-spiral2
  (->> (range)
       (drop 3)
       (reductions #(next-spiral %1 %2) [1 2 4 5 10 11 23 25])
       (lazy-cat [1])
       flatten))

(first (drop-while #(< % 289326) infinite-spiral2))