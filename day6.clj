(defn maxi [x]
  (second
    (reduce
      (fn [[v, i, j] n]
        (if (> n v) [n, j, (+ j 1)] [v, i, (+ j 1)]))
    [0, 0, 0]
    x)))

(defn next-index [blocks i]
  (if (= (+ i 1) (count blocks)) 0 (+ i 1)))

(defn redistribute
  ([blocks]
    (let [i (maxi blocks)
          freed (nth blocks i)
          removed (assoc blocks i 0)]
      (redistribute removed freed (next-index blocks i))))
  ([blocks freeBlocks pos]
    (if (<= freeBlocks 0)
      blocks
      (let [value (nth blocks pos)
            newValue (+ value 1)
            newBlocks (assoc blocks pos newValue)
            newPos (next-index blocks pos)]
        (recur newBlocks (- freeBlocks 1) newPos)))))

(defn redist-until-cycle
  ([initial-blocks] (redist-until-cycle initial-blocks (set [initial-blocks]) 1))
  ([blocks history counter]
   (let [next-blocks (redistribute blocks)]
     (if (contains? history next-blocks)
        counter
        (recur next-blocks (conj history next-blocks) (+ counter 1))))))

(defn indices-of [f coll]
  (keep-indexed #(if (= f %2) %1 nil) coll))

(defn redist-until-cycle2
  ([initial-blocks] (redist-until-cycle2 initial-blocks [initial-blocks] 1))
  ([blocks history counter]
   (let [next-blocks (redistribute blocks)
         i (first (indices-of next-blocks history))]
     (if-not (nil? i)
        (- counter i)
        (recur next-blocks (conj history next-blocks) (+ counter 1))))))

(redist-until-cycle2 [0 2 7 0])
(redist-until-cycle2 [14 0 15 12 11 11 3 5 1 6 8 4 9 1 8 4])
