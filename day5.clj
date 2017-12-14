(require '[clojure.string :as str])

(defn update-instruction [offset]
  (+ offset 1))

(defn update-instruction2 [offset]
  (if (>= offset 3) (- offset 1) (+ offset 1)))

(defn jump [instructions position counter]
  (let [instr (nth instructions position)]
    [(assoc instructions position (update-instruction2 instr))
     (+ position instr)
     (+ counter 1)]))

(defn exited? [instructions position]
  (>= position (count instructions)))

(defn run
  ([instructions]
   (run instructions 0 0))

  ([instructions position counter]
   (if (exited? instructions position)
     counter
     (let [[i p c] (jump instructions position counter)]
       (recur i p c)))))

(defn parse-and-run [input]
  (->> input
       str/split-lines
       (map read-string)
       vec
       run))

(parse-and-run "0
               3
               0
               1
               -3")