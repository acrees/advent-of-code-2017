(require '[clojure.string :as str])

(defrecord Instruction [reg op value cond-reg cond-op cond-value])

(defn parse-instr [string]
  (let [[reg op value cond-reg cond-op cond-value]
        (->> string
             (re-matches #"(\w+) (inc|dec) (-?\d+) if (\w+) (>|<|<=|>=|==|!=) (-?\d+)")
             rest)]
    (Instruction.
      reg
      op
      (read-string value)
      cond-reg
      cond-op
      (read-string cond-value))))

(defn get-or-default [env reg]
  (let [value (get env reg)]
    (if (nil? value) 0 value)))

(defn should-exec? [env instr]
  (let [value (get-or-default env (:cond-reg instr))
        cond-value (:cond-value instr)]
    (case (:cond-op instr)
      ">"  (> value cond-value)
      "<"  (< value cond-value)
      ">=" (>= value cond-value)
      "<=" (<= value cond-value)
      "==" (= value cond-value)
      "!=" (not (= value cond-value)))))

(defn apply-instr [instr current-value]
  (let [value (:value instr)]
    (case (:op instr)
      "inc" (+ current-value value)
      "dec" (- current-value value))))

(defn exec [env instr]
  (let [reg (:reg instr)
        current-value (get-or-default env reg)
        new-value (apply-instr instr current-value)]
    (assoc env reg new-value)))

(defn run [input]
  (->> input
       str/split-lines
       (map parse-instr)
       (reduce (fn [acc n] (if (should-exec? acc n) (exec acc n) acc))
               (hash-map))
       (sort-by (fn [[reg v]] v))
       reverse))

(defn run2 [input]
  (->> input
         str/split-lines
         (map parse-instr)
         (reductions (fn [acc n] (if (should-exec? acc n) (exec acc n) acc))
                 (hash-map))
         (mapcat seq)
         (sort-by (fn [[reg v]] v))
         reverse))

(run2 "b inc 5 if a > 1
a inc 1 if b < 5
c dec -10 if a >= 1
c inc -20 if c == 10")