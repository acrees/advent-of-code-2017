(require '[clojure.string :as str])

(defrecord Node [program weight children])

(defn parse-row [input]
  (let [[n w xs] (rest (re-matches #"(\w+?) \((\d+)\)(?: -> )?(.*)?" input))
        weight (read-string w)
        children (if (empty? xs) [] (str/split xs #", "))]
    (Node. n weight children)))

(defn all-children [rows]
  (->> rows
       (mapcat :children)
       set))

(defn parse-input [input]
  (->> input
       str/split-lines
       (map parse-row)))

(defn get-base [rows]
  (let [children (all-children rows)]
    (first (filter #(not (contains? children (:program %))) rows))))

(defn find-base [input]
  (->> input parse-input get-base))

#_("PART 2")

(defn remove-nulls [coll]
  (filter #(not (nil? %)) coll))

(defn freqs [coll]
  (reduce
    (fn [acc [i n]]
      (let [[c j] (get acc n)]
        (if (nil? c)
          (assoc acc n [1 i])
          (assoc acc n [(+ c 1) j]))))
    (hash-map)
    (map-indexed (fn [x y] [x y]) coll)))

(defn non-matching-indices [coll]
  (->> coll
       freqs
       (filter #(= 1 (first (second %))))
       (map (fn [[v [c i]]] i))))

(defn most-common-value [coll]
  (->> coll
       freqs
       (sort-by (fn [[v [c i]]] c))
       reverse
       first
       first))

(defn get-children [node-set node]
  (->> node
       :children
       (map #(get node-set %))
       remove-nulls
       (map #(build-tree node-set %))))

(defn build-tree [node-set root]
    (Node. (:program root) (:weight root) (get-children node-set root)))

(defn weight-of [node]
  (+
    (:weight node)
    (->> node
         :children
         (map weight-of)
         (reduce + 0))))

(defn is-unbalanced? [node]
  (>
    (->> node
       :children
       (map weight-of)
       non-matching-indices
       count)
  0))

(defn unbalanced [parent node]
  (let [children (:children node)
        child-weights (map weight-of children)
        unbalanced-result (->> children (map #(unbalanced node %)) remove-nulls first)
        i (first (non-matching-indices child-weights))
        odd-child (when-not (nil? i) (nth children i))]
    (when-not (or (empty? children) (nil? odd-child))
      (if-not (nil? unbalanced-result)
        unbalanced-result
        (if (empty? (:children odd-child))
          (most-common-value child-weights)
          (-
            (most-common-value child-weights)
            (-
              (weight-of odd-child)
              (:weight odd-child))))))))

(defn run [input]
  (let [rows (parse-input input)
        root (get-base rows)
        node-set (apply hash-map (mapcat (fn [x] [(:program x) x]) rows))
        tree (build-tree node-set root)]
    (unbalanced nil tree)))

(run "root (0) -> left, right
left (4)
right (5)")

(run "root (0) -> left, right, centre
left (4)
right (6)
centre (4)")

(run "root (0) -> left, right
left (10) -> ll, lr
right (10)
ll (5)
lr (5)")
