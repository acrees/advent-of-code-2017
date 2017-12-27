(require '[clojure.string :as str])

(defn parse-row [input]
  (let [[n w xs] (rest (re-matches #"(\w+?) \((\d+)\)(?: -> )?(.*)?" input))
        children (if (nil? xs) [] (str/split xs #", "))]
    [n w children]))

(defn all-children [rows]
  (->> rows
       (mapcat #(nth % 2))
       set))

(defn all-roots [rows]
  (map first rows))

(defn parse-input [input]
  (->> input
       str/split-lines
       (map parse-row)))

(defn find-base [input]
  (let [rows (parse-input input)
        children (all-children rows)
        roots (all-roots rows)]
    (filter #(not (contains? children %)) roots)))

(find-base "suvtxzq (242) -> tdoxrnb, oanxgk
smjsfux (7)
oanxgk (68) -> smjsfux")
