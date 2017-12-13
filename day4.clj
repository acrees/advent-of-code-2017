(require '[clojure.string :as str])

(defn all-different? [password]
  (let [words (str/split password #"\s")]
    (= (count words) (count (set words)))))

(defn no-anagrams? [password]
  (let [words (str/split password #"\s")]
    (=
      (count words)
      (->> words
           (map sort)
           set
           count))))

(defn number-of-lines-matching [f input]
  (->> input
       str/split-lines
       (filter f)
       count))

(number-of-lines-matching no-anagrams? "kvvfl kvvfl olud wjqsqa olud frc
slhm rdfm yxb rsobyt rdfm
pib wzfr xyoakcu zoapeze rtdxt rikc jyeps wdyo hawr xyoakcu hawr
ismtq qwoi kzt ktgzoc gnxblp dzfayil ftfx asscba ionxi dzfayil qwoi
dzuhys kfekxe nvdhdtj hzusdy xzhehgc dhtvdnj oxwlvef
gxg qahl aaipx tkmckn hcsuhy jsudcmy kcefhpn kiasaj tkmckn")