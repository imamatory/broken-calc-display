(ns broken-calc-display.core
  (:require [clojure.string :as str]))

(def src-num ["2" "5" "6" "9" "1" "7" "2" "5"])

(def digit-codes
  {"0" [1 1 1 1 1 1 0]
   "1" [0 1 1 0 0 0 0]
   "2" [1 1 0 1 1 0 1]
   "3" [1 1 1 1 0 0 1]
   "4" [0 1 1 0 0 1 1]
   "5" [1 0 1 1 0 1 1]
   "6" [1 0 1 1 1 1 1]
   "7" [1 1 1 0 0 0 0]
   "8" [1 1 1 1 1 1 1]
   "9" [1 1 1 1 0 1 1]})

(def variants
  (let [rng (range 1 100)
        str-variants (for [a rng b rng]
                       (format "%02d%02d%04d" a b (* a b)))]
    (map #(str/split % #"") str-variants)))

(defn diff-chars [a b]
  (letfn [(compare-chars [[ins, del] [x y]]
            (cond
              (> x y) [(inc ins) del]
              (< x y) [ins (inc del)]
              :else [ins del]))]
    (->> [a, b]
         (map #(get digit-codes %))
         (apply map vector)
         (reduce compare-chars [0 0]))))

(defn solution []
 (letfn [(diff-digits [a b]
           (->> (map vector a b)
                (map #(apply diff-chars %))
                (apply map +)))
         (every-eq-five? [coll] (every? #(= % 5) coll))
         (diffs-with-five-bars? [x]
           (every-eq-five? (diff-digits x src-num)))]
   (->> (take 10000 variants)
        (filter diffs-with-five-bars?)))
 )


(defn -main []
  (doseq [result (solution)]
    (println result))
  )
