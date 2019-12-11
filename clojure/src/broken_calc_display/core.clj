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

(defn diff-digits [[ins, del] [x y]]
  (cond
    (> x y) [(inc ins) del]
    (< x y) [ins (inc del)]
    :else [ins del]))

(defn diff-chars [a b]
  (->> [a, b]
       (map #(get digit-codes %))
       (apply map vector)
       (reduce diff-digits [0 0])))

(defn diff-numbers [a b]
  (->> (map vector a b)
       (map #(apply diff-chars %))
       (apply map +)))

(defn solution []
  (letfn [(every-eq-five? [coll] (every? #(= % 5) coll))
          (five-segments-changed? [x]
            (-> x (diff-numbers src-num) (every-eq-five?)))]
    (filter five-segments-changed? variants)))

(defn -main []
  (doseq [result (solution)]

    (println result)))
