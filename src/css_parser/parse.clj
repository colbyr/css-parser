(ns css-parser.parse)

(defn one [parser input]
  (parser input))

(defn all [parser input]
  (->> input
       (one parser)
       (filter #(= "" (second %)))
       ffirst))
