(ns css-parser.monad
  (:require [css-parser.parse :as parse]))


(defn- any [input]
  (if (empty? input) '()
    (list [(first input)
           (apply str (rest input))])))

(defn- failure [_] '())

(defn return [v]
  (fn [input] (list [v input])))

(defn >>= [m f]
  (fn [input]
    (->> input
         (parse/one m)
         (mapcat (fn [[v tail]] (parse/one (f v) tail))))))

(defn sat
  "I have no idea what this does"
  [pred]
  (>>= any (fn [v] (if (pred v) (return v) failure))))

;; haskell-ish do* macro

(defn- merge-bind [body bind]
  (if (and (not= clojure.lang.Symbol (type bind))
           (= 3 (count bind))
           (= '<- (second bind)))
    `(>>= ~(last bind) (fn [~(first bind)] ~body))
    `(>>= ~bind (fn [~'_] ~body))))

(defmacro do* [& forms]
  (reduce merge-bind (last forms) (reverse (butlast forms))))
