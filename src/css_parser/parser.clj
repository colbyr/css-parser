(ns css-parser.parser
  (:require [clojure.string :refer [trim]]
            [css-parser.parse :as parse]))

; theory: type Parser a = String -> [(a, String)]

(defn any [input]
  (if (empty? input) '()
    (list [(first input)
           (apply str (rest input))])))

(defn failure [_] '())

(any "clojure-1.7")

; utilities

; monad stuff *waves hands*

(defn return [v]
  (fn [input] (list [v input])))

(defn >>= [m f]
  (fn [input]
    (->> input
         (parse/one m)
         (mapcat (fn [[v tail]] (parse/one (f v) tail))))))

;; haskell-ish do* macro

(defn merge-bind [body bind]
  (if (and (not= clojure.lang.Symbol (type bind))
           (= 3 (count bind))
           (= '<- (second bind)))
    `(>>= ~(last bind) (fn [~(first bind)] ~body))
    `(>>= ~bind (fn [~'_] ~body))))

(defmacro do* [& forms]
  (reduce merge-bind (last forms) (reverse (butlast forms))))

; basic parsers!

(defn sat
  "I have no idea what this does"
  [pred]
  (>>= any (fn [v] (if (pred v) (return v) failure))))

(defn char-compare
  "a helper for building boolean based parsers"
  [f]
  (fn [c] (sat (partial f (first c)))))

(def char-match
  "accepts the given character"
  (char-compare =))

(def char-omit
  "rejects the given character"
  (char-compare not=))

(defn re-compare
  "a helper for building regex based parsers"
  [re]
  (sat (fn[v] (not (nil? (re-find re (str v)))))))

(def digit-match
  "accepts any digit"
  (re-compare #"[0-9]"))

(def letter-match
  "accepts any letter"
  (re-compare #"[a-zA-Z]"))

; combinators

(defn and-then
  "(ab)"
  [p1 p2]
  (do*
   (r1 <- p1)
   (r2 <- p2)
   (return (str r1 r2))))

(defn or-else
  "(a|b)"
  [p1 p2]
  (fn [input]
    (lazy-cat (parse/one p1 input) (parse/one p2 input))))

(declare plus)
(declare optional)

(defn many
  "a*"
  [parser]
  (optional (plus parser)))

(defn plus
  "(a+) equal to (aa*)"
  [parser]
  (do*
   (a <- parser)
   (as <- (many parser))
   (return (cons a as))))

(defn optional
  "(a?)"
  [parser]
  (or-else parser (return "")))

; combinator based parsers

(def space-match
  "recognizes space (or newline)"
  (or-else (char-match " ") (char-match "\n")))

(def spaces-match
  "recognizes empty string or arbitrary number of spaces"
  (many space-match))

(defn string-match [s]
  "recognizes given string, i.e. \"clojure\""
  (reduce and-then (map #(char-match (str %)) s)))

; CSS Gramar

(defrecord Rule [key value])
(defrecord Ruleset [selector rules])

(def letter+-match (or-else letter-match (char-match "-")))

(def rule (do*
           (rule-name <- (many (char-omit ":")))
           (char-match ":")
           spaces-match
           (rule-value <- (many (char-omit ";")))
           (char-match ";")
           spaces-match
           (return (Rule. (apply str rule-name) (apply str rule-value)))))

(def stylesheet (do*
                 (selector <- (plus (char-omit "{")))
                 (char-match "{")
                 spaces-match
                 (rules <- (plus rule))
                 spaces-match
                 (char-match "}")
                 spaces-match
                 (return (Ruleset. (trim (apply str selector)) rules))))

(def css ".container h1 {
  color: rgba(255, 0, 0, 0.9);font-size: 24px;
  font-family: Monaco;
}

.test p {
  color: green;
}")

(parse/all stylesheet css)
