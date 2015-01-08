(ns css-parser.combinators
  (:require [css-parser.monad :refer :all]
            [css-parser.parse :as parse]))

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
