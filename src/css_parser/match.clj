(ns css-parser.match
  (:require [clojure.string :refer [trim]]
            [css-parser.combinators :refer :all]
            [css-parser.monad :refer :all]
            [css-parser.parse :as parse]))

(defn- character-compare
  "a helper for building boolean based parsers"
  [f]
  (fn [c] (sat (partial f (first c)))))

(def character
  "accepts the given character"
  (character-compare =))

(def character-omit
  "rejects the given character"
  (character-compare not=))

(defn- re-compare
  "a helper for building regex based parsers"
  [re]
  (sat (fn[v] (not (nil? (re-find re (str v)))))))

(def digit
  "accepts any digit"
  (re-compare #"[0-9]"))

(def letter
  "accepts any letter"
  (re-compare #"[a-zA-Z]"))

(def new-line
  "accepts a new line character, i.e. '\n'"
  (character "\n"))

(def space (character " "))

(def spaces (many space))

(def white-space (or-else space new-line))

(def white-spaces (many white-space))

(defn string [s]
  "recognizes given string, i.e. \"clojure\""
  (reduce and-then (map #(character (str %)) s)))

; CSS Gramar

(def single-line-comment-head
  (and-then
   (character "/")
   (character "/")))

(def single-line-comment-tail
  (and-then
   (many (character-omit "\n"))
   new-line))

(def single-line-comment
  (and-then
   single-line-comment-head
   single-line-comment-tail))

(def white-space-and-comments
  (many
   (or-else
    single-line-comment
    white-space)))

(defrecord Rule [key value])
(def rule
  (do*
   (rule-name <- (many (character-omit ":")))
   (character ":")
   white-spaces
   (rule-value <- (many (character-omit ";")))
   (character ";")
   white-space-and-comments
   (return (Rule. (apply str rule-name) (apply str rule-value)))))

(defrecord Ruleset [selector rules])
(def ruleset
  (do*
   (selector <- (plus (character-omit "{")))
   (character "{")
   white-space-and-comments
   (rules <- (plus rule))
   white-space-and-comments
   (character "}")
   white-space-and-comments
   (return (Ruleset. (trim (apply str selector)) rules))))


(defrecord Stylesheet [rulesets])
(def stylesheet
  (do*
   (rulesets <- (many ruleset))
   (return (Stylesheet. rulesets))))
