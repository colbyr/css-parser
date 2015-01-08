(ns css-parser.match
  (:require [clojure.string :refer [trim]]
            [css-parser.monad :refer :all]
            [css-parser.combinators :refer :all]))

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

(def space
  "recognizes space (or newline)"
  (or-else (character " ") (character "\n")))

(def spaces
  "recognizes empty string or arbitrary number of spaces"
  (many space))

(defn string [s]
  "recognizes given string, i.e. \"clojure\""
  (reduce and-then (map #(character (str %)) s)))

(def letter+ (or-else letter (character "-")))

; CSS Gramar

(defrecord Rule [key value])
(def rule (do*
           (rule-name <- (many (character-omit ":")))
           (character ":")
           spaces
           (rule-value <- (many (character-omit ";")))
           (character ";")
           spaces
           (return (Rule. (apply str rule-name) (apply str rule-value)))))

(defrecord Ruleset [selector rules])
(def ruleset (do*
                 (selector <- (plus (character-omit "{")))
                 (character "{")
                 spaces
                 (rules <- (plus rule))
                 spaces
                 (character "}")
                 spaces
                 (return (Ruleset. (trim (apply str selector)) rules))))
