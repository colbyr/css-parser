(ns css-parser.css
  (:require [css-parser.match :as match]
            [css-parser.parse :as parse]))

(defn parse-css [css]
  (parse/all match/ruleset css))
