(ns css-parser.parser
  (:require [css-parser.match :refer [ruleset]]
            [css-parser.parse :as parse]))

(def css ".container h1 {
  color: rgba(255, 0, 0, 0.9);font-size: 24px;
  font-family: Monaco;
}

.test p {
  color: green;
}")

(parse/all ruleset css)
