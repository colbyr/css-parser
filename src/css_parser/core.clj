(ns css-parser.core
  (:require [css-parser.css :refer [parse-css]]))

(def css ".container h1 {
  color: rgba(255, 0, 0, 0.9);
  font-size: 24px;
  font-family: Monaco;
}
.test p {
  color: green;
}")

(parse-css css)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
