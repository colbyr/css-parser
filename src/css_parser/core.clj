(ns css-parser.core
  (:require [css-parser.css :refer [parse-css]]))

(def css ".container h1 {
  // comments are fucking everywhere!
  color: rgba(255, 0, 0, 0.9);
  font-size: 24px;
  font-family: Monaco;
}
// comments and shit
.test p {
  color: green; //wat
}")

(parse-css css)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
