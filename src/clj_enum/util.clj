(ns clj-enum.util
  (:use midje.sweet))

(defn singleton? [xs]
  (and (not (empty? xs))
       (empty? (rest xs))))

(fact "list with one element is singleton?"
  (singleton? [...x...]) => true)
(fact "list with two elements is not singleton?"
  (singleton? [...x... ...y...]) => false)
(fact "empty list is not singleton?"
  (singleton? []) => false)

(defn exactly-one-true? [booleans]
  (singleton? (filter true? booleans)))

(fact "one true is exactly-one-true?"
  (exactly-one-true? [...x...]) => true
  (provided
    (true? ...x...) => true))
(fact "two trues are not exactly-one-true?"
  (exactly-one-true? [...x... ...y...]) => false
  (provided
    (true? ...x...) => true
    (true? ...y...) => true))
(fact "true and false are exactly-one-true?"
  (exactly-one-true? [...x... ...y...]) => true
  (provided
    (true? ...x...) => true
    (true? ...y...) => false))