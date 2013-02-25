(ns clj-enum.stream
  (:use midje.sweet))

(defprotocol Stream
  (flowing? [self])
  (depleted? [self]))

(defrecord AStream [tag data]
  Stream
  (flowing? [_] (= tag :chunk))
  (depleted? [_] (= tag :eof)))

(defn chunk [data]
  (AStream. :chunk data))

(def eof (AStream. :eof nil))

(facts "Chunk"
  (let [s (chunk ...x...)]
    (fact "is flowing"
      (flowing? s) => true)
    (fact "is not depleted"
      (depleted? s) => false)

    (fact "has data"
      (:data s) => ...x...)))

(facts "EOF"
  (fact "is not flowing"
    (flowing? eof) => false)
  (fact "is depleted"
    (depleted? eof) => true))