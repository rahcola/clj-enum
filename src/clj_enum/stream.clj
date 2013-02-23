(ns clj-enum.stream
  (:use midje.sweet))

(defprotocol Stream
  (flowing? [self])
  (depleted? [self]))

(defrecord Chunk [data]
  Stream
  (flowing? [_] true)
  (depleted? [_] false))

(facts "Chunk"
  (let [s (->Chunk ...x...)]
    (fact "is flowing"
      (flowing? s) => true)
    (fact "is not depleted"
      (depleted? s) => false)

    (fact "has data"
      (contains? s :data) => true)))

(deftype EOF []
  Stream
  (flowing? [_] false)
  (depleted? [_] true))

(def eof (->EOF))

(facts "EOF"
  (let [s (->EOF)]
    (fact "is not flowing"
      (flowing? s) => false)
    (fact "is depleted"
      (depleted? s) => true)))