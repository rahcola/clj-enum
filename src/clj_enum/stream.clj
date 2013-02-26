(ns clj-enum.stream
  (:refer-clojure :exclude [chunk])
  (:use midje.sweet))

(defprotocol Stream
  (flowing? [self])
  (depleted? [self]))

(defrecord Chunk [data]
  Stream
  (flowing? [_] true)
  (depleted? [_] false))

(deftype EOF []
  Stream
  (flowing? [_] false)
  (depleted? [_] true))

(defn chunk [data]
  (Chunk. data))

(def eof (EOF.))

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