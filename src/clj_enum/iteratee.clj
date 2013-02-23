(ns clj-enum.iteratee
  (:use midje.sweet)
  (:require [clj-enum.stream :as s]))

(defprotocol Iteratee
  (done? [self])
  (running? [self])
  (broken? [self]))

(defrecord Yield [value leftover-chunk]
  Iteratee
  (done? [_] true)
  (running? [_] false)
  (broken? [_] false))

(facts "Yield"
  (let [i (->Yield ...val... ...lc...)]
    (fact "is done"
      (done? i) => true)
    (fact "is not running"
      (running? i) => false)
    (fact "is not broken"
      (broken? i) => false)

    (fact "has a value"
      (contains? i :value) => true
      (:value i) => ...val...)
    (fact "has leftovers of a chunk"
      (contains? i :leftover-chunk) => true
      (:leftover-chunk i) => ...lc...)))

(deftype Continue [on-eof with-data]
  Iteratee
  (done? [_] false)
  (running? [_] true)
  (broken? [_] false)
  clojure.lang.Fn
  clojure.lang.IFn
  (invoke [_ x]
    (if (s/depleted? x)
      on-eof
      (with-data x)))
  (applyTo [self args]
    (clojure.lang.AFn/applyToHelper self args)))

(facts "Continue"
  (let [i (->Continue ...e... identity)]
    (fact "is not done"
      (done? i) => false)
    (fact "is running"
      (running? i) => true)
    (fact "is not broken"
      (broken? i) => false)

    (fact "is a function"
      (fn? i) => true)

    (fact "returns on-eof when given eof"
      (i s/eof) => ...e...)
    (fact "continues with-data if given data"
      (i ...data...) => ...data...
      (provided
        (s/depleted? ...data...) => false))))

(defrecord Break [message]
  Iteratee
  (done? [_] false)
  (running? [_] false)
  (broken? [_] true))

(facts "Break"
  (let [i (->Break ...msg...)]
    (fact "is not done"
      (done? i) => false)
    (fact "is not running"
      (running? i) => false)
    (fact "is broken"
      (broken? i) => true)

    (fact "has a message"
      (contains? i :message) => true
      (:message i) => ...msg...)))