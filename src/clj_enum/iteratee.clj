(ns clj-enum.iteratee
  (:use midje.sweet)
  (:require [clj-enum.stream :as s]))

(defprotocol Iteratee
  (yield? [self])
  (continue? [self])
  (broken? [self]))

(defrecord Yield [value chunk]
  Iteratee
  (yield? [_] true)
  (continue? [_] false)
  (broken? [_] false))

(facts "Yield"
  (let [i (->Yield ...val... ...c...)]
    (fact "is yield"
      i => yield?)
    (fact "is not continue"
      i =not=> continue?)
    (fact "is not broken"
      i =not=> broken?)

    (fact "has a value"
      (contains? i :value) => true
      (:value i) => ...val...)
    (fact "has a chunk"
      (contains? i :chunk) => true
      (:chunk i) => ...c...)))

(deftype Continue [k]
  Iteratee
  (yield? [_] false)
  (continue? [_] true)
  (broken? [_] false)
  clojure.lang.Fn
  clojure.lang.IFn
  (invoke [_ arg]
    (k arg))
  (applyTo [self args]
    (clojure.lang.AFn/applyToHelper self args)))

(facts "Continue"
  (let [i (->Continue ...f...)]
    (fact "is not yield"
      i =not=> yield?)
    (fact "is continue"
      i => continue?)
    (fact "is not broken"
      i =not=> broken?)

    (fact "is a function"
      i => fn?)))

(defrecord Broken [error]
  Iteratee
  (yield? [_] false)
  (continue? [_] false)
  (broken? [_] true))

(facts "Broken"
  (let [i (->Broken ...f...)]
    (fact "is not yield"
      i =not=> yield?)
    (fact "is not continue"
      i =not=> continue?)
    (fact "is broken"
      i => broken?)))

(def consume
  (let [step (fn step [acc s]
               (if (s/depleted? s)
                 (->Yield acc s)
                 (->Continue (partial step (concat acc (:data s))))))]
    (->Continue (fn [s] (step [] s)))))

(fact "consume"
  (fact "is a continue"
    consume => continue?)
  (fact "yields given an eof"
    (let [result (consume s/eof)]
      result => yield?
      (fact "with an empty sequence"
        (:value result) => empty?)
      (fact "with no chunks"
        (:chunk result) => s/eof)))
  (fact "continues given a flowing stream"
    (consume (s/chunk [...x...])) => continue?))

;; (bind [_ f]
;;     (Iteratee.
;;      (m/bind run
;;              (fn [step]
;;                (if (continue? step)
;;                  (m/return run (Continue. (fn [y] (m/bind (step y) f))))
;;                  (let [chunk (:chunk step)
;;                        r (:run (f (:value step)))]
;;                    (if (and (s/flowing? chunk)
;;                             (empty? (:data chunk)))
;;                      r
;;                      (m/bind r
;;                              (fn [step']
;;                                (if (continue? step')
;;                                  (:run (step' chunk))
;;                                  (m/return r (Yield. (:value step')
;;                                                      chunk))))))))))))