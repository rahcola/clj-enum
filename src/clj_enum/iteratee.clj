(ns clj-enum.iteratee
  (:use midje.sweet)
  (:require [clj-enum.stream :as s])
  (:require [clj-enum.functor :as f]))

(defprotocol Iteratee
  (yield? [self])
  (continue? [self])
  (broken? [self]))

(defrecord Yield [value chunk]
  Iteratee
  (yield? [_] true)
  (continue? [_] false)
  (broken? [_] false)
  f/Functor
  (fmap [_ f]
    (Yield. (f value) chunk)))

(facts "Yield"
  (let [i (->Yield ...val... ...c...)]
    (fact "is yield"
      (yield? i) => true)
    (fact "is not continue"
      (continue? i) => false)
    (fact "is not broken"
      (broken? i) => false)

    (fact "has a value"
      (contains? i :value) => true
      (:value i) => ...val...)
    (fact "has a chunk"
      (contains? i :chunk) => true
      (:chunk i) => ...c...)))

(defrecord Continue [k]
  Iteratee
  (yield? [_] false)
  (continue? [_] true)
  (broken? [_] false)
  f/Functor
  (fmap [_ f]
    (Continue. (fn [x] (f/fmap (k x) f))))
  clojure.lang.Fn
  clojure.lang.IFn
  (invoke [_ arg]
    (k arg))
  (applyTo [self args]
    (clojure.lang.AFn/applyToHelper self args)))

(facts "Continue"
  (let [i (->Continue ...f...)]
    (fact "is not yield"
      (yield? i) => false)
    (fact "is continue"
      (continue? i) => true)
    (fact "is not broken"
      (broken? i) => false)

    (fact "is a function"
      (fn? i) => true)))

(defrecord Broken [error]
  Iteratee
  (yield? [_] false)
  (continue? [_] false)
  (broken? [_] true)
  f/Functor
  (fmap [self _] self))

(facts "Broken"
  (let [i (->Broken ...f...)]
    (fact "is not yield"
      (yield? i) => false)
    (fact "is not continue"
      (continue? i) => false)
    (fact "is broken"
      (broken? i) => true)))

(def consume
  (let [step (fn step [acc s]
               (if (s/depleted? s)
                 (->Yield acc s)
                 (->Continue (partial step (concat acc (:data s))))))]
    (->Continue (fn [s] (step [] s)))))

(fact "consume"
  (fact "is a continue"
    (continue? consume) => true)
  (fact "yields given an eof"
    (let [result (consume s/eof)]
      (yield? result) => true
      (fact "with an empty sequence"
        (:value result) => empty?)
      (fact "with no chunks"
        (:chunk result) => s/eof)))
  (fact "continues given a flowing stream"
    (continue? (consume (s/chunk [...x...]))) => true))

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