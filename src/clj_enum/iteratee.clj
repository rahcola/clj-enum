(ns clj-enum.iteratee
  (:use midje.sweet)
  (:require [clj-enum.stream :as s])
  (:require [clj-enum.monad :as m])
  (:require [clj-enum.eithert :as et]))

(defprotocol Step
  (yield? [self])
  (continue? [self]))

(defrecord Yield [value chunk]
  Step
  (yield? [_] true)
  (continue? [_] false))

(facts "Yield"
  (let [i (->Yield ...val... ...c...)]
    (fact "is yield"
      (yield? i) => true)
    (fact "is not continue"
      (continue? i) => false)

    (fact "has a value"
      (contains? i :value) => true
      (:value i) => ...val...)
    (fact "has a chunk"
      (contains? i :chunk) => true
      (:chunk i) => ...c...)))

(defrecord Continue [k]
  Step
  (yield? [_] true)
  (continue? [_] false)
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

    (fact "is a function"
      (fn? i) => true)))

(defrecord Iteratee [run]
  m/Monad
  (return [_ x]
    (Iteratee. (m/return run (Yield. data (s/chunk [])))))
  (bind [_ f]
    (Iteratee.
     (m/bind run
             (fn [step]
               (if (continue? step)
                 (m/return run (Continue. (fn [y] (m/bind (step y) f))))
                 (let [r (:run (f (:value step)))
                       chunk (:chunk step)]
                   (if (empty? chunk)
                     r
                     (m/bind r (fn [step']
                                 (if (continue? step')
                                   (:run (step' chunk))
                                   (m/return r (Yield. (:value step')
                                                       chunk)))))))))))))

(defn yield
  ([data chunk]
     (yield m/identity data chunk))
  ([monad data chunk]
     (Iteratee. (m/return (et/->Either monad) (Yield. data chunk)))))

(defn continue
  ([k]
     (continue m/identity k))
  ([monad k]
     (Iteratee. (m/return (et/->Either monad) (Continue. k)))))