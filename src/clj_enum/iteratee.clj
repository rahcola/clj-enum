(ns clj-enum.iteratee
  (:use midje.sweet)
  (:require [clj-enum.stream :as s])
  (:require [clj-enum.monad :as m])
  (:require [clj-enum.eithert :as et])
  (:require [clj-enum.functor :as f]))

(defprotocol Step
  (yield? [self])
  (continue? [self]))

(defrecord Yield [value chunk]
  Step
  (yield? [_] true)
  (continue? [_] false)
  f/Functor
  (fmap [_ f]
    (Yield. (f value) chunk)))

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
  (yield? [_] false)
  (continue? [_] true)
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

    (fact "is a function"
      (fn? i) => true)))

(defrecord Iteratee [run]
  f/Functor
  (fmap [_ f]
    (Iteratee.
     (m/bind run (fn [step] (m/return run (f/fmap step f))))))
  m/Monad
  (return [_ x]
    (Iteratee. (m/return run (Yield. x (s/chunk [])))))
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
     (yield data data chunk))
  ([monad data chunk]
     (Iteratee. (m/return (et/->EitherT monad) (Yield. data chunk)))))

(defn continue
  ([k]
     (continue :identity k))
  ([monad k]
     (Iteratee. (m/return (et/->EitherT monad) (Continue. k)))))