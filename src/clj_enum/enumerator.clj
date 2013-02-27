(ns clj-enum.enumerator
  (:use midje.sweet)
  (:require [clj-enum.iteratee :as i])
  (:require [clj-enum.stream :as s]))

(defprotocol Enumerator
  (sequence-enumerators [first then]))

(deftype AEnumerator [f]
  Enumerator
  (sequence-enumerators [_ then]
    (AEnumerator.
     (fn [iteratee]
       (then (f iteratee)))))
  clojure.lang.Fn
  clojure.lang.IFn
  (invoke [_ iteratee]
    (f iteratee))
  (applyTo [self args]
    (clojure.lang.AFn/applyToHelper self args)))

(fact "Enumerator"
  (let [e (->AEnumerator ...f...)]
    (fact "is a function"
      (fn? e) => true)))

(def eof
  (->AEnumerator
   (fn [iteratee]
     (cond (i/continue? iteratee)
           (let [iteratee' (iteratee s/eof)]
             (if (i/continue? iteratee')
               (i/->Broken "diverging iteratee")
               (recur iteratee')))
           (i/yield? iteratee)
           (i/->Yield (:value iteratee) s/eof)
           :else
           iteratee))))

(facts "eof"
  (fact "yields if the iteratee is a continue that yields"
    (let [result (eof (i/->Continue (fn [s] (i/->Yield ...val... ...lc...))))]
      (i/yield? result) => true
      (fact "with the value of the inner yield"
        (:value result) => ...val...)
      (fact "with no chunks"
        (:chunk result) => s/eof)))
  
  (fact "brakes if the iteratee is a continue that does not yields"
    (let [result (eof (i/->Continue (fn [s] (i/->Continue ...f...))))]
      (i/broken? result) => true
      (fact "with error \"diverging iteratee\""
        (:error result) => "diverging iteratee")))
  
  (fact "yields if the iteratee is a yield"
    (let [result (eof (i/->Yield ...val... ...lc...))]
      (i/yield? result) => true
      (fact "with the value of the iteratee"
        (:value result) => ...val...)
      (fact "with no chunks"
        (:chunk result) => s/eof)))

  (fact "brakes if the iteratee is broken"
    (let [result (eof (i/->Broken ...error...))]
      (i/broken? result) => true
      (fact "with error of the iteratee"
        (:error result) => ...error...))))

(defn enumerate-sequence [chunk-length sequence]
  (->AEnumerator
   (fn [iteratee]
     (if (and (not (empty? sequence))
              (i/continue? iteratee))
       (let [[chunk rest] (split-at chunk-length sequence)]
         ((enumerate-sequence chunk-length rest) (iteratee (s/chunk chunk))))
       iteratee))))

(defn >>> [first & rest]
  (reduce sequence-enumerators first rest))