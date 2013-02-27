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

(facts "Enumerator"
  (fact "is a function"
    (->AEnumerator ...f...) => fn?))

(def eof
  (->AEnumerator
   (fn [iteratee]
     (if (i/continue? iteratee)
       (let [iteratee' (iteratee s/eof)]
         (if (i/continue? iteratee')
           (i/*handle-error* "diverging iteratee")
           (recur iteratee')))
       (i/->Yield (:value iteratee) s/eof)))))

(facts "eof"
  (fact "yields given an iteratee that is a continue that yields"
    (let [result (eof (i/->Continue (fn [s] (i/->Yield .value. .chunk.))))]
      result => i/yield?
      (fact "with the value of the inner yield"
        (:value result) => .value.)
      (fact "with no chunks"
        (:chunk result) => s/eof)))
  
  (fact "yields give an iteratee that is a yield"
    (let [result (eof (i/->Yield .value. .chunk.))]
      result => i/yield?
      (fact "with the value of the iteratee"
        (:value result) => .value.)
      (fact "with no chunks"
        (:chunk result) => s/eof)))

  (fact "calls *handle-error* if iteratee does not yield given eof"
    (binding [i/*handle-error* (fn [error] error)]
      (let [result (eof (i/->Continue (fn [s] (i/->Continue .f.))))]
        result => "diverging iteratee"))))

(defn enumerate-sequence [chunk-length sequence]
  (->AEnumerator
   (fn [iteratee]
     (if (and (not (empty? sequence))
              (i/continue? iteratee))
       (let [[chunk rest] (split-at chunk-length sequence)]
         ((enumerate-sequence chunk-length rest) (iteratee (s/chunk chunk))))
       iteratee))))

(facts "enumerate-sequence"
  (fact "yields given an yield"
    (let [result ((enumerate-sequence .n. .s.) (i/->Yield .value. .chunk.))]
      result => i/yield?
      (fact "with the value of the yield"
        (:value result) => .value.)
      (fact "with the chunk of the yield"
        (:chunk result) => .chunk.)))

  (fact "yields given a continue that yields"
    (let [i (i/->Continue (fn [s] (i/->Yield .value. .chunk.)))
          result ((enumerate-sequence 1 [.x.]) i)]
      result => i/yield?
      (fact "with the value of the yield"
        (:value result) => .value.)
      (fact "with the chunk of the yield"
        (:chunk result) => .chunk.)))

  (fact "returns with given iteratee if sequence is empty"
    (let [result ((enumerate-sequence .n. []) .iteratee.)]
      result => .iteratee.))

  (fact "passes chunk-length elements of the sequence to a continue"
    (let [i (i/->Continue (fn [s] (i/->Yield (:data s) .chunk.)))
          result ((enumerate-sequence 3 [1 2 3 4 5]) i)]
      (:value result) => [1 2 3])))

(defn >>> [first & rest]
  (reduce sequence-enumerators first rest))

(defn run [iteratee]
  (let [i (eof iteratee)]
    (cond (i/yield? i) (:value i)
          :else (i/*handle-error* "diverging iteratee"))))