(ns clj-enum.enumerator
  (:use midje.sweet)
  (:require [clj-enum.iteratee :as i]
            [clj-enum.stream :as s]))

(deftype Enumerator [f]
  clojure.lang.Fn
  clojure.lang.IFn
  (invoke [_ iter]
    (f iter))
  (applyTo [self args]
    (clojure.lang.AFn/applyToHelper self args)))

(fact "Enumerator"
  (let [e (->Enumerator ...f...)]
    (fact "is a function"
      (fn? e) => true)))

(def eof
  (->Enumerator
   (fn [iter]
     (cond (i/running? iter)
           (let [iter (iter s/eof)]
             (if (i/running? iter)
               (i/->Break "diverging iteratee")
               (recur iter)))
           (i/done? iter)
           (i/->Yield (:value iter) s/eof)
           :else iter))))

(facts "eof"
  (let [i (i/->Yield ...val... ...lc...)]
    (fact "yields if iteratee yields"
      (let [result (EOF i)]
        result => i/done?
        (fact "with the value of the iteratee"
          (:value result) => ...val...)
        (fact "with no leftovers of a chunk"
          (:leftover-chunk result) => s/eof))))

  (let [i (i/->Break ...msg...)]
    (fact "breaks if the iteratee breaks"
      (let [result (EOF i)]
        result => i/broken?
        (fact "with the message of the iteratee"
          (:message result) => ...msg...))))

  (let [i (i/->Continue (i/->Continue ...y... ...c...) ...f...)]
    (fact "breaks if running iteratee continues with EOF"
      (let [result (EOF i)]
        result => i/broken?
        (:message result) => "diverging iteratee")))

  (let [i (i/->Continue (i/->Yield ...val... s/eof) ...f...)]
    (fact "yields if running iteratee yields with EOF"
      (let [result (EOF i)]
        result => i/done?
        (fact "with the value of the iteratee"
          (:value result) => ...val...)
        (fact "with no leftovers of a chunk"
          (:leftover-chunk result) => s/eof))))

  (let [i (i/->Continue (i/->Break ...msg...) ...f...)]
    (fact "breaks if running iteratee breaks with EOF"
      (let [result (EOF i)]
        result => i/broken?
        (:message result) => ...msg...))))