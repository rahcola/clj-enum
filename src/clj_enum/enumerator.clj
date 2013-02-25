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
  (fact "yields if the iteratee yields"
    (let [result (eof (i/->Yield ...val... ...lc...))]
      result => i/done?
      (fact "with the value of the iteratee"
        (:value result) => ...val...)
      (fact "with no leftovers of a chunk"
        (:leftover-chunk result) => s/eof)))

  (fact "breaks if the iteratee breaks"
    (let [result (eof (i/->Break ...msg...))]
      result => i/broken?
      (fact "with the message of the iteratee"
        (:message result) => ...msg...)))

  (fact "breaks if a running iteratee continues after EOF"
    (let [result (eof (i/->Continue (i/->Continue ...y... ...c...) ...f...))]
      result => i/broken?
      (fact "with a message"
        (:message result) => "diverging iteratee")))

  (fact "yields if a running iteratee yields after EOF"
    (let [result (eof (i/->Continue (i/->Yield ...val... ...lc...) ...f...))]
      result => i/done?
      (fact "with the value of the iteratee"
        (:value result) => ...val...)
      (fact "with no leftovers of a chunk"
        (:leftover-chunk result) => s/eof)))

  (fact "breaks if a running iteratee breaks after EOF"
    (let [result (eof (i/->Continue (i/->Break ...msg...) ...f...))]
      result => i/broken?
      (fact "with the message of the iteratee"
        (:message result) => ...msg...))))