(ns clj-enum.either
  (:use midje.sweet)
  (:require [clj-enum.monad :as m]))

(defprotocol Either
  (left? [self])
  (right? [self]))

(defrecord Left [left]
  Either
  (left? [_] true)
  (right? [_] false)
  m/MonadError
  (throw-error [_ error] (Left. error))
  (catch-error [_ handler] (handler left)))

(defrecord Right [right]
  Either
  (left? [_] false)
  (right? [_] true)
  m/MonadError
  (throw-error [_ error] (Left. error))
  (catch-error [self _] self))

(extend-type Right
  m/Monad
  (return [_ x] (Right. x))
  (bind [self f] (f (:right self))))

(extend-type Left
  m/Monad
  (return [_ x] (Right. x))
  (bind [self _] self))

(defn right [x]
  (Right. x))

(defn left [x]
  (Left. x))

(facts "Either"
  (fact "value in either monad is right"
    (m/return (right nil) ...x...) => (right ...x...))
  (fact "left then left is the first left"
    (m/bind (left ...x...) (fn [_] (left ...y...))) => (left ...x...))
  (fact "left then right is the first left"
    (m/bind (left ...x...) (fn [_] (right ...y...))) => (left ...x...))
  (fact "right then right is the second right"
    (m/bind (right ...x...) (fn [_] (right ...y...))) => (right ...y...))
  (fact "right then left is the second left"
    (m/bind (right ...x...) (fn [_] (left ...y...))) => (left ...y...))
  (fact "value of first right is passed to the function"
    (m/bind (right ...x...) (fn [x] (right x))) => (right ...x...)))