(ns clj-enum.error
  (:use midje.sweet)
  (:require [clj-enum.monad :as m]))

(defrecord ErrorT [base-monad run-errort]
  m/Monad
  (return [_ x]
    (ErrorT. base-monad (->Right x)))
  (bind [_ f]
    (ErrorT. base-monad
             (m/bind run-errort
                     (fn [a]
                       (if (e/left? a)
                         (m/return base-monad a)
                         (:run-errort (f (:right a))))))))
  m/MonadError
  (throw-error [_ message]
    (ErrorT. base-monad (m/return base-monad (e/->Left message))))
  (catch-error [_ handler]
    (ErrorT. base-monad
             (m/bind run-errort
                     (fn [a]
                       (if (e/left? a)
                         (:run-errort (handler (:left a)))
                         (m/return base-monad a)))))))

(defn return [base-monad x]
  (m/return (ErrorT. base-monad nil) x))

(defn throw-error [base-monad message]
  (m/throw-error (ErrorT. base-monad nil) message))