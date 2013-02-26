(ns clj-enum.eithert
  (:require [clj-enum.either :as e])
  (:require [clj-enum.monad :as m]))

(defrecord EitherT [run]
  m/Monad
  (return [_ x]
    (EitherT.
     (m/return run (e/right x))))
  (bind [_ f]
    (EitherT.
     (m/bind run (fn [x]
                   (if (e/right? x)
                     (:run (f (:right x)))
                     (m/return run x))))))
  m/MonadError
  (throw-error [_ error]
    (EitherT. (m/return run (e/left error))))
  (catch-error [_ handler]
    (EitherT. (m/bind run (fn [x]
                            (if (e/left? x)
                              (:run (handler (:left x)))
                              (m/return run x)))))))