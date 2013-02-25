(ns clj-enum.monad)

(defprotocol Monad
  (return [_ x])
  (bind [_ f]))

(defprotocol MonadError
  (throw-error [_ error])
  (catch-error [_ handler]))

(deftype Identity []
  Monad
  (return [_ x] x)
  (bind [_ f] (f x)))

(def identity (Identity.))