(ns clj-enum.monad)

(defprotocol Monad
  (return [_ x])
  (bind [_ f]))

(defprotocol MonadError
  (throw-error [_ error])
  (catch-error [_ handler]))

(extend-type Object
  Monad
  (return [_ x] x)
  (bind [m f] (f m)))

(defrecord Identity [x]
  Monad
  (return [_ y] (Identity. y))
  (bind [_ f] (f x)))

(def identity-m (Identity. nil))