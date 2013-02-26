(ns clj-enum.functor)

(defprotocol Functor
  (fmap [self f]))