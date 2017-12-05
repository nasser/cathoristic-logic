(ns cl
  (:refer-clojure :exclude [conj]))

(defn- symbol-set [s] 
  (Microsoft.FSharp.Collections.FSharpSet|`1[System.String]|. (into-array (map str s))))

(def top Cathoristic+Prop/Top)
(def bot Cathoristic+Prop/Bot)
(defn trans
  ([s] (trans (str s) top))
  ([s p] (Cathoristic+Prop/NewTrans (str s) p)))
(defn conj [p q] (Cathoristic+Prop/NewConj p q))
(defn bang [ss] (Cathoristic+Prop/NewBang (symbol-set ss)))
(defn entails? [a b] (Cathoristic/entails a b))

;; syntax

(def ∧ conj)
(defn ! [& ss] (bang ss))
(def ?= entails?)