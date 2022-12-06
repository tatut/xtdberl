(ns xtdberl.term
  "Convert between Clojure data and Erlang terms using the OtpErlang classes."
  (:import (com.ericsson.otp.erlang
            OtpErlangList OtpErlangTuple OtpErlangBinary OtpErlangAtom OtpErlangBoolean OtpErlangByte
            OtpErlangChar OtpErlangDouble OtpErlangFloat OtpErlangLong OtpErlangInt OtpErlangUInt
            OtpErlangShort OtpErlangUShort OtpErlangString OtpErlangObject OtpErlangPid OtpErlangPort
            OtpErlangRef))
  (:require [clojure.walk :as walk]))

(set! *warn-on-reflection* true)

(defprotocol ToClojure
  (->clj [x] "Convert item to Clojure data"))

(defprotocol ToErlang
  (->erl [x] "Convert item to Erlang object"))

(defrecord Tuple [elements])

(defn tuple? [x]
  (instance? Tuple x))

(defn tuple [& items]
  (->Tuple (vec items)))

(extend-protocol ToClojure
  OtpErlangAtom
  (->clj [x]
    (let [s (.atomValue x)]
      (case s
        "true" true
        "false" false
        "undefined" nil

        (if (= (.charAt s 0) \:)
          (keyword (subs s 1))
          (symbol s)))))

  OtpErlangList
  (->clj [x]
    (mapv ->clj (iterator-seq (.iterator x))))

  OtpErlangTuple
  (->clj [x]
    (->Tuple (mapv ->clj (.elements x))))

  OtpErlangPid
  (->clj [x] x)

  OtpErlangRef
  (->clj [x] x)

  OtpErlangString
  (->clj [x]
    (.stringValue x))

  OtpErlangDouble
  (->clj [x]
    (.doubleValue x))

  OtpErlangLong
  (->clj [x]
    (.longValue x)))

(declare ->erl)

(defn ^"[Lcom.ericsson.otp.erlang.OtpErlangObject;" otp-array [things]
  (into-array OtpErlangObject (map ->erl things)))

(defn ^"[Lcom.ericsson.otp.erlang.OtpErlangObject;" otp-array-raw [things]
  (into-array OtpErlangObject things))

(extend-protocol ToErlang
  Boolean
  (->erl [x]
    (OtpErlangAtom. (str x)))

  clojure.lang.Keyword
  (->erl [x]
    (OtpErlangAtom. (str x)))

  clojure.lang.Symbol
  (->erl [x]
    (OtpErlangAtom. (name x)))

  java.util.Map
  (->erl [x]
    ;; Output maps as orddicts, eg [{key1, "value}, ...]
    (OtpErlangList.
     (otp-array-raw
      (for [key-val x]
        (OtpErlangTuple. (otp-array key-val))))))

  java.util.List
  (->erl [x]
    (OtpErlangList. (otp-array x)))

  java.util.Set
  (->erl [x]
    ;; Output sets as just lists
    (OtpErlangList. (otp-array x)))

  String
  (->erl [x]
    (OtpErlangString. x))

  Long
  (->erl [x]
    (OtpErlangLong. x))

  Double
  (->erl [x]
    (OtpErlangDouble. x))

  Tuple
  (->erl [x]
    (OtpErlangTuple. (otp-array (:elements x))))

  OtpErlangPid
  (->erl [x] x)

  OtpErlangRef
  (->erl [x] x)

  nil
  (->erl [_] (OtpErlangAtom. "undefined")))

(defn unwrap-tuples
  "Deeply unwrap tuple elements to something else, defaults to vector."
  ([form] (unwrap-tuples form vec))
  ([form with]
   (walk/postwalk
    (fn [x]
      (if (tuple? x)
        (with (:elements x))
        x))
    form)))
