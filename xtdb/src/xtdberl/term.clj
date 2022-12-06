(ns xtdberl.term
  "Convert between Clojure data and Erlang terms using the OtpErlang classes."
  (:import (com.ericsson.otp.erlang
            OtpErlangList OtpErlangTuple OtpErlangBinary OtpErlangAtom OtpErlangBoolean OtpErlangByte
            OtpErlangChar OtpErlangDouble OtpErlangFloat OtpErlangLong OtpErlangInt OtpErlangUInt
            OtpErlangShort OtpErlangUShort OtpErlangString OtpErlangObject OtpErlangPid OtpErlangPort
            OtpErlangRef)))

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
        (keyword s))))

  OtpErlangList
  (->clj [x]
    (map ->clj (iterator-seq (.iterator x))))

  OtpErlangTuple
  (->clj [x]
    (->Tuple (mapv ->clj (.elements x))))

  OtpErlangPid
  (->clj [x] x)

  OtpErlangRef
  (->clj [x] x)

  OtpErlangString
  (->clj [x]
    (.stringValue x)))

(declare ->erl)

(defn ^"[Lcom.ericsson.otp.erlang.OtpErlangObject;" otp-array [things]
  (into-array OtpErlangObject (map ->erl things)))

(extend-protocol ToErlang
  Boolean
  (->erl [x]
    (OtpErlangAtom. (str x)))

  clojure.lang.Keyword
  (->erl [x]
    (OtpErlangAtom. (name x)))

  java.util.List
  (->erl [x]
    (OtpErlangList. (otp-array x)))

  String
  (->erl [x]
    (OtpErlangString. x))

  Tuple
  (->erl [x]
    (OtpErlangTuple. (otp-array (:elements x))))

  OtpErlangPid
  (->erl [x] x)

  OtpErlangRef
  (->erl [x] x)

  nil
  (->erl [_] (OtpErlangAtom. "undefined")))
