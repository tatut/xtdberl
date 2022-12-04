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

(extend-protocol ToClojure
  OtpErlangAtom
  (->clj [x]
    (let [s (.atomValue x)]
      (case s
        "true" true
        "false" false
        (keyword s))))

  OtpErlangList
  (->clj [x]
    (map ->clj (iterator-seq (.iterator x))))

  OtpErlangTuple
  (->clj [x]
    (->Tuple (mapv ->clj (.elements x)))))


(extend-protocol ToErlang
  Boolean
  (->erl [x]
    (OtpErlangAtom. (str x)))

  clojure.lang.Keyword
  (->erl [x]
    (OtpErlangAtom. (name x)))

  java.util.List
  (->erl [x]
    (OtpErlangList. (into-array OtpErlangObject
                                (map ->erl x))))

  Tuple
  (->erl [x]
    (OtpErlangTuple. (into-array OtpErlangObject (map ->erl (:elements x))))))
