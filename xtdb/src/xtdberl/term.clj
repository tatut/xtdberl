(ns xtdberl.term
  "Convert between Clojure data and Erlang terms using the OtpErlang classes."
  (:import (com.ericsson.otp.erlang
            OtpErlangList OtpErlangTuple OtpErlangBinary OtpErlangAtom OtpErlangBoolean OtpErlangByte
            OtpErlangChar OtpErlangDouble OtpErlangFloat OtpErlangLong OtpErlangInt OtpErlangUInt
            OtpErlangShort OtpErlangUShort OtpErlangString OtpErlangObject OtpErlangPid OtpErlangPort
            OtpErlangRef OtpErlangMap OtpErlangBinary))
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

(defmulti parse-tuple (fn [elements] (first elements)))
(defmethod parse-tuple :default [elements]
  (->Tuple (mapv ->clj elements)))

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
    (parse-tuple (seq (.elements x))))

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
    (.longValue x))

  OtpErlangMap
  (->clj [x]
    (zipmap (map ->clj (.keys x))
            (map ->clj (.values x)))))

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

  java.lang.Integer
  (->erl [x]
    (OtpErlangInt. x))

  java.util.Map
  (->erl [x]
    (let [entries (seq x)]
      (OtpErlangMap. (otp-array (map key entries))
                     (otp-array (map val entries)))))

  java.util.Date
  (->erl [x]
    (OtpErlangTuple. (otp-array-raw [(OtpErlangAtom. "timestamp")
                                     (OtpErlangLong. (.getTime x))])))
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

  java.time.LocalDate
  (->erl [x]
    (OtpErlangTuple. (otp-array-raw [(OtpErlangAtom. "local_date")
                                     (OtpErlangLong. (.getYear x))
                                     (OtpErlangLong. (.getValue (.getMonth x)))
                                     (OtpErlangLong. (.getDayOfMonth x))])))

  java.time.LocalDateTime
  (->erl [x]
    (OtpErlangTuple. (otp-array-raw [(OtpErlangAtom. "local_datetime")
                                     (OtpErlangLong. (.getYear x))
                                     (OtpErlangLong. (.getValue (.getMonth x)))
                                     (OtpErlangLong. (.getDayOfMonth x))
                                     (OtpErlangLong. (.getHour x))
                                     (OtpErlangLong. (.getMinute x))
                                     (OtpErlangLong. (.getSecond x))])))

  java.util.UUID
  (->erl [x]
    (let [bb (java.nio.ByteBuffer/wrap (byte-array 16))]
      (.putLong bb (.getMostSignificantBits x))
      (.putLong bb (.getLeastSignificantBits x))
      (OtpErlangTuple. (otp-array-raw [(OtpErlangAtom. "uuid")
                                       (OtpErlangBinary. (.array bb))]))))

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

(defmethod parse-tuple (->erl 'local_date)
  [[_ ^OtpErlangLong year ^OtpErlangLong month ^OtpErlangLong date]]
  (java.time.LocalDate/of (.intValue year) (.intValue month) (.intValue date)))

(defmethod parse-tuple (->erl 'local_datetime)
  [[_
    ^OtpErlangLong year ^OtpErlangLong month ^OtpErlangLong date
    ^OtpErlangLong hour ^OtpErlangLong minute ^OtpErlangLong sec]]
  (java.time.LocalDateTime/of
   (.intValue year) (.intValue month) (.intValue date)
   (.intValue hour) (.intValue minute) (.intValue sec)))

(defmethod parse-tuple (->erl 'timestamp)
  [[_ ^OtpErlangLong ts]]
  (java.util.Date. (.longValue ts)))

(defmethod parse-tuple (->erl 'uuid)
  [[_ ^OtpErlangBinary bin]]
  (let [bb (java.nio.ByteBuffer/wrap (.binaryValue bin))
        most (.getLong bb)
        least (.getLong bb)]
    (java.util.UUID. most least)))
