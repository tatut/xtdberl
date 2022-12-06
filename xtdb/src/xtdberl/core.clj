(ns xtdberl.core
  (:require [xtdb.api :as xt]
            [xtdberl.term :refer [->erl ->clj] :as term]
            [clojure.tools.logging :as log])
  (:import (com.ericsson.otp.erlang
            OtpNode OtpMbox
            OtpErlangAtom
            OtpErlangDecodeException
            OtpErlangExit
            OtpErlangPid OtpErlangObject)

           (java.util.concurrent
            Executors ExecutorService)))

(set! *warn-on-reflection* true)

;; FIXME: proper startup with args
;;
;; Now just defining things with defaults to get started quickly
;;

(defonce xtdb (delay (xt/start-node {})))

(defonce inbox
  (delay
    (let [node (OtpNode. "xtdb@localhost")]
      (.createMbox node "xtdb"))))

(defonce executor-service
  (delay (Executors/newFixedThreadPool 20)))

;;(def node (OtpNode. "heppa@localhost"))
;;(def mbox (.createMbox node "xtdb"))

;;(.send mbox "foo@localhost" (OtpErlangAtom. "joopajoo"))

;;(.ping node "foo@localhost" 1000)
;;(def pid (.receive mbox))

;;(.send mbox pid (OtpErlangAtom. "sulle_takas"))

(defmulti handle
  "Handle a message. Message is a vector of tuple elements and dispatch on the
  first element."
  (fn [_xtdb _mbox [msg-type & _]] msg-type))

(defn send! [^OtpMbox mbox ^OtpErlangPid to & tuple-elements]
  (let [^OtpErlangObject msg (->erl (apply term/tuple tuple-elements))]
    (println "Sending " to ", msg: " msg)
    (def *out msg)
    (.send mbox to msg)))

(defmethod handle :q [xtdb mbox [_ from-pid query-id query args]]
  (println "Q, from: " from-pid ", id: " query-id ", q: " query ", args: " args)
  (try
    (let [result (apply xt/q (xt/db xtdb) query args)]
      (send! mbox from-pid :ok query-id result)
      ))

  (send! mbox from-pid
         query-id "response-to" args))

(defn server
  "Main server loop, reads commands and dispatches them to executor pool."
  [xtdb ^OtpMbox mbox ^ExecutorService executor-service]
  (letfn [(recv [] (->clj (.receive mbox)))]
    (loop [msg (recv)]
      (println "Received " msg)
      (def *msg msg)
      (.submit executor-service
               ^Runnable #(try
                            (if (term/tuple? msg)
                              (handle xtdb mbox (:elements msg))
                              (log/warn "Received unrecognized message, ignoring: " msg))
                            (catch Throwable t
                              (println "thrown: " t)
                              (def *t t)
                              (log/warn t "Exception in message handler, msg: " msg))))
      (recur (recv)))))

(defonce xtdb-server
  (delay (.start (Thread. #(server @xtdb @inbox @executor-service)))))
