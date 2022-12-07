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

(defonce xtdb (delay (xt/start-node {:xtdb.lucene/lucene-store {}})))

(defonce inbox
  (delay
    (let [node (OtpNode. "xtdb@localhost")]
      (.createMbox node "xtdb"))))

(defonce executor-service
  (delay (Executors/newFixedThreadPool 20)))

(defmulti handle
  "Handle a message. Message is a vector of tuple elements and dispatch on the
  first element."
  (fn [_xtdb _mbox [msg-type & _]] msg-type))

(defn send! [^OtpMbox mbox ^OtpErlangPid to & tuple-elements]
  (let [^OtpErlangObject msg (->erl (apply term/tuple tuple-elements))]
    (def *out msg)
    (.send mbox to msg)))

(defmethod handle 'q [xtdb mbox [_ from-pid query-id query args]]
  (try
    (let [;; Convert tuples to lists (for operation calls)
          query (term/unwrap-tuples query #(apply list %))
          _ (def *q query)
          result (apply xt/q (xt/db xtdb) query args)]

      (send! mbox from-pid 'ok query-id result))
    (catch Exception e
      (log/warn e "Error in query")
      (send! mbox from-pid 'error query-id (ex-data e)))))

(defmethod handle 'put [xtdb mbox [_ from-pid msg-id & docs]]
  (def *docs docs)
  (try
    (let [{::xt/keys [tx-id tx-time]}
          (xt/submit-tx xtdb
                        (for [d (term/unwrap-tuples docs)]
                          [::xt/put (if (map? d)
                                      ;; Use map directly
                                      d
                                      ;; Turn orddict into a map
                                      (into {} d))]))]
      (send! mbox from-pid
             'ok msg-id (term/tuple tx-id tx-time)))
    (catch Exception e
      (log/warn e "Error in PUT")
      (send! mbox from-pid 'error msg-id (ex-data e)))))

(defmethod handle 'status [xtdb mbox [_ from-pid msg-id]]
  (send! mbox from-pid 'ok msg-id (xt/status xtdb)))

(defn server
  "Main server loop, reads commands and dispatches them to executor pool."
  [xtdb ^OtpMbox mbox ^ExecutorService executor-service]
  (letfn [(recv []
            (try
              (->clj (.receive mbox))
              (catch Throwable t
                (println "ERROR converting received to clj data: " t))))]
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

(comment
  (xt/submit-tx @xtdb [[::xt/put {:xt/id {:person "555444222"}
                                  :person/first-name "Snöwmän"
                                  :person/last-name "Unicode ☃"
                                  :person/email "snowman.unicode@example.com"
                                  :person/date-of-birth (java.time.LocalDate/of 1900 1 1)}]])
  (xt/submit-tx @xtdb [[::xt/put {:xt/id "hep1" :name "hep" :jotain 42 :ok? true}]
                       [::xt/put {:xt/id "hep2" :name "hep" :jotain 666 :ok? false :muuta :tietoja}]])
  (xt/submit-tx @xtdb [[::xt/put {:xt/id "hep3" :name "heppa" :jotain 4211 :ok? true}]]))
