(ns xtdberl.core
  (:require [xtdb.api :as xt]
            [xtdberl.term :refer [->erl ->clj] :as term]
            xtdberl.logging
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

(defmulti handle
  "Handle a message. Message is a vector of tuple elements and dispatch on the
  first element."
  (fn [_xtdb _mbox [msg-type & _]] msg-type))

(defn send! [^OtpMbox mbox ^OtpErlangPid to & tuple-elements]
  (let [^OtpErlangObject msg (->erl (apply term/tuple tuple-elements))]
    (def *out msg)
    (.send mbox to msg)))

(defn- options->db-basis [{:keys [tx_time tx_id valid_time]}]
  (merge
   (when valid_time
     {::xt/valid-time valid_time})
   (when (or tx_time tx_id)
     {::xt/tx
      (merge
       (when tx_time
         {::xt/tx-time tx_time})
       (when tx_id
         {::xt/tx-id tx_id}))})))

(defmethod handle 'q [xtdb mbox [_ from-pid query-id options query args :as msg]]
  (def *msg msg)
  (try
    (let [;; Convert tuples to lists (for operation calls)
          query (term/unwrap-tuples query #(apply list %))
          _ (def *q query)
          _ (def *xtdb xtdb)
          opts (term/orddict->map options)
          _ (def *opts opts)
          result (apply xt/q (xt/db xtdb (options->db-basis opts)) query args)]

      (send! mbox from-pid 'ok query-id result))
    (catch Exception e
      (log/warn e "Error in query")
      (send! mbox from-pid 'error query-id (ex-data e)))))

(defmulti tx-ops (fn [cmd _args] cmd))

(defmethod tx-ops 'put [_ docs]
  (for [d (term/unwrap-tuples docs)]
    [::xt/put (if (map? d)
                ;; Use map directly
                d
                ;; Turn orddict into a map
                (into {} d))]))

(defmethod handle 'put [xtdb mbox [_ from-pid msg-id docs :as put]]
  (def *put put)
  (try
    (let [{::xt/keys [tx-id tx-time]}
          (xt/submit-tx xtdb
                        (tx-ops 'put docs))]
      (send! mbox from-pid
             'ok msg-id (term/tuple tx-id tx-time)))
    (catch Exception e
      (log/warn e "Error in PUT")
      (send! mbox from-pid 'error msg-id (ex-data e)))))

(defmethod handle 'status [xtdb mbox [_ from-pid msg-id]]
  (send! mbox from-pid 'ok msg-id (xt/status xtdb)))

(defmethod handle 'batch [xtdb mbox [_ from-pid msg-id ops :as msg]]
  (def *batch msg)
  (let [{::xt/keys [tx-id tx-time] :as res}
        (xt/submit-tx xtdb
                      (mapcat (fn [{[type args] :elements}]
                                (tx-ops type args))
                              ops))]
    (log/debug "Batch op done: " res)
    (send! mbox from-pid 'ok msg-id (term/tuple tx-id tx-time))))

(defn- server-loop
  "Main server loop, reads commands and dispatches them to executor pool."
  [xtdb ^OtpMbox mbox ^ExecutorService executor-service]
  (log/info "xtdberl server mailbox started, mbox = " mbox)
  (letfn [(recv []
            (try
              (->clj (.receive mbox))
              (catch Throwable t
                (log/warn t "ERROR converting received to clj data"))))]
    (loop [msg (recv)]
      (log/debug "Received: " msg)
      (.submit executor-service
               ^Runnable #(try
                            (if (term/tuple? msg)
                              (handle xtdb mbox (:elements msg))
                              (log/warn "Received unrecognized message, ignoring: " msg))
                            (catch Throwable t
                              (log/warn t "Exception in message handler, msg: " msg))))
      (recur (recv)))))

(defn- create-xtdb [xtdb]
  (if (map? xtdb)
    (xt/start-node xtdb)
    xtdb))

(defn- create-mbox [{:keys [node mbox]}]
  (let [node (OtpNode. node)]
    (.createMbox node mbox)))

(defn- create-xtdb-inspector [xtdb config]
  (when config
    ((requiring-resolve 'xtdb-inspector.core/start)
     (merge {:xtdb-node xtdb}
            config))))

(defn- start-server [{:keys [xtdb ^OtpMbox mbox xtdb-inspector]
                      {:keys [workers] :or {workers 20}} :server}]
  (let [executor-service (Executors/newFixedThreadPool workers)
        stop-xtdb-inspector (create-xtdb-inspector xtdb xtdb-inspector)]
    (.start (Thread. #(server-loop xtdb mbox executor-service)))
    ;; Return function to stop
    (fn []
      (try
        (log/info "Stopping xtdberl")
        (.exit mbox "stopping")
        (when stop-xtdb-inspector
          (stop-xtdb-inspector))
        (catch Throwable t
          (log/error t "Exception while trying to stop xtdberl"))))))

(defn start
  "Start XTDB service from configuration.
  Returns a function to stop the server.

  Configuration keys:

  :xtdb    a map of XTDB configuration for starting a node or
           a running XTDB instance to use
  :mbox    the configuration to use for Erlang process communication
           should be a map containing the following keys:
           :node  node name to use
           :mbox  name to use for registered process
  :server  Contains other server configuration
           :workers  how many worker threads to use (default: 20)

  :log-config
           JUL logging configuration .properties file path
           (default configuration is used if omitted)

  Optional keys:
  :xtdb-inspector
           If present, the XTDB inspector web UI is required and
           started. Configuration map is passed to [[xtdb-inspector.core/start]].
  "
  [config]
  (xtdberl.logging/configure (:log-config config))
  (log/info "XTDB/Erlang node starting up")
  (-> config
      (update :xtdb create-xtdb)
      (update :mbox create-mbox)
      start-server))
