(ns xtdberl.core
  (:import (com.ericsson.otp.erlang
            OtpNode OtpMbox
            OtpErlangAtom
            OtpErlangDecodeException
            OtpErlangExit)))

(def node (OtpNode. "heppa@localhost"))
(def mbox (.createMbox node "xtdb"))

(.send mbox "foo@localhost" (OtpErlangAtom. "joopajoo"))

(.ping node "foo@localhost" 1000)
(def pid (.receive mbox))

(.send mbox pid (OtpErlangAtom. "sulle_takas"))
