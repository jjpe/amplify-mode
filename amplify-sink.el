;;; amplify-sink.el --- Sink definition for amplify-mode
(setq lexical-binding t)

;; Copyright (c) 2015-2017, Joey Ezechiëls
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:

;; 1. Redistributions of source code must retain the above copyright notice,
;; this list of conditions and the following disclaimer.

;; 2. Redistributions in binary form must reproduce the above copyright notice,
;; this list of conditions and the following disclaimer in the documentation
;; and/or other materials provided with the distribution.

;; 3. Neither the name of the copyright holder nor the names of its contributors
;; may be used to endorse or promote products derived from this software without
;; specific prior written permission.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

;;; Commentary:

;;; Code:

;; (require 'rainbow-delimiters)
;; (require 'smartparens)
(require 'amplify-core     (amplify/path "amplify-core.el"))
(require 'amplify-reporter (amplify/path "amplify-reporter.el"))
;; (require 'spoofax-coloring)

;; TODO: use these minor modes in Emacs buffers where spoofax-mode is active
;; (spoofax/add-modes #'rainbow-mode
;;                    #'show-paren-mode
;;                    #'smartparens-mode
;;                    ;; #'whitespace-mode
;;                    #'rainbow-delimiters-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     Sink                                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar amplify/sink nil)

(defun amplify/sink-connect ()
  "Connect the Sink."
  (unless amplify/sink
    (setq amplify/sink (-> (amplify-elisp/uclient-new)
                           (amplify-elisp/uclient-connect)))
    ;; (amplify-elisp/cclient-set-linger amplify/sink 0) ;; TODO: Do I even need this?
    (amplify-elisp/cclient-set-receive-timeout amplify/sink 1)
    (amplify/log "connected sink"))
  t)

(defun amplify/sink-disconnect ()
  "Disconnect the Sink."
  (when amplify/sink
    (setq amplify/sink nil)
    (garbage-collect)
    (amplify/log "disconnected sink"))
  t)


(defun amplify/sink-receive ()
  "Receive a Msg."
  (unless amplify/sink  (error "Sink is not connected"))
  (let ((msg (amplify-elisp/msg-new)))
    (pcase (amplify-elisp/cclient-receive amplify/sink msg)
      (:reconnect  (progn (amplify/source-disconnect)
                          (amplify/sink-disconnect)
                          ;; (amplify/reporter-disconnect)
                          (amplify/source-connect)
                          (amplify/sink-connect)
                          ;; (amplify/reporter-connect)
                          ))
      (_  (->> (amplify-elisp/msg-plistify msg)
               (amplify/drop-msg-if ;; Drop msgs sent by Emacs
                (lambda (msg) (string-prefix-p "emacs " (plist-get msg :process))))
               ;; Add more filters here
               )))))


(defun amplify/drop-msg-if (pred msg-plist)
  "Apply PRED to MSG-PLIST.  Return nil if (PRED MSG-PLIST) is truthy.
Otherwise return MSG.  That it, the predicates specify \"filter out\" conditions."
  (when msg-plist
    (if (funcall pred msg-plist)
        (progn (amplify/log "dropped msg[%s, %d]: %s"
                            (plist-get msg-plist :process)
                            (plist-get msg-plist :request-number)
                            (plist-get msg-plist :kind))
               nil)
        msg-plist)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     Sink timer                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar amplify/sink-poll-interval 0.1
  "The time interval between the moments where the Sink looks for new messages.
Its value is specified in seconds, and may be a decimal number.
The default is to check each 100 ms.")

(defvar amplify/sink-idle-timeout 30
  "The time in seconds the user is idle, before the sink timer is canceled.
The default is 30 seconds.")

(defvar amplify/sink-timer nil
  "A global (i.e. shared across buffers) var that starts the scan for new msgs.")

(defvar amplify/sink-idle-timer nil
  "A global (i.e. shared across buffers) var.
It runs when the user has been inactive for `amplify/sink-idle-timeout' seconds.")

(defun amplify/set-sink-timer (&optional period)
  "Set `amplify/sink-timer', and execute `amplify/sink-timer-fn' every PERIOD seconds."
  (unless (timerp amplify/sink-timer)
    (let ((period (or period amplify/sink-poll-interval)))
      (setq amplify/sink-timer
            (run-at-time  period  period  #'amplify/sink-timer-fn))
      (amplify/log "set sink timer"))))

(defun amplify/cancel-sink-timer ()
  "Reset `amplify/sink-timer'."
  (when (and (timerp amplify/sink-timer)
             (zerop (length (amplify/find-all-descendant-buffers))))
    (cancel-timer amplify/sink-timer)
    (amplify/log "canceled sink timer")
    (setq amplify/sink-timer nil)))

(defun amplify/set-sink-idle-timer (timeout)
  "Set `amplify/sink-idle-timer' to TIMEOUT seconds.
The timer runs when the user has been inactive for that amount of time."
  (unless (timerp amplify/sink-idle-timer)
    (setq amplify/sink-idle-timer
          (run-with-idle-timer timeout :repeat 'amplify/cancel-sink-timer))
    (amplify/log "set sink idle timer")))

(defun amplify/cancel-sink-idle-timer ()
  "Reset `amplify/sink-idle-timer'."
  (when (and (timerp amplify/sink-idle-timer)
             (zerop (length (amplify/find-all-descendant-buffers))))
    (cancel-timer amplify/sink-idle-timer)
    (amplify/log "canceled sink idle timer")
    (setq amplify/sink-idle-timer nil)))


(defconst amplify/null-msg-plist (->> (amplify-elisp/msg-new)
                                      (amplify-elisp/msg-plistify)))

(defun amplify/is-null-msg? (msg)
  "Return t if MSG equals the null msg as either a user-ptr or a property list."
  (cond ((null msg)  t)
        ((and msg (listp msg))
         (equal msg amplify/null-msg-plist))
        ((user-ptrp msg)
         (equal  (amplify-elisp/msg-plistify msg)  amplify/null-msg-plist))
        (t  (error "[amplify/is-null-msg?] Can't handle msg: %s" msg))))



(cl-defun amplify/sink-timer-fn ()
  "Check if there is a new MSG.
If there is, call `amplify/sink-functions' in each amplify buffer."
  (cl-block 'amplify/sink-timer-fn
    (let* (;;(inhibit-modification-hooks t) ;; TODO: Inhibit change hooks here?
           (msg-plist (amplify/sink-receive))
           (msg-process (plist-get msg-plist :process))
           (msg-reqno   (plist-get msg-plist :request-number))
           (msg-kind    (plist-get msg-plist :kind)))
      (when (or  (not (listp msg-plist))  (amplify/is-null-msg? msg-plist))
        (return-from 'amplify/sink-timer-fn nil))
      (unless (eq amplify/request-number msg-reqno)
        ;; Ignore old messages
        (amplify/log "ignoring old msg[%s, %d]: %s"  msg-process  msg-reqno  msg-kind)
        (return-from 'amplify/sink-timer-fn nil))
      (amplify/log "received msg[%s, %d]: %s"  msg-process  msg-reqno  msg-kind)
      (run-hook-with-args  'amplify/sink-functions  msg-plist))))


(provide 'amplify-sink)
;;; amplify-sink.el ends here
