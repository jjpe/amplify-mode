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


(defvar amplify/sink nil)
(make-local-variable 'amplify/sink) ;; Each buffer maintains its own sink

(defun amplify/sink-connect ()
  "Connect the Sink."
  (unless amplify/sink
    (make-local-variable 'amplify/sink) ;; Each buffer maintains its own sink
    (setq amplify/sink (-> (cereal/uclient-new)
                           (cereal/uclient-connect)))
    ;; (cereal/cclient-set-linger amplify/sink 0) ;; TODO: Do I even need this?
    (cereal/cclient-set-receive-timeout amplify/sink 1)
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
  (unless amplify/sink
    (error "Sink is not connected"))
  (let ((msg (cereal/msg-new)))
    (cereal/cclient-receive amplify/sink msg)
    msg))


(defvar amplify/sink-poll-interval 0.1 ;; 0.25
  "This determines how often the Sink will look for messages from the Broker.
Its value is specified in seconds, and may be a decimal number.")





;; TODO: Inspect this thoroughly. Its very likely that some of this can just go.
;;
;; (cl-defun spoofax/sink (product-alist recv-duration-nanos)
;;   (with-current-buffer (get-buffer (spoofax/requested-source))
;;     (cl-destructuring-bind (source-str reqno product language contents)
;;         (spoofax/split-product product-alist)
;;       (unless (eq (spoofax/requested-reqno) reqno)
;;         (return-from spoofax/sink)) ;; Prune products from old revisions
;;       (spoofax/report :action "received product"
;;                       :process "emacs-sink"
;;                       :revision reqno
;;                       :duration-nanos recv-duration-nanos)
;;       (message "[spoofax] Received product (@ %s s, took %5d ms)"
;;                (spoofax/time-since-request)
;;                (/ recv-duration-nanos spoofax/ns-per-ms))
;;       (cond ((string= product "parse messages")
;;              (spoofax/update-feedback source-str contents reqno))
;;             ((string= product "analysis messages")
;;              (spoofax/update-feedback source-str contents reqno))
;;             ((string= product "syntax coloring")
;;              (spoofax/update-coloring contents reqno))
;;             ((string= product "open projects")
;;              (spoofax/log-info "Open projects: %s" contents))
;;             ((string= product "reference resolution")
;;              (spoofax/jump-to-reference contents))
;;             ((member product '("parse result" "analysis result"))
;;              ;; This admittedly odd construction is to ensure that even
;;              ;; if the user hasn't requested this particular result, if
;;              ;; they could have then this COND branch should be taken
;;              ;; nonetheless.
;;              (when (string= (spoofax/requested-product) product)
;;                ;; (and (string= (spoofax/requested-product) product)
;;                ;;        (eq (spoofax/requested-reqno) reqno))
;;                (spoofax/update-result-buffer source-str product contents)
;;                (spoofax/update-product-buffer product-alist)))
;;             (t (message "[spoofax] Unknown product name. product-alist: %s"
;;                         (pp-to-string product-alist)))))))





(defun spoofax/update-feedback (source-string contents request-number)
  ""
  (let* ((json-key-type 'string)
         (bufname (file-name-nondirectory source-string))
         (results (spoofax/measure
                      (let* ((feedback-items
                              (->> contents
                                   json-read-from-string
                                   (mapcar #'json-read-from-string)
                                   (mapcar (lambda (msg)
                                             (split-string msg "||"))))))
                        (when spoofax/clear-fontification?
                          (setq spoofax/clear-fontification? nil)
                          (spoofax/clear-tooltips (point-min) (point-max)))
                        (with-current-buffer bufname
                          (loop for item in feedback-items
                                do (spoofax/highlight-feedback-item item))))))
         (duration-nanos (first results)))
    (spoofax/report :action "updated feedback"
                    :process "emacs-sink"
                    :revision request-number
                    :duration-nanos duration-nanos)
    (message "[spoofax] Updated feedback (@ %s s, took %5d ms)"
             (spoofax/time-since-request)
             (/ duration-nanos spoofax/ns-per-ms))))

(defun spoofax/update-coloring (contents request-number)
  ""
  (let* ((results (->> (loop for item across (json-read-from-string contents)
                             do (spoofax/highlight-coloring-item item))
                       (spoofax/measure)))
         (duration-nanos (first results)))
    (spoofax/report :action "updated coloring"
                    :process "emacs-sink"
                    :revision request-number
                    :duration-nanos duration-nanos)
    (message "[spoofax] Updated coloring (@ %s s, took %5d ms)"
             (spoofax/time-since-request)
             (/ duration-nanos spoofax/ns-per-ms))))

(defun spoofax/jump-to-reference (contents)
  "Jump to a reference, which must be parseable from CONTENTS."
  (when (string= contents "no resolution found")
    (beep)
    (return-from spoofax/jump-to-reference))
  (let* ((results (spoofax/measure
                      (let* ((resolution (json-read-from-string contents))
                             (filepath (spoofax/alist-get resolution 'file))
                             (start (spoofax/alist-get-int resolution 'start))
                             (end   (spoofax/alist-get-int resolution 'end)))
                        (with-current-buffer (find-file filepath)
                          (goto-char (1+ start))))))
         (duration-nanos (first results)))
    (spoofax/report :action "opened resolution"
                    :process "emacs-sink"
                    :revision reqno
                    :duration-nanos duration-nanos)
    (message "[spoofax] Opened resolution (@ %s s, took %5d ms)"
             (spoofax/time-since-request)
             (/ duration-nanos spoofax/ns-per-ms))))

(defun spoofax/update-result-buffer (source-string product contents)
  ""
  (let ((since-timestamp (time-since (spoofax/requested-timestamp))))
    (spoofax/log-info "Request (took %s s): %s"
                    (format-time-string "%S.%3N" since-timestamp)
                    spoofax/most-recent-request)
    (spoofax/with-output-buffer "result"
        (insert "From \"" source-string "\":\n"
                "Result type: \"" product "\"\n\n"
                contents))))

(defun spoofax/update-product-buffer (product-alist)
  ""
  (when spoofax/show-product-buffer
    (spoofax/with-output-buffer "product"
        (insert "Product:\n\n" (pp-to-string product-alist)))))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              Sink timer                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar amplify/sink-timer nil)

(defun amplify/sink-timer-fn ()
  "Timer function."
  (let (;; Don’t run any of the hooks that respond to buffer changes while this fn runs
        (inhibit-modification-hooks t)

        ;; (rx-result (spoofax/measure (zmqdl/receive spoofax/sink-socket)))
        )
    (cl-destructuring-bind (recv-nanos response) rx-result
      (cond ((stringp response)
             (cl-destructuring-bind (product-nanos product-alist)
                 (->> response (amplify/to-product-alist) (amplify/measure))
               (amplify/sink product-alist (+ recv-nanos product-nanos))))
            ((eq response :no-bytes-read) nil) ;; No messages ready
            ((eq response :EAGAIN) nil) ;; No messages ready
            ((eq response :EINTR)  nil) ;; Interrupted (no messages ready)
            ((eq response :ENOTSUP)
             (error "Can't receive: Socket does not support receiving"))
            ((eq response :EFSM)
             (error "Can't receive: Socket not in the appropriate state"))
            ((eq response :ETERM)
             (error "Can't receive: ZMQ context terminated"))
            ((eq response :ENOTSOCK)
             (error "Can't receive: amplify/sink-socket is not a valid socket"))
            (t nil) ;; Glug glug, swallow the garbage
            ;; (t (error "Invalid response: %s" response)) ;; TODO: reactivate
            ))))

(defun amplify/set-sink-timer (period)
  "Set the timer for the Sink.
This executes the timer function TIMER-FN every PERIOD milliseconds."
  ;; TODO: write my own change hook, which works like `after-change-functions',
  ;; except it only fires for buffers opened under spoofax.el.
  (setq amplify/sink-timer (run-at-time 0 period #'amplify/sink-timer-fn)))

(defun amplify/cancel-sink-timer ()
  "Reset the Monto sink timer."
  (when (timerp amplify/sink-timer)
    (cancel-timer amplify/sink-timer)
    (setq amplify/sink-timer nil)
    (message "[amplify] Canceled Sink timer")))







(provide 'amplify-sink)
;;; amplify-sink.el ends here
