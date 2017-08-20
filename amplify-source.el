;;; amplify-source.el --- Source definition for amplify-mode
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

(require 'amplify-core (amplify/path "amplify-core.el"))

(defvar amplify/source nil)

(defun amplify/source-connect (&optional force)
  "Connect the Source.
If FORCE is truthy, connect regardless of whether there already was a connection."
  (unless (or amplify/source force)
    (setq amplify/source (-> (amplify-elisp/uclient-new)
                             (amplify-elisp/uclient-connect)))
    ;; (amplify-elisp/cclient-set-linger amplify/source 0) ;; TODO: don't linger
    (amplify-elisp/cclient-set-send-timeout    amplify/source 1)
    (amplify-elisp/cclient-set-receive-timeout amplify/source 1)
    (amplify-elisp/cclient-set-send-hwm    amplify/source 1000)
    (amplify-elisp/cclient-set-receive-hwm amplify/source 1000)
    (amplify/log "connected source"))
  t)

(defun amplify/source-disconnect ()
  "Disconnect the Source."
  (when amplify/source
    (setq amplify/source nil)
    (garbage-collect)
    (amplify/log "disconnected source"))
  t)

(defun amplify/source-reconnect ()
  "Reconnect the Source."
  (progn (amplify/source-disconnect)
         (amplify/source-connect)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              Request handling                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar amplify/request-number 0 "Number of the request most recently issued.
The only valid values are positive numbers, the first issues number is 1.")
;; (make-local-variable 'amplify/request-number) ;; Each buffer maintains its own request number

;; (defvar amplify/most-recent-request nil "The request most recently issued.")

(defun amplify/next-request-number ()
  "Return the next request number."
  (incf amplify/request-number))


;; TODO: review and re-implement persistence:

;; (defun amplify/request-number-load ()
;;   "Read the most recent request number from disk."
;;   ;; TODO: To support multiple buffers, there should be 1 file per buffer,
;;   ;;    with all files contained in a `.request' directory.
;;   (if (file-exists-p (amplify/subpath ".request-number"))
;;       (->> (amplify/subpath ".request-number")
;;            amplify/read-string-from-file
;;            string-to-int)
;;     0))

;; (defun amplify/request-number-store ()
;;   "Write the most recent request number to disk."
;;   (with-temp-file (amplify/subpath ".request-number")
;;     (insert (format "%d" amplify/request-number))))


;; TODO: re-implement requesting functionality:

;; (defun amplify/request (action-kw source)
;;   "Send a request, then send a Report about it."
;;   (let* ((reqno (amplify/next-request-number))
;;          (process "Emacs")
;;          ;; (kind "kind of msg ???") ;; TODO:
;;          (kind "request")
;;          (request (amplify-elisp/msg :process process :request-number reqno :kind kind
;;                               :origin ""
;;                               :contents ""
;;                               :regions ""
;;                               :language ""
;;                               :ast ""
;;                               )))
;;     )
;; ;;   "Make a new [SOURCE ACTION-KW REQUEST-NUMBER `current-time'] vector request."
;; ;;   (let* ((request-number (amplify/next-request-number))
;; ;;          (request `[,source ,action-kw ,request-number ,(current-time)]))
;; ;;     (setq amplify/most-recent-request request
;; ;;           amplify/clear-fontification? t)
;; ;;     (amplify/log-debug "Requested %s" request)
;; ;;     (with-temp-file (amplify/subpath ".revision")
;; ;;       (insert (format "%d" request-number))
;; ;;       nil))
;;   nil)







(cl-defun amplify/source-send (&key kind buffer contents regions language ast)
  "Make the Source send a message.  The following keys are significant:
MODE-NAME: The name of the mode deriving from `amplify-mode'.
KIND: What the Msg represents, e.g. 'parse', 'analyze', 'analysis result',
    'syntax coloring' etc.
BUFFER: The name of the buffer sending the Msg.  Must be a string.
CONTENTS: The Msg contents.  May be nil, a string, or a list of strings.
REGIONS: A list of (begin . end) conses, indicating regions in the sending
    buffer. May be nil as that equals the empty list.
LANGUAGE: The language of the Contents / AST if applicable, otherwise nil.
AST: either nil (default) or a plistified AST, see `amplify-elisp/ast-plistify'."
  (unless amplify/source
    (error "Source is not connected"))
  (unless (or (bufferp buffer) (stringp buffer))
    (error "Expected a buffer or a string name for 'buffer'"))
  (let* ((buffer (get-buffer buffer))
         (mode-name (with-current-buffer buffer
                      (symbol-name major-mode)))
         (process (concat "emacs " mode-name))
         (request-number (amplify/next-request-number))
         (msg (amplify-elisp/msg :process process
                                 :request-number request-number
                                 :kind kind
                                 :origin (buffer-file-name buffer)
                                 :contents contents
                                 :regions regions
                                 :language language
                                 :ast ast))
         (result (amplify-elisp/cclient-send amplify/source msg)))
    (cond ((eq result :reconnect)  (progn
                                     (amplify/source-reconnect) ;; TODO: what does this even return?!
                                     (amplify/log "ERROR: failed to send msg[%s, %d]: %s"
                                                  process  request-number  kind)))
          ((eq result t)           (progn
                                     (amplify/log "sent msg[%s, %d]: %s"
                                                  process  request-number  kind)
                                     msg))
          ((eq result nil)         msg))))

(cl-defun amplify/report (&key request-number kind contents)
  "Make the Source send a report message.  The following keys are significant:
KIND: What the Msg represents, e.g. 'parse', 'analyze', 'analysis result',
    'syntax coloring' etc.
CONTENTS: The Msg contents.  May be nil, a string, or a list of strings."
  (unless amplify/source
    (error "Source is not connected"))
  (let* ((process "emacs")
         (request-number (or request-number (amplify/next-request-number)))
         (msg (amplify-elisp/msg :process process
                                 :request-number request-number
                                 :kind kind
                                 :contents contents))
         (result (amplify-elisp/cclient-send amplify/source msg)))
    (cond ((eq result :reconnect)  (progn
                                     (amplify/source-reconnect) ;; TODO: what does this even return?!
                                     (amplify/log "ERROR: failed to send msg[%s, %d]: %s"
                                                  process  request-number  kind)))
          ((eq result t)           (progn
                                     (amplify/log "sent msg[%s, %d]: %s"
                                                  process  request-number  kind)
                                     msg))
          ((eq result nil)         msg))))


(provide 'amplify-source)
;;; amplify-source.el ends here

;;  LocalWords:  plistified msg's
