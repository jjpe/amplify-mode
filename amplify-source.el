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

(require 'amplify-core     (amplify/path "amplify-core.el"))
(require 'amplify-reporter (amplify/path "amplify-reporter.el"))

(defvar amplify/source nil)
(make-local-variable 'amplify/source) ;; Each buffer maintains its own source

(defun amplify/source-connect ()
  "Connect the Source."
  (unless amplify/source
    (make-local-variable 'amplify/source)  ;; Each buffer maintains its own source
    (setq amplify/source (-> (cereal/uclient-new)
                             (cereal/uclient-connect)))
    ;; (cereal/cclient-set-linger amplify/source 0) ;; TODO: Do I even need this?
    (cereal/cclient-set-send-timeout amplify/source 1)
    (amplify/log "connected source"))
  t)

(defun amplify/source-disconnect ()
  "Disconnect the Source."
  (when amplify/source
    (setq amplify/source nil)
    (garbage-collect)
    (amplify/log "disconnected source"))
  t)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              Request handling                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar amplify/request-number 1 "Number of the request most recently issued.
The only valid values are positive numbers, so counting starts at 1.")
(make-local-variable 'amplify/request-number) ;; Each buffer maintains its own request number

(defvar amplify/most-recent-request nil "The request most recently issued.")

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
;;          (request (cereal/msg :process process :request-number reqno :kind kind
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







(cl-defun amplify/source-send (&key mode-name kind buffer contents regions language ast)
  "Make the Source send a message.  The following keys are significant:
MODE-NAME: The name of the mode deriving from `amplify-mode'.
KIND: What the Msg represents, e.g. 'parse', 'analyze', 'analysis result',
    'syntax coloring' etc.
BUFFER: The name of the buffer sending the Msg.  Must be a string.
CONTENTS: The Msg contents.  May be nil, a string, or a list of strings.
REGIONS: A list of (begin . end) conses, indicating regions in the sending
    buffer. May be nil as that equals the empty list.
LANGUAGE: The language of the Contents / AST if applicable, otherwise nil.
AST: either nil (default) or a plistified AST, see `cereal/ast-plistify'."
  (unless amplify/source
    (error "Source is not connected"))
  (unless (stringp buffer)
    (error "Expected a string name for 'buffer'"))
  (let ((mode-name (cond ((symbolp mode-name) (symbol-name mode-name))
                         ((stringp mode-name) mode-name)
                         (t (error "Expected a string or symbol for mode-name")))))
    (cereal/cclient-send amplify/source
                         (cereal/msg :process (concat "Emacs/" mode-name)
                                     :request-number (amplify/next-request-number)
                                     :kind kind
                                     :origin buffer
                                     :contents contents
                                     :regions regions
                                     :language language
                                     :ast ast))
    t))


(provide 'amplify-source)
;;; amplify-source.el ends here

;;  LocalWords:  plistified
