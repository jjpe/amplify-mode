;;; amplify-reporter.el --- Reporter definition for amplify-mode
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

(defvar amplify/reporter nil)

(defun amplify/reporter-connect ()
  "Connect the Reporter, if it was not connected."
  (unless amplify/reporter
    (setq amplify/reporter (-> (amplify-elisp/ureporter-new)
                               (amplify-elisp/ureporter-connect)))
    ;; (amplify-elisp/creporter-set-linger amplify/reporter 0) ;; TODO: Do I even need this?
    (amplify-elisp/creporter-set-send-timeout amplify/reporter 1)
    (amplify/log "connected reporter"))
  t)

(defun amplify/reporter-disconnect ()
  "Disconnect the Reporter, if it was connected."
  (when amplify/reporter
    (setq amplify/reporter nil)
    (garbage-collect)
    (amplify/log "disconnected reporter"))
  t)

(cl-defun amplify/reporter-send (&key mode-name action request-number duration-nanos command)
  "Report performance information.  The following keys are significant:
MODE-NAME: The name of the mode deriving from `amplify-mode'.
ACTION: What the original Msg represents, e.g. 'parse result', 'analysis result',
    'syntax coloring', 'symbol resolution' etc.
REQUEST-NUMBER: The request number of the subject Msg of this report.
DURATION-NANOS: The period between sending out a Msg requesting the information,
        and completing the action that depends on the answer.
COMMAND: May be, and defaults to, nil. Otherwise a string representing a command
        for the collector itself. Currently the only recognized command is:
        * flush: Make the collector persist its cached data"
  (unless amplify/reporter
    (error "Reporter is not connected"))
  (unless (stringp action)
    (error "Expected a string value for :action"))
  (unless (numberp request-number)
    (error "Expected a number for :request-number"))
  (unless (numberp duration-nanos)
    (error "Expected a number for :duration-nanos"))
  (->> (amplify-elisp/report
        :action action
        :process (format "Emacs/mode:%s/buffer:%s"
                         major-mode (buffer-name (current-buffer)))
        :request-number (or request-number amplify/request-number)
        :duration-nanos (or duration-nanos 0)
        :command command)
       (amplify-elisp/creporter-send amplify/reporter)))

(provide 'amplify-reporter)
;;; amplify-reporter.el ends here
