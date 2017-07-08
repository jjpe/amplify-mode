;;; amplify-collector.el --- Collector definition for amplify-mode
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
(require 'amplify-source   (amplify/path "amplify-source.el"))
(require 'amplify-upgrade  (amplify/path "amplify-upgrade.el"))

(amplify/defprocess collector
  (concat
   ;; Change dir because Amplify loads the settings from a file relative
   ;; to the "current" directory:
   "cd " amplify/current-release-dir "; "
   ;; Start the binary:
   amplify/current-release-dir
   "amplify-" amplify/current-version "-" amplify/current-os " collect"))

(amplify/defprocess collector-debug
  (concat
   ;; Change dir because Amplify loads the settings from a file relative
   ;; to the "current" directory:
   "cd " amplify/current-release-dir "; "
   ;; Start the binary:
   amplify/current-release-dir
   "amplify-" amplify/current-version "-" amplify/current-os "-dbg collect"))


(cl-defun amplify/collector-send-cmd (cmd)
  "Send a command to the collector."
  (amplify/source-send :kind (concat "collector:" cmd)
                       :buffer (current-buffer)))

(defun amplify/kill-collector ()
  "Ask the collector process to die."
  (interactive)
  (amplify/collector-send-cmd "exit"))

(defun amplify/flush-collector ()
  "Ask the *Collector* process to persist the `msg's it has collected so far.
After this is completed, the `msg's are erased from RAM.
This means that so the next flush will only persist the `msg's since then."
  (interactive)
  (amplify/collector-send-cmd "flush"))


(provide 'amplify-collector)
;;; amplify-collector.el ends here
