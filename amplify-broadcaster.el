;;; amplify-broadcaster.el --- Broadcaster definition for amplify-mode
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

;; (amplify/defprocess version-broker
;;   (amplify/subproc-path "yang/target/debug/version_broker"))

;; (amplify/defprocess product-broker
;;   (amplify/subproc-path "yang/target/debug/product_broker"))

(amplify/defprocess broadcaster
  (concat
   ;; Change dir because Amplify loads the settings from a file relative
   ;; to the "current" directory:
   "cd " amplify/amplify-current-dir "; "
   ;; Start the binary:
   amplify/amplify-current-dir
   "amplify-" amplify/amplify-version "-" amplify/current-os " broadcast"))

(amplify/defprocess broadcaster-debug
  (concat
   ;; Change dir because Amplify loads the settings from a file relative
   ;; to the "current" directory:
   "cd " amplify/amplify-current-dir "; "
   ;; Start the binary:
   amplify/amplify-current-dir
   "amplify-" amplify/amplify-version "-" amplify/current-os "-dbg broadcast"))

(provide 'amplify-broadcaster)
;;; amplify-broadcaster.el ends here
