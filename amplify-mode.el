;;; amplify-mode.el --- Amplify mode
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

;; TODO: With an eye on performance it is probably a good idea if the
;;     Reporter, Source and Sink are all global rather than buffer local.
;;     It's not so much the instantiation of the objects themselves that's
;;     expensive, rather it's the fact that now all the thread locals need
;;     to do their own de/serialization, and it takes time to transfer Msg
;;     objects into/out of Elisp.


;; Ensure Emacs can find the root directory
(defvar amplify/root-directory (file-name-directory load-file-name)
  "The Amplify root directory.")

(defvar load-path nil)
(add-to-list 'load-path amplify/root-directory)

(defun amplify/path (&rest subpath-specs)
  "Calculate the absolute path out of of SUBPATH-SPECS, then return it.
The path formed by concatenating the SUBPATH-SPECS will be relative
to `amplify/root-directory'.  SUBPATH-SPECS must be some mix of strings and
symbols.  So for example, (amplify/path \"foo/\" \"bar/\") will work, as
will (amplify/path \"foo/\" 'bar/). Note that the path separators are
explicitly included."
  (let ((spec-names (mapcar (lambda (x) (if (symbolp x) (symbol-name x) x))
                            subpath-specs)))
    (apply #'concat amplify/root-directory spec-names)))

(defun amplify/subproc-path (&rest subpath-specs)
  "Return the path to a file located in one of the sub-processes."
  (apply #'amplify/path "subproc/" subpath-specs))




(require 'amplify-core        (amplify/path "amplify-core.el"))
(require 'amplify-upgrade     (amplify/path "amplify-upgrade.el"))
(require 'amplify-reporter    (amplify/path "amplify-reporter.el"))
(require 'amplify-broadcaster (amplify/path "amplify-broadcaster.el"))
(require 'amplify-source      (amplify/path "amplify-source.el"))
(require 'amplify-sink        (amplify/path "amplify-sink.el"))
(require 'amplify-collector   (amplify/path "amplify-collector.el"))




(defvar amplify-mode-shared-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map prog-mode-map)
    ;; (define-key map "\e\C-q" 'indent-sexp)
    ;; (define-key map "\177" 'backward-delete-char-untabify)

    ;; TODO: Add key bindings for:
    ;;        * start
    ;;        * open project
    ;;        * close project
    ;;        * stop
    ;;        * restart
    map)
  "Key map for commands shared by all sorts of Amplify modes.")

;; (defcustom amplify-mode-hook nil
;;   "Hook run when entering Amplify mode."
;;   :options '(imenu-add-menubar-index)
;;   :type 'hook
;;   :group 'amplify)

;; (defcustom amplify-interaction-mode-hook nil
;;   "Hook run when entering Amplify Interaction mode."
;;   :options '(eldoc-mode)
;;   :type 'hook
;;   :group 'amplify)



;;; Generic Amplify mode.

(defvar amplify-mode-map
  (let ((map (make-sparse-keymap))
        (menu-map (make-sparse-keymap "Amplify")))
    (set-keymap-parent map amplify-mode-shared-map)
    ;; (define-key map "\e\C-x" 'lisp-eval-defun)
    ;; (define-key map "\C-c\C-z" 'run-lisp)
    ;; (bindings--define-key map [menu-bar lisp] (cons "Lisp" menu-map))
    ;; (bindings--define-key menu-map [run-lisp]
    ;;   '(menu-item "Run inferior Lisp" run-lisp
    ;;               :help "Run an inferior Lisp process, input and output via buffer `*inferior-lisp*'"))
    ;; (bindings--define-key menu-map [ev-def]
    ;;   '(menu-item "Eval defun" lisp-eval-defun
    ;;               :help "Send the current defun to the Lisp process made by M-x run-lisp"))
    ;; (bindings--define-key menu-map [ind-sexp]
    ;;   '(menu-item "Indent sexp" indent-sexp
    ;;               :help "Indent each line of the list starting just after point"))
    map)
  "Keymap for ordinary Amplify mode.
All commands in `amplify-mode-shared-map' are inherited by this map.")



(define-derived-mode amplify-mode prog-mode "Amplify"
  "Raw Amplify mode. Not really intended for direct consumption.
Rather, it is meant as infrastructure for other modes similarly to Lisp mode.

‘\[command]’, ‘\{keymap}’, and ‘\<keymap>’
"
  (amplify/download-release amplify/current-version)
  (amplify/start-broadcaster!)
  ;; (amplify/start-broadcaster-dbg!)
  ;; (monto/start-brokers!)
  ;; (monto/start-brokers-dbg!)
  (amplify/start-collector!) ;; Receives and records Reports
  (amplify/reporter-connect) ;; Sends Reports from various points in the tool graph
  (amplify/sink-connect)
  (amplify/source-connect)
  ;; (amplify/source-send :process "Emacs tester"
  ;;                      :request-number 300
  ;;                      :kind "test msg"
  ;;                      :origin "origin"
  ;;                      :contents '("contents line 0"  "contents line 1")
  ;;                      :regions '((0 1)  (1 2))
  ;;                      :language "Mxsptlk"
  ;;                      :ast nil)

  ;; TODO: Init amplify state using buffer-local variables
  )




;; (define-derived-mode lisp-mode prog-mode "Lisp"
;;   "Major mode for editing Lisp code for Lisps other than GNU Emacs Lisp.
;; Commands:
;; Delete converts tabs to spaces as it moves back.
;; Blank lines separate paragraphs.  Semicolons start comments.
;; \\{lisp-mode-map}
;; Note that `run-lisp' may be used either to start an inferior Lisp job
;; or to switch back to an existing one."
;;   (lisp-mode-variables nil t)
;;   (setq-local find-tag-default-function 'lisp-find-tag-default)
;;   (setq-local comment-start-skip
;;               "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\)\\(;+\\|#|\\) *")
;;   (setq imenu-case-fold-search t))








(provide 'amplify-mode)
;;; amplify-mode.el ends here

;;  LocalWords:  FMT lify
