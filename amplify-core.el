;;; amplify-core.el --- Amplify core
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

(unless (>= emacs-major-version 25)
  (error "[amplify-mode] Only Emacs >= 25.1 is supported"))

(unless (package-installed-p 'dash)
  (package-install 'dash))

(require 'dash) ;; Threading operators (->, ->>)
(require 'amplify-log)



(defun spoofax/read-string-from-file (file-path)
  "Return the contents of FILE-PATH as a string."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun amplify/descendant-mode? (major-mode)
  "Return t if and only if the MAJOR-MODE of the current buffer is derived from `amplify-mode'."
  (let ((mode major-mode)
        parents)
    (while mode
      (setq parents (cons mode parents)
            mode (get mode 'derived-mode-parent)))
    (member 'amplify-mode parents)))

(defun amplify/find-all-descendant-buffers ()
  "Return all live buffers with a `major-mode' derived from `amplify-mode'."
  (loop for buf in (buffer-list)
        if (amplify/descendant-mode? (with-current-buffer buf major-mode))
        collect buf))


(cl-defmacro amplify/defprocess
    (proc-spec command &optional start-predicate start-fn-docstring)
  "Define process utility functions to help manage its life cycle.
PROC-SPEC is a symbol that labels the process. From this the names for the
          functions are generated.
COMMAND is the command to be run in order to start the process.
START-FN-DOCSTRING is the docstring for the generated amplify/*-start! function.
                The docstring for the corresponding amplify/*-stop! function can
                be automatically generated."
  (declare (indent defun))
  (cl-labels ((make-name (sym &rest infix)
                         (concat "*"
                                 (capitalize (symbol-name sym))
                                 (if infix
                                     (concat " " (car infix) "*")
                                   "*")))
              (make-amplify-sym (&rest parts)
                                (->> (mapcar #'symbol-name parts)
                                     (apply #'concat "amplify/")
                                     intern)))
    (let* (;; Ensure proper argument evaluation:
           (spec-var         proc-spec)
           (start-pred-var   start-predicate)
           (pred-fn          `(lambda () ,start-predicate))
           ;; Helper bindings, derived from input:
           (proc-sym         (make-amplify-sym spec-var '-process))
           (proc-name        (make-name spec-var))
           (buffer-name      (make-name spec-var "out"))
           (start-sym        (make-amplify-sym 'start- spec-var '!))
           (stop-sym         (make-amplify-sym 'stop- spec-var '!))
           (start-docstring  (concat (format "Start the %s process. " proc-name)
                                     start-fn-docstring))
           (stop-docstring   (format "Kill the %s process and its %s buffer."
                                     proc-name buffer-name)))
      `(progn
         (defvar ,proc-sym nil) ;; Intended to be global
         (intern (symbol-name ',proc-sym))
         (defun ,start-sym ()
           ,start-docstring
           ,(if (eq nil start-pred-var)
                `(unless (process-live-p (get-process ,proc-name))
                   (start-process-shell-command ,proc-name ,buffer-name ,command))
              `(if (,pred-fn)
                   (unless (process-live-p (get-process ,proc-name))
                     (start-process-shell-command ,proc-name ,buffer-name ,command))
                 (error "Couldn't start process %s, start-predicate failed: %s"
                        ,proc-name
                        ',start-pred-var)))
           ;; (with-current-buffer ,buffer-name
           ;;   (amplify/enable-modes))
           )
         (defun ,stop-sym ()
           ,stop-docstring
           (when (and (eq (process-status ,proc-name) 'run)
                      (zerop (length (amplify/find-all-descendant-buffers))))
             (stop-process ,proc-name)
             (delete-process ,proc-name)
             ;; (funcall (amplify/get-buffer-kill-fn) (get-buffer ,buffer-name))
             (setq ,proc-sym nil)))))))

(provide 'amplify-core)
;;; amplify-core.el ends here
