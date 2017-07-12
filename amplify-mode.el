;;; amplify-mode.el --- Amplify mode
(setq lexical-binding t)

;; TODO: support Windows

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

;; Ensure Emacs can find the root directory
(defvar amplify/root-directory (file-name-directory load-file-name)
  "The Amplify root directory.")

(require 'depend (concat spoofax/root-directory "depend.el/depend.el"))



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





(defvar amplify/semver "0.15.6")

;; Download and load the `amplify' core:
(defvar amplify/amplify-root-dir (amplify/subproc-path "amplify"))
(defvar amplify/amplify-version "0.14.0")
(defvar amplify/amplify-current-dir
  (concat amplify/amplify-root-dir "/" amplify/amplify-version))

;; Download and load `amplify-elisp':
(defvar amplify/amplify-elisp-root-dir (amplify/subproc-path "amplify-elisp"))
(defvar amplify/amplify-elisp-version "0.15.2")
(defvar amplify/amplify-elisp-current-dir
  (concat amplify/amplify-elisp-root-dir "/" amplify/amplify-elisp-version))





(defun amplify/update-amplify ()
  "Update the `amplify' core.
Specifically the following is downloaded:
 * `amplify'-`SEMVER'-`OS'.zip
 * `amplify'-`SEMVER'-`OS'-dbg.zip
For each dependency the corresponding `SEMVER's are looked up on github.com.
The `OS' will be automatically detected.
If it already exists, it won't be downloaded again."
  (let* ((semver (depend/query-github-release "jjpe" "amplify"))
         (amplify-dir-path (amplify/subproc-path "amplify/" semver))
         (os (pcase system-type
               ('darwin       "osx")
               ;; ('gnu/linux    "linux-x86-64") ;; TODO:
               ('gnu/linux    "linux")
               ;; TODO: Windows support
               (_ (error "Operating system '%s' is not supported" system-type))))
         (url-base "https://github.com/jjpe/amplify/releases/download")
         (url (concat url-base "/" semver "/amplify-" semver "-" os))
         (bin (concat amplify-dir-path    "/amplify-" semver "-" os))
         (dbg-url (concat url-base "/" semver "/amplify-" semver "-" os "-dbg"))
         (dbg-bin (concat amplify-dir-path    "/amplify-" semver "-" os "-dbg")))
    (unless (file-exists-p (amplify/subproc-path))
      (make-directory (amplify/subproc-path)))
    (unless (file-exists-p amplify/amplify-root-dir) ;; TODO:
      (make-directory amplify/amplify-root-dir))
    (unless (file-exists-p amplify-dir-path)
      (make-directory amplify-dir-path))
    (depend/download url bin)
    (depend/download dbg-url dbg-bin)))

(defun amplify/update-amplify-elisp ()
  "Update `amplify-elisp'.
Specifically the following is downloaded:
 * `amplify-elisp'-`SEMVER'.zip
For each dependency the corresponding `SEMVER's are looked up on github.com.
If it already exists, it won't be downloaded again."
  (let* ((semver (depend/query-github-release "jjpe" "amplify-elisp"))
         (amplify-elisp-dir-path (amplify/subproc-path "amplify-elisp/" semver))
         (url-base "https://github.com/jjpe/amplify-elisp/releases/download/")
         (amplify-elisp-url (concat url-base semver "/amplify-elisp-" semver ".zip"))
         (amplify-elisp-zip (concat amplify-elisp-dir-path "/amplify-elisp-" semver ".zip")))
    (unless (file-exists-p (amplify/subproc-path))
      (make-directory (amplify/subproc-path)))
    (unless (file-exists-p amplify/amplify-elisp-root-dir) ;; TODO:
      (make-directory amplify/amplify-elisp-root-dir))
    (unless (file-exists-p amplify-elisp-dir-path)
      (make-directory amplify-elisp-dir-path))
    (depend/download amplify-elisp-url amplify-elisp-zip)
    (depend/extract-zip amplify-elisp-zip amplify-elisp-dir-path)))

(defun amplify/update-dependencies ()
  "Update the `amplify-mode' dependencies.
Specifically the following is downloaded:
 * `amplify-elisp'-`SEMVER'.zip
For each dependency the corresponding `SEMVER's are looked up on github.com.
Dependencies that already exist on the file system won't be downloaded again."
  (amplify/update-amplify)
  (amplify/update-amplify-elisp))



;; This needs to complete successfully BEFORE requiring `amplify-elisp':
(amplify/update-dependencies)




(require 'amplify-elisp (amplify/subproc-path "amplify-elisp/"
                                              amplify/amplify-elisp-version
                                              "/amplify-elisp.el"))

(require 'amplify-core        (amplify/path "amplify-core.el"))
(require 'amplify-broadcaster (amplify/path "amplify-broadcaster.el"))
(require 'amplify-source      (amplify/path "amplify-source.el"))
(require 'amplify-sink        (amplify/path "amplify-sink.el"))
(require 'amplify-collector   (amplify/path "amplify-collector.el"))




(defcustom amplify/sink-functions nil
  "A list of hooks run when Amplify receives a MSG.
Deriving modes can use this to asynchronously get access.
To do so, functions are added to this list that take an arg list (BUFFER MSG)."
  :options '(imenu-add-menubar-index)
  :type 'hook
  :group 'amplify)

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




(defvar amplify-mode-map
  (let ((map (make-sparse-keymap))
        (menu-map (make-sparse-keymap "Amplify")))
    (set-keymap-parent map prog-mode-map)
    map)
  "Keymap for Amplify mode.
All commands in `amplify-mode-shared-map' are inherited by this map.")



(define-derived-mode amplify-mode prog-mode "Amplify"
  "Raw Amplify mode. Not really intended for direct consumption.
Rather, it is meant as infrastructure for other modes to inherit from.
That will make it easier to build IDE-like functionality for multiple languages.
\\{amplify-mode-map}"

  (amplify/install-packages 'rainbow-delimiters
                            'rainbow-mode
                            'smartparens
                            'whitespace
                            'paren)
  (rainbow-mode 1)
  (rainbow-delimiters-mode 1)
  (smartparens-mode 1)
  (whitespace-mode 1)
  (show-paren-mode 1)

  (unless (process-live-p (get-process "*Broadcaster*"))
    ;; (amplify/start-broadcaster-dbg!)
    ;; (monto/start-brokers!)
    ;; (monto/start-brokers-dbg!)
    (amplify/start-broadcaster!))
  (unless (process-live-p (get-process "*Collector*"))
    (amplify/start-collector!))

  (amplify/sink-connect)
  (amplify/source-connect)

  (amplify/set-sink-timer      amplify/sink-poll-interval)
  (amplify/set-sink-idle-timer amplify/sink-idle-timeout);; TODO: This might not be needed anymore

  (add-hook 'after-change-major-mode-hook 'amplify/try-shutdown)
  )




(defun amplify/try-shutdown ()
  "Try to shut `amplify-mode' down."
  (when (zerop (length (amplify/find-all-descendant-buffers)))
    (amplify/sink-disconnect)
    (amplify/source-disconnect)

    (amplify/cancel-sink-timer)
    (amplify/cancel-sink-idle-timer)
    (amplify/stop-broadcaster!)
    (amplify/stop-collector!)
    (remove-hook 'after-change-major-mode-hook 'amplify/try-shutdown)))




(defun amplify/install-packages (&rest packages)
  "Ensure that all PACKAGES (which consists of symbols) are installed."
  (loop for package in packages
        do (unless (package-installed-p package)
             (package-install package))))




(provide 'amplify-mode)
;;; amplify-mode.el ends here

;;  LocalWords:  FMT lify
