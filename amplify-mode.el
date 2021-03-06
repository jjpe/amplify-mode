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

(defvar amplify/debug t
  "Set to t to turn on debug mode.")

(defvar amplify/os
  (pcase system-type
    ('darwin       "osx")
    ('gnu/linux    "linux")
    ;; ('gnu/linux    "linux-x86-64") ;; TODO:
    ;; TODO: Windows support
    (_ (error "Operating system '%s' is not supported" system-type)))
  "A tag associated with the current operating system.")





(defvar amplify/semver "0.17.7")

;; Download and load the `amplify' core:
(defvar amplify/amplify-root-dir (amplify/subproc-path "amplify"))
(defvar amplify/amplify-version "0.17.2")
(defvar amplify/amplify-current-dir
  (concat amplify/amplify-root-dir "/" amplify/amplify-version))

;; Download and load `amplify-elisp':
(defvar amplify/amplify-elisp-root-dir (amplify/subproc-path "amplify-elisp"))
(defvar amplify/amplify-elisp-version "0.17.2")
(defvar amplify/amplify-elisp-current-dir
  (concat amplify/amplify-elisp-root-dir "/" amplify/amplify-elisp-version))

;; Download and load `amplify-viz':
(defvar amplify/amplify-viz-root-dir (amplify/subproc-path "amplify-viz"))
(defvar amplify/amplify-viz-version "1.1.0")
(defvar amplify/amplify-viz-current-dir
  (concat amplify/amplify-viz-root-dir "/" amplify/amplify-viz-version))





(defun amplify/update-amplify ()
  "Update the `amplify' core.
Specifically the following is downloaded:
 * `amplify'-`SEMVER'-`OS'
 * `amplify'-`SEMVER'-`OS'-dbg
For each dependency the corresponding `SEMVER's are looked up on github.com.
The `OS' will be automatically detected.
If it already exists, it won't be downloaded again."
  (let* ((path-base (amplify/subproc-path "amplify/" amplify/amplify-version))
         (url-base (format "https://github.com/jjpe/%s/releases/download/%s"
                           "amplify"
                           amplify/amplify-version))
         (bin-name     (format "amplify-%s-%s"
                               amplify/amplify-version
                               amplify/os))
         (dbg-bin-name (format "amplify-%s-%s-dbg"
                               amplify/amplify-version
                               amplify/os))
         (url     (concat url-base  "/" bin-name))
         (bin     (concat path-base "/" bin-name))
         (dbg-url (concat url-base  "/" dbg-bin-name))
         (dbg-bin (concat path-base "/" dbg-bin-name)))
    (unless (file-exists-p (amplify/subproc-path))
      (make-directory (amplify/subproc-path)))
    (unless (file-exists-p amplify/amplify-root-dir) ;; TODO:
      (make-directory amplify/amplify-root-dir))
    (unless (file-exists-p path-base)
      (make-directory path-base))
    (if (file-exists-p bin)
        (depend/log "Using cached bin @ %s" bin)
      (progn (depend/download url bin)
             (depend/make-executable bin)))
    (if (file-exists-p dbg-bin)
        (depend/log "Using cached dbg bin @ %s" dbg-bin)
      (progn (depend/download url dbg-bin)
             (depend/make-executable dbg-bin)))))

(defun amplify/update-amplify-elisp ()
  "Update `amplify-elisp'.
Specifically the following is downloaded:
 * `amplify-elisp'-`SEMVER'.zip
For each dependency the corresponding `SEMVER's are looked up on github.com.
If it already exists, it won't be downloaded again."
  (let* ((path-base (amplify/subproc-path "amplify-elisp/"
                                          amplify/amplify-elisp-version))
         (url-base (format "https://github.com/jjpe/%s/releases/download/%s"
                           "amplify-elisp"
                           amplify/amplify-elisp-version))
         (zip-name (format "amplify-elisp-%s.zip" amplify/amplify-elisp-version))
         (url      (concat url-base  "/" zip-name))
         (zip-file (concat path-base "/" zip-name)))
    (unless (file-exists-p (amplify/subproc-path))
      (make-directory (amplify/subproc-path)))
    (unless (file-exists-p amplify/amplify-elisp-root-dir) ;; TODO:
      (make-directory amplify/amplify-elisp-root-dir))
    (unless (file-exists-p path-base)
      (make-directory path-base))
    (if (file-exists-p zip-file)
        (depend/log "Using cached zip file @ %s" zip-file)
      (progn (depend/download url zip-file)
             (depend/extract-zip zip-file path-base)))))

(defun amplify/update-amplify-viz ()
  "Update `amplify-viz'.
Specifically the following is downloaded:
 * `amplify-viz'-`SEMVER'.zip
For each dependency the corresponding `SEMVER's are looked up on github.com.
If it already exists, it won't be downloaded again."
  (let* ((path-base (amplify/subproc-path "amplify-viz/"
                                          amplify/amplify-viz-version))
         (url-base (format "https://github.com/jjpe/%s/releases/download/%s"
                           "amplify-viz"
                           amplify/amplify-viz-version))
         (zip-name (format "amplify-viz-%s.zip" amplify/amplify-viz-version))
         (url      (concat url-base  "/" zip-name))
         (zip-file (concat path-base "/" zip-name)))
    (unless (file-exists-p (amplify/subproc-path))
      (make-directory (amplify/subproc-path)))
    (unless (file-exists-p amplify/amplify-viz-root-dir) ;; TODO:
      (make-directory amplify/amplify-viz-root-dir))
    (unless (file-exists-p path-base)
      (make-directory path-base))
    (if (file-exists-p zip-file)
        (depend/log "Using cached zip file @ %s" zip-file)
      (progn (depend/download url zip-file)
             (depend/extract-zip zip-file path-base)))))

(defun amplify/update-dependencies ()
  "Update the `amplify-mode' dependencies.
Specifically the following is downloaded:
 * `amplify-elisp'-`SEMVER'.zip
For each dependency the corresponding `SEMVER's are looked up on github.com.
Dependencies that already exist on the file system won't be downloaded again."
  (amplify/update-amplify)
  (amplify/update-amplify-elisp)
  (amplify/update-amplify-viz))



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
(require 'amplify-benchmark   (amplify/path "amplify-benchmark.el"))
(require 'amplify-viz         (amplify/path "amplify-viz.el"))




(defcustom amplify/sink-functions nil
  "A list of hooks run when Amplify receives a msg with certain properties.
The properties are:
  * The msg must have a `request-number' field equal to `amplify/request-number'
Deriving modes can use this to asynchronously get access to the msg.
To do so, functions are added to this list that take a msg argument."
  :options '(imenu-add-menubar-index)
  :type 'hook
  :group 'amplify)

(defcustom amplify/raw-sink-functions nil
  "A list of hooks run when Amplify receives a msg.
Deriving modes can use this to asynchronously get access to the msg.
To do so, functions are added to this list that take a msg argument."
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
    (amplify/start-broadcaster!))
  (unless (process-live-p (get-process "*Collector*"))
    ;; (amplify/start-collector-dbg!)
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
