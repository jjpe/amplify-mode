;;; amplify-mode.el --- Amplify mode
(setq lexical-binding t)

;; TODO: Amplify.toml support
;; TODO: support OSes other than OS X

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






;; Code to download and load `amplify-elisp'
(defvar amplify/amplify-elisp-root-dir (amplify/subproc-path "amplify-elisp"))

(defvar amplify/amplify-elisp-version "0.13.1")

(defvar amplify/amplify-elisp-current-dir
  (concat amplify/amplify-elisp-root-dir "/" amplify/amplify-elisp-version))

(defun amplify/download-resource (url file-name)
  "Download a resource from URL to FILE-NAME."
  (condition-case nil
      (url-copy-file url file-name)
    (error ; file-already-exists
     (message "[amplify] using cached resource @ %s" file-name))))

(defun amplify/extract-amplify-elisp (archive-file-name target-dir)
  "Extract an archive, located at ARCHIVE-FILE-NAME, to a TARGET-DIR.
If the TARGET-DIR already exists, skip the extraction."
  (unless (file-exists-p target-dir)
    (make-directory target-dir))
  (shell-command (concat "unzip " archive-file-name " -d " target-dir))
  (message "[amplify] extracted %s  to  %s" archive-file-name target-dir))

(defun amplify/amplify-elisp-query-latest-release ()
  "Query GitHub for the latest `amplify-elisp' release information."
  (amplify/fetch-latest-release "amplify-elisp"))

(cl-defun amplify/fetch-latest-release (project &key author)
  "Query GitHub for the latest release information for a PROJECT by AUTHOR.
AUTHOR is a keyword argument that can be omitted, and defaults to \"jjpe\".
This function uses the GitHub REST API v3. "
  (let* ((author (or author "jjpe"))
         (url (format "https://api.github.com/repos/%s/%s/releases/latest"
                      author project)))
    (with-current-buffer (url-retrieve-synchronously url)
      (->> (json-read)
           (assoc 'tag_name)
           cdr))))

(defun amplify/amplify-elisp-download-release (semver)
  "Download an `amplify-elisp' release with a specific SEMVER.
Specifically the following is downloaded:
 * amplify-elisp-SEMVER.zip, which contains `amplify-elisp'.
If it already exists, it won't be downloaded again."
  (let* ((new-dir-path (amplify/subproc-path "amplify-elisp/" semver "/"))
         (url-base "https://github.com/jjpe/amplify-elisp/releases/download/")
         (amplify-elisp-url (concat url-base semver "/amplify-elisp-" semver ".zip"))
         (amplify-elisp-zip (concat new-dir-path "amplify-elisp-" semver ".zip")))
    (unless (file-exists-p (amplify/subproc-path))
      (make-directory (amplify/subproc-path)))
    (unless (file-exists-p amplify/amplify-elisp-root-dir)
      (make-directory amplify/amplify-elisp-root-dir))
    (unless (file-exists-p (amplify/subproc-path "amplify-elisp/"))
      (make-directory (amplify/subproc-path "amplify-elisp/")))
    (unless (file-exists-p new-dir-path)
      (make-directory new-dir-path))
    (amplify/download-resource amplify-elisp-url amplify-elisp-zip)
    (amplify/extract-amplify-elisp amplify-elisp-zip new-dir-path)))



;; This needs to complete successfully BEFORE requiring `amplify-elisp':
(amplify/amplify-elisp-download-release  amplify/amplify-elisp-version)




(require 'amplify-elisp (amplify/subproc-path "amplify-elisp/"
                                              amplify/amplify-elisp-version
                                              "/amplify-elisp.el"))

(require 'amplify-core        (amplify/path "amplify-core.el"))
(require 'amplify-upgrade     (amplify/path "amplify-upgrade.el"))
;; (require 'amplify-reporter    (amplify/path "amplify-reporter.el"))
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
  (amplify/download-release amplify/current-version)

  (amplify/install-packages
   'rainbow-delimiters
   'rainbow-mode
   'smartparens
   'whitespace)

  (unless (process-live-p (get-process "*Broadcaster*"))
    ;; (amplify/start-broadcaster-dbg!)
    ;; (monto/start-brokers!)
    ;; (monto/start-brokers-dbg!)
    (amplify/start-broadcaster!))
  (unless (process-live-p (get-process "*Collector*"))
    (amplify/start-collector!))

  ;; (amplify/reporter-connect)
  (amplify/sink-connect)
  (amplify/source-connect)

  (amplify/set-sink-timer      amplify/sink-poll-interval)
  (amplify/set-sink-idle-timer amplify/sink-idle-timeout);; TODO: This might not be needed anymore

  (add-hook 'after-change-major-mode-hook 'amplify/try-shutdown)
  )

(defun amplify/try-shutdown ()
  "Try to shut `amplify-mode' down."
  (when (zerop (length (amplify/find-all-descendant-buffers)))
    ;; (amplify/reporter-disconnect)
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
