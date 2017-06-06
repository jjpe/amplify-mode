;;; amplify-upgrade.el --- Upgrade functionality for amplify-mode
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
(require 'cl-macs)

(defvar amplify/current-version "0.11.0"
  "The current semantic version of Amplify.")

(defvar amplify/releases-dir
  (amplify/subproc-path "amplify/")
  "The directory in which all amplify releases are stored.")

(defvar amplify/current-release-dir
  (concat  amplify/releases-dir  amplify/current-version  "/")
  "The directory in which the current amplify release is stored.
This is based on the semantic version stored in `amplify/current-version'.")





;; Internal utility functions
(defun amplify/download-resource (url file-name)
  "Download a resource from URL to FILE-NAME."
  (condition-case nil
      (url-copy-file url file-name)
    (error ; file-already-exists
     (amplify/log "using cached resource @ %s" file-name))))

(defun amplify/extract-resource (archive-file-name target-dir)
  "Extract an archive, located at ARCHIVE-FILE-NAME, to a TARGET-DIR.
If the TARGET-DIR already exists, skip the extraction."
  (if (file-exists-p target-dir)
      (amplify/log "using cached dir @ %s" target-dir)
    (progn
      (shell-command (concat "unzip " archive-file-name " -d " target-dir))
      (amplify/log "extracted dir @ %s" target-dir))))

(defun amplify/query-latest-release ()
  "Query GitHub for the latest Amplify release information."
  (amplify/fetch-latest-release "amplify"))

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


(defun amplify/download-release (semver)
  "Download an Amplify release with a specific SEMVER, e.g. \"0.9.6\".
Specifically the following is downloaded:
  * amplify-SEMVER-osx, the main Amplify binary. Hosts the Broadcaster.
  * amplify-SEMVER-osx-dbg, a version of amplify-SEMVER-osx with debug symbols.
  * monto-SEMVER-osx, the legacy Monto binary. Hosts the Broadcaster.
  * monto-SEMVER-osx-dbg, a version of monto-SEMVER-osx with debug symbols.
  * A default settings file.
Files that already exist won't be downloaded again."
  (let* ((new-dir-path (concat amplify/releases-dir semver "/"))
         (url-base "https://github.com/jjpe/amplify/releases/download/")
         (amplify-url (concat url-base semver "/amplify-" semver "-osx"))
         (amplify-bin (concat new-dir-path "amplify-" semver "-osx"))
         (amplify-dbg-url (concat url-base semver "/amplify-" semver "-osx-dbg"))
         (amplify-dbg-bin (concat new-dir-path "amplify-" semver "-osx-dbg"))
         (monto-url (concat url-base semver "/monto-" semver "-osx"))
         (monto-bin (concat new-dir-path "monto-" semver "-osx"))
         (monto-dbg-url (concat url-base semver "/monto-" semver "-osx-dbg"))
         (monto-dbg-bin (concat new-dir-path "monto-" semver "-osx-dbg"))
         ;; (settings-url "https://raw.githubusercontent.com/jjpe/spoofax-server/master/settings.json")
         ;; (settings-file (concat new-dir-path "settings.json"))
         )
    (unless (file-exists-p (amplify/subproc-path))
      (make-directory (amplify/subproc-path)))
    (unless (file-exists-p amplify/releases-dir)
      (make-directory amplify/releases-dir))
    (unless (file-exists-p new-dir-path)
      (make-directory new-dir-path))
    (amplify/download-resource amplify-url amplify-bin)
    (amplify/download-resource amplify-dbg-url amplify-dbg-bin)
    (set-file-modes amplify-bin #o755)
    (set-file-modes amplify-dbg-bin #o755)
    (amplify/download-resource monto-url monto-bin)
    (amplify/download-resource monto-dbg-url monto-dbg-bin)
    (set-file-modes monto-bin #o755)
    (set-file-modes monto-dbg-bin #o755)
    ;; (amplify/download-resource settings-url settings-file)
    ))


(defun amplify/switch-version (semver)
  "Download and switch to Amplify SEMVER version e.g. \"0.11.0\".
This explicitly does not stop or start any processes, that must be done separately."
  ;; TODO: persistence of new SEMVER, especially when Amplify was upgraded.
  (amplify/download-release semver)
  (setq amplify/current-version      semver)
  (setq amplify/current-release-dir  (concat  amplify/releases-dir  semver  "/")))



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






(provide 'amplify-upgrade)
;;; amplify-upgrade.el ends here

;;  LocalWords:  SEMVER
