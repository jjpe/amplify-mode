;;; depend.el --- A system to manage dependencies that need to be downloaded

;;; Commentary:

;;; Code:

(require 'dash) ;; threading macros

(defvar depend/semver "0.5.3"
  "The semantic version of this depend.el release.")

(defvar depend/bin-semver "0.1.0"
  "The semantic version of the `bin/download-*' executables that will be used.")

(defvar depend/buffer-name "*depend*")

(defvar depend/root-directory (file-name-directory load-file-name)
  "The depend.el root directory.")

(defvar load-path nil)
(add-to-list 'load-path (directory-file-name depend/root-directory))

(defvar depend/os
  (pcase system-type
    ('darwin       "osx")
    ('gnu/linux    "linux-x86-64")
    ;; TODO: Windows support
    (_ (error "Operating system '%s' is not supported" system-type)))
  "A tag representing the current operating system.")

(defvar depend/debug t
  "Set to t if this code should run in debug mode.
Set to nil otherwise, which means the code will run in release mode.
Release mode means the code will use the bins in `depend/root-directory'/bin/.
In debug mode, however, the `-dbg' variants of those bins will be used.")



(defun depend/log (msg &rest args)
  "Log formatted MSG (including any ARGS) to the `depend/buffer-name' buffer."
  (let ((buffer (get-buffer-create depend/buffer-name)))
    (switch-to-buffer-other-window buffer)
    (with-current-buffer buffer
      (goto-char (point-max))
      (insert-string "[DEPEND] ")
      (insert-string (apply #'format msg args))
      (insert-string "\n")
      (goto-char (point-max)))))



(defun depend/write-dependencies (depend-file dependencies)
  "Open DEPEND-FILE and write DEPENDENCIES to it.
The DEPENDENCIES must be in the form of an associative list.
Each entry in the list must be a (NAME . SEMVER) cons where:
  * NAME is a string identifier e.g. 'amplify-mode
  * SEMVER is the semantic version e.g. \"0.14.4\""
  (with-temp-buffer
    (insert (prin1-to-string dependencies))
    (write-file depend-file)))

(defun depend/read-dependencies (depend-file)
  "Read dependencies from DEPEND-FILE and return them as an associative list.
Each entry in the list must be a (NAME . SEMVER) cons where:
  * NAME is a string identifier e.g. \"amplify-mode\"
  * SEMVER is the semantic version e.g. \"0.14.4\""
  (with-temp-buffer
    (insert-file-contents depend-file)
    (-> (buffer-string)
        (read-from-string)
        (car))))


(cl-defun depend/query-github-release (author project)
  "Query GitHub for the latest release of a PROJECT by AUTHOR.
This function uses the GitHub REST API v3."
  (let ((url (format "https://api.github.com/repos/%s/%s/releases/latest"
                     author project)))
    (with-current-buffer (url-retrieve-synchronously url)
      (->> (json-read)
           (assoc 'tag_name)
           cdr))))


(defun depend/command-bool (cmd &rest args)
  "Execute a `CMD' with any `ARGS', and return the success status as a boolean.
Additionally, redirect all output to the `depend/buffer-name' buffer."
  (let* ((proc-name depend/buffer-name)
         (infile nil)
         (destination (get-buffer-create depend/buffer-name))
         (display t))
    (eq  (apply #'call-process cmd infile destination display args)  0)))

(defun depend/download (url file-path)
  "Download a resource from URL to FILE-PATH.
Return t when the command succeeds i.e. exits with exit code 0.
If the command fails, return the exit code itself."
  (let* ((stem (concat depend/root-directory "bin/"
                       "download-" depend/bin-semver "-" depend/os))
         (bin (if depend/debug  (concat stem "-dbg")  stem))
         (buffer (get-buffer-create depend/buffer-name)))
    (with-current-buffer buffer
      (goto-char (point-max))
      (and (depend/command-bool bin "--from" url "--to" file-path)
           (depend/log "Downloaded %s" file-path)))))

(defun depend/make-executable (file-path)
  "Make FILE-PATH executable."
  (let ((buffer (get-buffer-create depend/buffer-name)))
    (with-current-buffer buffer
      (goto-char (point-max))
      (and (depend/command-bool "chmod" "ug+x" file-path)
           (depend/log "Made %s executable" file-path)))))

(defun depend/extract-zip (zip-file-path target-dir-path)
  "Extract an archive, located at ZIP-FILE-PATH, to a TARGET-DIR-PATH.
If the TARGET-DIR-PATH already exists, skip the extraction."
  (let ((buffer (get-buffer-create depend/buffer-name))
        (command (concat "unzip " zip-file-path " -d " target-dir-path)))
    (with-current-buffer buffer
      (goto-char (point-max))
      (and (depend/command-bool "unzip" zip-file-path "-d" target-dir-path)
           (depend/log "Extracted \"%s\" to \"%s\"" zip-file-path target-dir-path)))))

(provide 'depend)
;;; depend.el ends here
