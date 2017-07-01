;;; init.el --- The first thing GNU Emacs runs

;; Do NOT run garbage collection during startup by setting the value super high.
;; This drastically improves `emacs-init-time'.
;; NOTE: We revert this at the end of the file.
(setq gc-cons-threshold 999999999999)

;; Ignore default REGEXP checks of file names at startup.
;; This ALSO drastically improves `emacs-init-time'.
;; NOTE: Some bogus, benign errors will be thrown.
(let ((file-name-handler-alist nil))

  ;; Keep all backups/auto-saves in $TMPDIR
  (setq backup-directory-alist
	`((".*" . ,temporary-file-directory)))
  (setq auto-save-file-name-transforms
	`((".*" ,temporary-file-directory t)))

  ;; Save customizations to their own file
  (setq custom-file "~/.emacs.d/custom.el")
  (load custom-file :noerror)

  ;; Revert garbage collection behaviour to a more modern level
  (run-with-idle-timer
   5 nil
   (lambda ()
     (setq gc-cons-threshold 20000000)))) ; magnars

;;; init.el ends here
