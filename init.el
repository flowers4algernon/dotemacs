;;; init.el --- The first thing GNU Emacs runs

;; Keep all backups/auto-saves in $TMPDIR
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;; init.el ends here
