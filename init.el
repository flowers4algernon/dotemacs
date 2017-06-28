;;; init.el --- The first thing GNU Emacs runs

;; Keep all backups/auto-saves in $TMPDIR
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Save customizations to their own file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file :noerror)

;;; init.el ends here
