;;; init.el --- The first thing GNU Emacs runs

;; Do NOT run garbage collection during startup by setting the value super high.
;; This drastically improves `emacs-init-time'.
;; NOTE: We revert this at the end of the file.
(setq gc-cons-threshold 999999999999)

;; Ignore default REGEXP checks of file names at startup.
;; This ALSO drastically improves `emacs-init-time'.
;; NOTE: Some bogus, benign errors will be thrown.
(let ((file-name-handler-alist nil))

  ;; Set up package.el for use with MELPA
  (require 'package)
  (setq package-enable-at-startup nil)
  (add-to-list 'package-archives
	       '("melpa" . "https://melpa.org/packages/"))
  (package-initialize)

  ;; Bootstrap `use-package'.  It will manage all other packages.
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

  ;; further reduce load time
  (eval-when-compile
    (require 'use-package))
  (require 'diminish) ; allows for easy removal of packages' modeline strings
  (require 'bind-key) ; simplifies how keybindings are set
  
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
