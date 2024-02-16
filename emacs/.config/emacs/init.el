;; Hello
(setq user-full-name "Rich Smith"
      user-mail-address (concat "rls" "@" "hwyl.org"))

;; Emacs-handled config
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Define initialisation files
(setq init-dir (file-name-concat user-emacs-directory "init"))
(defvar init-file-names
  '("core.org"
    "display.org"
    "navigation.org"
    "code.org"
    "misc.org"
    "vcs.org"
    "org.org")
  "List of org initialisation file names to load.")
(when (eq system-type 'darwin)
  (add-to-list 'init-file-names "mac.org"))
(defvar init-files
  (mapcar (lambda (file-name) (expand-file-name file-name init-dir))
          init-file-names)
  "List of org files to load.")

;; Load the initialisation files
(defun load-init-file (file-name)
  (org-babel-load-file (expand-file-name file-name init-dir)))
(mapc 'load-init-file init-files)

;; Provision for opening initialisation files
(defun find-init ()
  "Open an init file."
  (interactive)
  (let ((file (completing-read "Select init file: " init-files)))
    (when file
      (find-file file))))

;; Load machine-localised settings if present
;; (Do this after everything else so that it can override settings)
(let ((local-file (expand-file-name "local.el" user-emacs-directory)))
  (when (file-exists-p local-file)
    (load local-file)))
