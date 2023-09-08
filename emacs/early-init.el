;; Separate elpa dir for each Emacs version
(setq package-user-dir (locate-user-emacs-file
                        (concat
                         (file-name-as-directory "elpa")
                         emacs-version)))

;; Turn off unneeded window stuff
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; Trade some RAM for speed
(setq gc-cons-threshold (* 64 1024 1024))

;; Set frame title to: buffer-name (username@host)
(add-hook 'window-configuration-change-hook
      (lambda ()
        (setq frame-title-format
          (concat
           "%b ("
           user-login-name "@" system-name ")"))))
