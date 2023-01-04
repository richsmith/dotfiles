;;; ***************************************************************************
;;; Windowing stuff (early initialisation to turn off GUI items as
;;; soon as possible)
;;;
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; set frame title to buffer/filename followed by (username@host)
(add-hook 'window-configuration-change-hook
      (lambda ()
        (setq frame-title-format
          (concat
           "%b ("
           user-login-name "@" system-name ")"))))
