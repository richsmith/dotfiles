;; Turn off unneeded window stuff

(setq default-frame-alist
       '((height . 55)
         (width . 174)
         (left . 613)
         (top . 100)
         (vertical-scroll-bars . nil)
         (horizontal-scroll-bars . nil)
         (tool-bar-lines . 0)))

(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
;; (tool-bar-mode -1)
;; (scroll-bar-mode -1)

;; Trade some RAM for speed
(setq gc-cons-threshold (* 64 1024 1024))

;; Set frame title to: buffer-name (username@host)
(add-hook 'window-configuration-change-hook
      (lambda ()
        (setq frame-title-format
          (concat
           "%b ("
           user-login-name "@" system-name ")"))))
