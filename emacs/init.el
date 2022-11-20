;;; Rich Smith (rlsNO@SPAMhwyl.org)



;; ***************************************************************************
;; Windowing stuff (keep at top to turn off GUI items as soon as possible)
;;
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


;; ***************************************************************************
;; Files
;;
; A place for emacs to update variables
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)
; A file where localised settings can be defined; load it if present
(let ((local-file (expand-file-name "local.el" user-emacs-directory)))
  (if (file-exists-p local-file)
      (load local-file)))



;;; ***************************************************************************
;;; Packages
;;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
 (package-refresh-contents)
 (package-install 'use-package))
(setq use-package-always-ensure t)

;; Essentials
(use-package helm
  :init
  (setq helm-delete-minibuffer-contents-from-point 1)
  (setq helm-exit-idle-delay 0)
  (setq helm-split-window-in-side-p t)
  ; uncomment below if Helm starts crashing
  ; (setq gc-cons-threshold 100000000)
  :bind
  ("M-x" . helm-M-x)
  ("C-x C-f" . helm-find-files)
  :config
  (helm-mode)
  ; uncomment below for extra contrast
  ; (set-face-attribute
  ; 'helm-selection nil :background "blue" :foreground "white")
  (use-package helm-ag
    :init
    (setq helm-ag-use-grep-ignore-list 1))
  (use-package helm-system-packages)
  (use-package helm-descbinds
    :config (helm-descbinds-mode)))
(use-package projectile
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'helm)
  (use-package helm-projectile
    :config
    (setq projectile-enable-caching nil)
    (helm-projectile-on)))

;; Version control
(use-package magit
  :init
  (setq magit-save-repository-buffers nil)
  :bind
  ("C-S-m" . magit-status)
  :config
  (define-key magit-mode-map "v"
    #'endless/visit-pull-request-url))
(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02) :ensure t)
(use-package git-gutter-fringe
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))
(use-package git-timemachine)


(use-package tree-sitter-langs :ensure t)
(use-package tree-sitter
  :ensure t
  :after tree-sitter-langs
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; Python stuff
(use-package eglot
  :ensure t
  :defer t
  :hook (python-mode . eglot-ensure)
  :init
  (setq eglot-workspace-configuration
        '((pylsp
           (plugins
            (pycodestyle (enabled . nil)))))))


(use-package python-isort
  :after python)

(use-package blacken
  :after python)

;; For local saves add to .dir-locals.el
;; ((python-mode
;;  (eval python-isort-on-save-mode)
;;  (eval blacken-mode)))

(use-package direnv
 :config
 (direnv-mode))





;; Provide drop-down completion.
(use-package company
  :ensure t
  :defer t
  :custom
  ;; Search other buffers with the same modes for completion instead of
  ;; searching all other buffers.
  (company-dabbrev-other-buffers t)
  (company-dabbrev-code-other-buffers t)
  ;; M-<num> to select an option according to its number.
  (company-show-numbers t)
  ;; Only 2 letters required for completion to activate.
  (company-minimum-prefix-length 3)
  ;; Do not downcase completions by default.
  (company-dabbrev-downcase nil)
  ;; Even if I write something with the wrong case,
  ;; provide the correct casing.
  (company-dabbrev-ignore-case t)
  ;; company completion wait
  (company-idle-delay 0.2)
  ;; No company-mode in shell & eshell
  (company-global-modes '(not eshell-mode shell-mode))
  ;; Use company with text and programming modes.
    :hook ((text-mode . company-mode)
           (prog-mode . company-mode)))


;; Other languages etc.
(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode)
  :hook
  (js2-mode . ac-js2-mode)
  :config
  (setq js-indent-level 2))
  (lambda () (tern-mode t))
(lambda () (flycheck-mode t))
(use-package typescript-mode
  :mode ("\.tsx$"))
(use-package web-mode
  :mode ("\\.ejs\\'" . web-mode)
  :config
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-indent-style 4))
(use-package json-mode)
(use-package terraform-mode)
(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode))
(use-package yaml-mode)
(use-package dotenv-mode)

;; Misc
(use-package editorconfig)

;; Aesthetics
(use-package minions
  :config (minions-mode 1))
(use-package emojify)
(use-package nyan-mode
  :config
  (nyan-mode t))
(use-package beacon)

(use-package nord-theme)
(use-package atom-one-dark-theme)
(load-theme 'atom-one-dark t)



;;; ***************************************************************************
;;; Filesystem
;;;

;; Put autosave and backup files in a temp directory rather than strewn everywhere
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq create-lockfiles nil)


;;; ***************************************************************************
;;; Handy functions
;;;

(defun open-config ()
  "Open the config file"
  (interactive)
  (find-file user-init-file))

(defun pretty-print-xml-region (begin end)
  (interactive "r")
    (save-excursion (nxml-mode)
            (goto-char begin)
            (while (search-forward-regexp "\>[ \\t]*\<" nil t)
              (backward-char)
              (insert "\n"))
            (indent-region begin end))
    (message "XML pretty-printed :)"))

(defun get-random ()
  (interactive)
  (insert (format "%d" (random))))

(defun replace-with-md5 (p1 p2)
  (interactive "r")
  (let ((md5-value (md5 (buffer-substring-no-properties p1 p2))))
    (delete-region (region-beginning) (region-end))
    (insert md5-value)))


(defun rotate-windows (arg)
  "Rotate your windows; use the prefix argument to rotate the other direction"
  (interactive "P")
  (if (not (> (count-windows) 1))
      (message "You can't rotate a single window")
    (let* ((rotate-times (if (and (numberp arg) (not (= arg 0))) arg 1))
           (direction (if (or (< rotate-times 0) (equal arg '(4)))
                          'reverse
                        (lambda (x) x)))
           (i 0))
      (while (not (= rotate-times 0))
        (while  (< i (- (count-windows) 1))
          (let* ((w1 (elt (funcall direction (window-list)) i))
                 (w2 (elt (funcall direction (window-list)) (+ i 1)))
                 (b1 (window-buffer w1))
                 (b2 (window-buffer w2))
                 (s1 (window-start w1))
                 (s2 (window-start w2))
                 (p1 (window-point w1))
                 (p2 (window-point w2)))
            (set-window-buffer-start-and-point w1 b2 s2 p2)
            (set-window-buffer-start-and-point w2 b1 s1 p1)
            (setq i (1+ i))))

        (setq i 0
              rotate-times
              (if (< rotate-times 0) (1+ rotate-times) (1- rotate-times)))))))


(defun close-brace-and-move-point ()
  (interactive)
  (insert "{")
  (insert "}")
  (backward-char)
  (newline-and-indent)
  (previous-line)
  (end-of-line)
  (newline-and-indent))


(defun close-bracket-and-move-point ()
  (interactive)
  (insert "(")
  (insert ")")
  (backward-char))

(defun close-double-quotes-and-move-point ()
  (interactive)
  (insert "\"")
  (insert "\"")
  (backward-char))


(defun close-quotes-and-move-point ()
  (interactive)
  (insert "'")
  (insert "'")
  (backward-char))

(defun goto-line-with-feedback (&optional line)
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive "P")
  (if line
      (goto-line line)
    (let ((initial-linum (if (bound-and-true-p linum-mode) t -1)))
      (unwind-protect
          (progn
            (linum-mode 1)
            (let ((line (read-number "Line: ")))
              (goto-line line)))
        (linum-mode initial-linum)))))

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file" name)
      (let ((new-name (read-file-name "New name: " filename nil nil)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(defun mask-text (start end mask-char)
  "Overwrite the region with the selected mask character."
 (interactive "@*r\ncMasking character: ")
 (let ((region-length (- end start))
       (original-point (point)))
  (delete-region start end)
  (goto-char start)
  (insert-char mask-char region-length)
  (goto-char original-point)))

(defun scroll-up-preserve-location ()
  "Scrolls up a line without moving the cursor position"
  (interactive)
  (scroll-up-line)
  (next-line))

(defun scroll-down-preserve-location ()
  "Scrolls down a line without moving the cursor position"
  (interactive)
  (scroll-down-line)
  (previous-line))


;;; ***************************************************************************
;;; Miscellaneous
;;;
(global-subword-mode 1)
(delete-selection-mode 1)
(put 'upcase-region 'disabled nil)
(setq-default indent-tabs-mode nil)
(setq-default dired-listing-switches "-alhv")
(setq default-tab-width 4)
(setq dabbrev-case-fold-search nil)
(setq sentence-end-double-space nil)
(setq fill-column 100)
(setq comment-fill-column 100)
(setq git-commit-summary-max-length 72)
(setq vc-follow-symlinks nil)
(setq confirm-kill-processes nil)
;; (desktop-save-mode 1)
;; (setq desktop-load-locked-desktop t)
;; (setq savehist-file (expand-file-name "savehist" user-emacs-directory))
;; (savehist-mode 1)
;; (setq savehist-save-minibuffer-history 1)
;; (setq savehist-additional-variables
;;       '(kill-ring search-ring regexp-search-ring))

;; Scrolling
(setq scroll-step 1)
(setq scroll-conservatively 100000)
(setq scroll-margin 10)
(setq linum-delay t)
(global-set-key (kbd "M-p") 'scroll-down-preserve-location)
(global-set-key (kbd "M-n") 'scroll-up-preserve-location)
(global-set-key (kbd "C-M-p") (kbd "C-u 8 C-p"))
(global-set-key (kbd "C-M-n") (kbd "C-u 8 C-n"))
(global-set-key (kbd "C-S-p") 'backward-sexp)
(global-set-key (kbd "C-S-n") 'forward-sexp)


;; Display
(setq-default transient-mark-mode t) ; show selections
(require 'paren) (show-paren-mode 1) ; highlight matching ()s
(global-font-lock-mode t)  ; syntax highlighting
(line-number-mode t)       ; show line number
(column-number-mode t)     ; show column number

(blink-cursor-mode -1) ; stop the cursor blinking

; stop at the end of the file, not just add lines
(setq next-line-add-newlines nil)

; turn off that BLOODY ANNOYING BEEP
(defun do-not-ring-bloody-bell () nil)
(setq ring-bell-function `do-not-ring-bloody-bell)

;; stop closing windows on repeated presses of escape
(defadvice keyboard-escape-quit (around my-keyboard-escape-quit activate)
  (let (orig-one-window-p)
    (fset 'orig-one-window-p (symbol-function 'one-window-p))
    (fset 'one-window-p (lambda (&optional nomini all-frames) t))
    (unwind-protect
        ad-do-it
      (fset 'one-window-p (symbol-function 'orig-one-window-p)))))

(setq highlight-tail-mode 1)
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)

; Confirm C-x C-c
(add-hook 'kill-emacs-query-functions
          (lambda () (y-or-n-p "Really exit Emacs? "))
          'append)

; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))


;; ***************************************************************************
;; Mac
;;
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta)
  (setq default-directory "~/")
  (setq create-lockfiles nil)
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))


;; ***************************************************************************
;; SLIME (Lisp environment)
;;


;;(add-to-list 'load-path "/Applications/slime-2.0")
;(add-to-list 'load-path "/usr/share/emacs22/site-lisp/slime")

;; (require 'slime)
;; (add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
;; (add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
;(slime-setup)

;(setq inferior-lisp-program "sbcl")
;(setq inferior-lisp-program "/opt/local/bin/sbcl")
;(setq inferior-lisp-program "/Applications/cmucl-2004-07-25-090-ppc-darwin/bin/lisp")
;(setq inferior-lisp-program "/opt/local/bin/clisp")
;(setq lisp-indent-function 'common-lisp-indent-function)


;; ***************************************************************************
;; Python
;;

;; make sure following are pip installed:
;; jedi importmagic autopep8 yapf
;; (setq python-check-command "flake8")
;; (highlight-indentation-mode -1)
;; (setq python-shell-interpreter "ipython3"
;;       python-shell-interpreter-args "-i --simple-prompt")

;; (setq python-shell-completion-native nil)
;; (setq python-shell-native-complete nil)
;; (add-to-list 'python-shell-completion-native-disabled-interpreters "python3")





(setq sql-indent-level 2)


;; ***************************************************************************
;; HTML and CSS
;;


(add-to-list 'auto-mode-alist '("\\.scss\\'" . css-mode))



;;; ***************************************************************************
;;; Keys
;;; (Keep at the bottom to avoid being clobbered by various modes)

(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)

;; delete as delete instead of backspace
(global-set-key [delete] 'delete-char)
(global-set-key [kp-delete] 'delete-char)

;; Shortcut keys
(global-set-key "\M-\+" 'comment-region)
(global-set-key "\M-\-" 'uncomment-region)
(global-set-key "\M-\_" 'uncomment-region)
(global-set-key "\C-x\M-o" 'other-frame)
(global-set-key [f12] 'visual-line-mode)
(global-set-key (kbd "<C-tab>") 'whitespace-mode)

;(global-set-key "\M-p" 'backward-paragraph)
;(global-set-key "\M-n" 'forward-paragraph)

(global-set-key (kbd "C-S-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.")
(global-set-key "\M-z" 'zap-up-to-char)

(global-set-key (kbd "C-x M-f") 'helm-projectile)
(global-set-key (kbd "C-x M-F") 'helm-projectile-find-file-in-known-projects)
(global-set-key (kbd "s-s") 'helm-do-ag)
(global-set-key (kbd "s-S") 'helm-do-ag-project-root)

(global-set-key (kbd "s-<left>") #'previous-buffer)
(global-set-key (kbd "s-<right>") #'next-buffer)

(global-set-key [remap goto-line] 'goto-line-with-feedback)

(global-set-key (kbd "C-'") 'close-quotes-and-move-point)
(global-set-key (kbd "M-\"") 'close-double-quotes-and-move-point)
(global-set-key (kbd "C-(") 'close-bracket-and-move-point)
(global-set-key (kbd "C-{") 'close-brace-and-move-point)

(define-key global-map (kbd "C-x O") 'previous-multiframe-window)
(global-set-key (kbd "C-x t") 'rotate-windows)

(global-set-key (kbd "C-M-g") 'top-level)

(global-set-key (kbd "C-z") 'repeat)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)

(setq tern-command '("tern" "--no-port-file"))
