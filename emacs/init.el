;;; Rich Smith (rlsNO@SPAMhwyl.org)

;;; ***************************************************************************
;;; Windowing stuff (keep at top to turn off GUI items as soon as possible)
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


;;; ***************************************************************************
;;; Files
;;;

;; A place for emacs to update variables
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)
;; A file where machine-localised settings can be defined; load it if present
(let ((local-file (expand-file-name "local.el" user-emacs-directory)))
  (if (file-exists-p local-file)
      (load local-file)))


;;; ***************************************************************************
;;; Packages
;;;

;; **** Setup ****
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
 (package-refresh-contents)
 (package-install 'use-package))
(setq use-package-always-ensure t)


;; Use straight for package management
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; **** Core stuff ****
(use-package emacs
  :init
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(use-package vertico
  :init
  (vertico-mode)
  (setq vertico-scroll-margin 5)
  (setq vertico-count 25))

(use-package consult
  :bind (("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g l" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-l" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook
  (completion-list-mode . consult-preview-at-point-mode)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key (kbd "M-.")
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)
)

(use-package savehist
  :init
  (savehist-mode))

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :ensure t
  :bind
  ("M-A" . marginalia-cycle)
  :config
  (marginalia-mode))

(use-package embark
  :ensure t
  :bind (("C->" . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

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
  :ensure t
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02))

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

(use-package eglot
  :ensure t
  :defer t
  :hook (python-mode . eglot-ensure)
  :init
  (setq eglot-workspace-configuration
        '((pylsp
           (plugins
            (pycodestyle (enabled . nil)))))))

(use-package direnv
 :config
 (direnv-mode))

(use-package company
  :ensure t
  :defer t
  :custom
  (company-dabbrev-other-buffers t)
  (company-dabbrev-code-other-buffers t)
  (company-show-numbers t)
  (company-minimum-prefix-length 2)
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case t)
  (company-idle-delay 0.2)
  (company-global-modes '(not eshell-mode shell-mode))
  :hook
  ((text-mode . company-mode)
   (prog-mode . company-mode)))


;; **** Python ****
;;
;; Recommend installing
;; pip install pyls-black pyls-isort pyls-mypy
;;
;; For local Python saves add to .dir-locals.el
;; ((python-mode
;;  (eval python-isort-on-save-mode)
;;  (eval blacken-mode)))
(use-package python-isort
  :after python)

(use-package blacken
  :after python)

;; ensure pip install importmagic epc
;; may need to set variable `python-shell-interpreter' to include path
(use-package importmagic
    :ensure t
    :config
    (add-hook 'python-mode-hook 'importmagic-mode))


;; **** Other languages etc. ****
(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode)
  :hook
  (js2-mode . ac-js2-mode)
  :config
  (setq js-indent-level 2))
  (lambda () (tern-mode t))

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

;; **** Aesthetics ****
(use-package minions
  :config (minions-mode 1))

(use-package nyan-mode
  :init
  (setq nyan-bar-length 16)
  :config
  (nyan-mode t))
(use-package beacon)

(use-package nord-theme)

(use-package atom-one-dark-theme
  :config
  (load-theme 'atom-one-dark t))

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

(defun find-config ()
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


(global-set-key
 (kbd "M-H")
 (lambda ()
   (interactive)
   (if mark-active (backward-paragraph) (mark-paragraph))))

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


;; SQL
(setq sql-indent-level 2)


;; ***************************************************************************
;; HTML and CSS
;;

(add-to-list 'auto-mode-alist '("\\.scss\\'" . css-mode))
(setq css-indent-offset 2)


;;; ***************************************************************************
;;; Keys
;;; (Keep at the bottom to avoid being clobbered by various modes)

(global-set-key (kbd "C-x M-f") 'project-find-file)
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

(setq tern-command '("tern" "--no-port-file"))
