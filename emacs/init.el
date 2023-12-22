(setq user-full-name "Rich Smith"
      user-mail-address (concat "rls" "@" "hwyl.org"))

(defvar native-comp-deferred-compilation-deny-list nil)


;;; ***************************************************************************
;;; Window display
;;;

(defconst laptop-screen-width 1920)
(defvar machine-type nil)

(defun get-primary-monitor-width ()
  "Get the width of the primary monitor in pixels."
  (let* ((primary-monitor (car (display-monitor-attributes-list)))
         (geometry (cdr (assoc 'geometry primary-monitor)))
         (width (nth 2 geometry)))
    width))

(defun get-machine-type ()
  (let ((width (get-primary-monitor-width)))
    (if (<= width laptop-screen-width)
        'laptop
      'desktop)))

(setq machine-type (get-machine-type))

(if (eq machine-type 'desktop)
    (setq-default default-frame-alist
       '((height . 60)
         (width . 176)
         (left . 613)
         (top . 100)
         (vertical-scroll-bars . nil)
         (horizontal-scroll-bars . nil)
         (tool-bar-lines . 0))))
  (setq-default default-frame-alist
       '((height . 55)
         (width . 176)
         (left . 613)
         (top . 100)
         (vertical-scroll-bars . nil)
         (horizontal-scroll-bars . nil)
         (tool-bar-lines . 0)))

(split-window-horizontally)

;;; ***************************************************************************
;;; Files
;;;

;; A place for emacs to update variables
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
;; A file where machine-localised settings can be defined; load it if present
(let ((local-file (expand-file-name "local.el" user-emacs-directory)))
  (when (file-exists-p local-file)
    (load local-file)))


;;; ***************************************************************************
;;; Packages
;;;

;; **** Setup ****
(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

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
  :bind (("<del>" . delete-char)
         ("M-<backspace>" . backward-kill-sexp)
         ("C-x C-r" . rename-current-buffer-file)
         ("C-x M-o" . other-frame)
         ("C-x 0" . previous-multiframe-window)
         ("C-x t" . rotate-windows)
         ("C-M-g" . top-level)
         ("C-z" . repeat)
         ("M-+" . comment-region)
         ("M-\-" . uncomment-region)
         ("M-_" . uncomment-region)
         ("<f12>" . visual-line-mode)
         ("C-<tab>" . whitespace-mode))
  :init
  ;; (setq enable-recursive-minibuffers t)
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))


(use-package frame-fns
  :straight (frame-fns :type git :host github :repo "emacsmirror/frame-fns")
  )
(use-package frame-cmds
  :straight (frame-cmds :type git :host github :repo "emacsmirror/frame-cmds")
  :bind (("M-<up>" . move-frame-up)
         ("M-<down>" . move-frame-down)
         ("M-<left>" . move-frame-left)
         ("M-<right>" . move-frame-right))
  )

(use-package vertico
  :bind (:map vertico-map
              ("C-j" . vertico-insert))
  :init
  (vertico-mode)
  (setq vertico-scroll-margin 5)
  (setq vertico-count 25))

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (("C-x M-F" . projectile-find-file-in-known-projects)
         :map projectile-mode-map
         ("s-p" . projectile-command-map)
         ("C-c p" . projectile-command-map)))

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
         ("M-'" . consult-register-store)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g l" . consult-goto-line)
         ("M-g M-l" . consult-goto-line)
         ("M-g o" . consult-outline)
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
         ("M-s L" . consult-line-multi) ; }
         ("M-s b" . consult-line-multi) ; } AKA search project buffers
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ("M-s w" . consult-yank-from-kill-ring)
         ("M-s y" . consult-yank-from-kill-ring)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ;; Minibuffer history
         :map minibuffer-local-map
         ("C-l" . up-directory)
         ("M-s" . consult-history)
         ("M-r" . consult-history))

  :hook
  (completion-list-mode . consult-preview-at-point-mode)

  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-multi-occur
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help))

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
  :after vertico
  :bind
  (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
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
  :after (embark consult)
  :demand t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Version control
(use-package magit
  :init
  (setq magit-save-repository-buffers nil)
  :bind
  (("C-S-m" . magit-status)
   ; below 3 recommended by Magit
   ("C-x g" . magit-status)
   ("C-c g" . magit-dispatch)
   ("C-c f" . magit-file-dispatch)
   :map magit-mode-map
   ("v". visit-pull-request-url))
  :config
  (defun visit-pull-request-url ()
    "Visit the current branch's PR on Github."
    (interactive)
    (browse-url
     (format "https://github.com/%s/pull/new/%s"
             (replace-regexp-in-string
              "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
              (magit-get "remote"
                         (magit-get-push-remote)
                         "url"))
             (magit-get-current-branch)))))

  

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


;; LSP
(use-package eglot
  :ensure t
  :defer t
  :hook (python-mode . eglot-ensure)
  :bind (("C-c r" . eglot-rename)
         ("C-c C-r" . eglot-rename)))

;; Currently disabled due to issues with eglot
;; See https://github.com/joaotavora/eglot/discussions/1127
;; (use-package corfu
;;   :custom
;;   ((corfu-auto t)                 ;; Enable auto completion
;;    (corfu-scroll-margin 5)        ;; Use scroll margin
;;    (corfu-min-width 48)
;;    (corfu-separator ?\s))
;;   :bind ((:map corfu-map
;;                ("<enter>" . nil)))
;;   :init
;;   (global-corfu-mode))

(use-package direnv
 :config
 (direnv-mode))


(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t
  :hook (prog-mode . copilot-mode)
  :bind (("C-c M-f" . copilot-accept-completion-by-word)
         ("C-c <tab>" . copilot-accept-completion)
         :map copilot-completion-map
         ("C-g" . 'copilot-clear-overlay)
         ("M-P" . 'copilot-previous-completion)
         ("M-N" . 'copilot-next-completion)
         ("<tab>" . 'copilot-accept-completion)
         ("M-f" . 'copilot-accept-completion-by-word)
         ("M-e" . copilot-accept-completion-by-line)
         ("M-n" . copilot-accept-completion-by-line))
  :custom-face (copilot-overlay-face ((t :foreground "silver" :underline t)))
  :config
  (setq copilot-log-max 10000)
  (setq copilot-node-executable
        (executable-find "node")))

;; (use-package combobulate
;;   :straight (:host github :repo "mickeynp/combobulate")
;;   ; :vc (:fetcher github :repo mickeynp/combobulate)
;;   :preface
;;   ;; You can customize Combobulate's key prefix here.
;;   ;; Note that you may have to restart Emacs for this to take effect!
;;   (setq combobulate-key-prefix "C-c o")

;;   ;; Optional, but recommended.
;;   ;;
;;   ;; You can manually enable Combobulate with `M-x
;;   ;; combobulate-mode'.
;;   :hook ((python-ts-mode . combobulate-mode)
;;          (js-ts-mode . combobulate-mode)
;;          (css-ts-mode . combobulate-mode)
;;          (yaml-ts-mode . combobulate-mode)
;;          (json-ts-mode . combobulate-mode)
;;          (typescript-ts-mode . combobulate-mode)
;;          (tsx-ts-mode . combobulate-mode)))

;;
;; **** Languages ****
;;
(use-package tree-sitter-langs
  :ensure t)
(use-package tree-sitter
  :ensure t
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))


;; **** Python ****
;;
;; Recommend installing
;; pip install pyls-black pyls-isort pyls-mypy
;;
;; For local Python saves add to .dir-locals.el
;; ((python-mode
;;  (eval python-isort-on-save-mode)
;;  (eval blacken-mode)))
(use-package python
  :init
  (setq python-fill-docstring-style 'django))

(use-package python-isort
  :ensure t
  :after python)

(use-package python-black
  :demand t
  :after python
  :hook (python-mode . python-black-on-save-mode-enable-dwim))


;; **** Other languages etc. ****
(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode)
  :hook
  (js2-mode . ac-js2-mode)
  :config
  (setq js-indent-level 2))

(use-package typescript-mode
  :mode ("\.tsx$"))
(use-package web-mode
  :ensure t
  :mode ("\\.ejs\\'" . web-mode)
  :config
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-indent-style 4))
(use-package json-mode
  :ensure t)
(use-package terraform-mode
  :ensure t)
(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . markdown-mode))
(use-package yaml-mode
  :ensure t)
(use-package dotenv-mode
  :ensure t)
(use-package dockerfile-mode
  :ensure t)

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

(defun find-custom ()
  "Open the custom file"
  (interactive)
  (find-file custom-file))

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

(defun up-directory (path)
  "Move up a directory in PATH without affecting the kill buffer."
  (interactive "p")
  (if (string-match-p "/." (minibuffer-contents))
      (let ((end (point)))
   (re-search-backward "/.")
   (forward-char)
   (delete-region (point) end))))


(defun get-buffers-matching-mode (mode)
  "Returns a list of buffers where their major-mode is equal to MODE"
  (let ((buffer-mode-matches '()))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (eq mode major-mode)
          (push buf buffer-mode-matches))))
    buffer-mode-matches))


(defun multi-occur-in-this-mode ()
  "Show all lines matching REGEXP in buffers with this major mode."
  (interactive)
  (consult-multi-occur
   (get-buffers-matching-mode major-mode)
   (car (occur-read-primary-args))))


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


(keymap-global-set "M-H"
                   (lambda ()
                     (interactive)
                     (if mark-active (backward-paragraph) (mark-paragraph))))

;; Scrolling
(setq scroll-step 1)
(setq scroll-conservatively 100000)
(setq scroll-margin 10)
(setq linum-delay t)
(keymap-global-set "M-p" 'scroll-down-preserve-location)
(keymap-global-set "M-n" 'scroll-up-preserve-location)
(global-set-key (kbd "C-M-p") (kbd "C-u 8 C-p"))
(global-set-key (kbd "C-M-n") (kbd "C-u 8 C-n"))
(keymap-global-set "C-S-p" 'backward-sexp)
(keymap-global-set "C-S-n" 'forward-sexp)


;; Display
(setq-default transient-mark-mode t) ; show selections
(require 'paren) (show-paren-mode 1) ; highlight matching ()s
(global-font-lock-mode t)  ; syntax highlighting
(line-number-mode t)       ; show line number
(column-number-mode t)     ; show column number

(blink-cursor-mode -1) ; stop the cursor blinking

; stop at the end of the file, not just add lines
(setq next-line-add-newlines nil)

; absolutely do NOT beep at me
(setq ring-bell-function `ignore)

;; stop closing windows on repeated presses of escape
(defadvice keyboard-escape-quit (around my-keyboard-escape-quit activate)
  (let (orig-one-window-p)
    (fset 'orig-one-window-p (symbol-function 'one-window-p))
    (fset 'one-window-p (lambda (&optional nomini all-frames) t))
    (unwind-protect
        ad-do-it
      (fset 'one-window-p (symbol-function 'orig-one-window-p)))))

(setq highlight-tail-mode 1)
(keymap-global-set "C-S-<mouse-1>" 'mc/add-cursor-on-click)

(add-hook 'kill-emacs-query-functions
          (lambda () (y-or-n-p "Really exit Emacs? "))
          'append)

(define-key minibuffer-local-filename-completion-map
            [C-l] #'up-directory)

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

(keymap-global-set "C-x M-f" 'project-find-file)


(keymap-global-set "C-S-j"
                   (lambda ()
                     (interactive)
                     (join-line -1)))

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.")
(keymap-global-set "M-z" 'zap-up-to-char)

(keymap-global-set "C-'" 'close-quotes-and-move-point)
(keymap-global-set "M-\"" 'close-double-quotes-and-move-point)

(keymap-global-set "C-(" 'close-bracket-and-move-point)
(keymap-global-set "C-{" 'close-brace-and-move-point)

(keymap-global-set "C-c e p" 'flymake-goto-previous-error)
(keymap-global-set "C-c e n" 'flymake-goto-next-error)

;; Disable key chord for set-goal-column, but skip warning
(put 'set-goal-column 'disabled nil)
(keymap-global-set "C-x C-n" nil)

(setq tern-command '("tern" "--no-port-file"))
