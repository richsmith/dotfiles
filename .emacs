;;; .emacs
;;; Rich Smith (rlsNO@SPAMhwyl.org)
;;; Reminder: C-x C-e to re-evaluate a line; M-x load-file to reload file


;; ***************************************************************************
;; Windowing stuff (keep at top to turn off GUI items as soon as possible)
;;

(when window-system (set-frame-size (selected-frame) 100 65))

;; colours
(set-background-color "white")

(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))


;; set frame title to buffer/filename followed by [username@machine]
(add-hook 'window-configuration-change-hook
      (lambda ()
        (setq frame-title-format
          (concat
           "%b ["
           user-login-name "@" system-name "]"))))


;;; ***************************************************************************
;;; Packages
;;;
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
;; Marmalade probably redundant compared to melpa
;; ("marmalade" . "http://marmalade-repo.org/packages/")


(setq package-list '(editorconfig
                     elpy
                     flycheck
                     helm
                     helm-ag
                     helm-projectile
                     js2-mode
                     json-mode
                     magit
                     projectile
                     web-mode))

(if (eq system-type 'darwin)
 	(add-to-list 'package-list 'exec-path-from-shell))

(package-initialize) ; activate all the packages (in particular autoloads)

(or (file-exists-p package-user-dir) ; fetch the list of packages available 
  (package-refresh-contents))

(dolist (package package-list) ; install the missing packages
  (unless (package-installed-p package)
	(package-refresh-contents)
    (package-install package)))


;;; ***************************************************************************
;;; Filesystem
;;;


;; Put autosave and backup files in their own directories rather than strewn
;; all over the filesystem
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms `((".*", "~/.emacs.d/autosaves" t)))



;;; ***************************************************************************
;;; Handy functions
;;;

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


(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))


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

(defun scroll-up-preserve-location ()
  (interactive)
  (scroll-up-line)
  (next-line))

(defun scroll-down-preserve-location ()
  (interactive)
  (scroll-down-line)
  (previous-line))


;;; ***************************************************************************
;;; Miscellaneous
;;;
(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)
(global-subword-mode 1)
(setq dabbrev-case-fold-search nil)
(put 'upcase-region 'disabled nil)
(delete-selection-mode 1)
(setq sentence-end-double-space nil)
(setq fill-column 100)
(setq comment-fill-column 100)
(setq git-commit-summary-max-length 72)
(setq-default dired-listing-switches "-alhv")
(setq vc-follow-symlinks nil)
(set-register ?e (cons 'file "~/.emacs"))

;; Scrolling
(setq scroll-step 1)
(setq scroll-conservatively 100000)
(setq scroll-margin 10)
(setq linum-delay t)
(global-set-key (kbd "M-p") 'scroll-down-preserve-location)
(global-set-key (kbd "M-n") 'scroll-up-preserve-location)
(global-set-key (kbd "C-M-p") (kbd "C-u 8 C-p"))
(global-set-key (kbd "C-M-n") (kbd "C-u 8 C-n"))

;; Display
(setq-default transient-mark-mode t) ; show selections
(require 'paren) (show-paren-mode 1) ; highlight matching ()s
(global-font-lock-mode t)  ; syntax highlighting
(line-number-mode t)       ; show line number
(column-number-mode t)     ; show column number

(blink-cursor-mode -1) ; stop the cursor blinking

; put the scroll bar on the right (consistency!)
(setq scroll-bar-mode-explicit t)
(set-scroll-bar-mode `right)

; stop at the end of the file, not just add lines
(setq next-line-add-newlines nil)

; turn off that BLOODY ANNOYING BEEP (only way that seems guaranteed to work!)
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


;; ***************************************************************************
;; Mac
;;
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)
(setq default-directory "~/")
(setq create-lockfiles nil)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))


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


(add-to-list 'load-path "~/.emacs.d/plugins")
(elpy-enable)
(setq elpy-rpc-python-command "python3")
(setq python-check-command "flake8")
(highlight-indentation-mode -1)
(define-key elpy-mode-map (kbd "M-,") 'pop-tag-mark)

(setq python-shell-completion-native nil)
(setq python-shell-native-complete nil)
(add-to-list 'python-shell-completion-native-disabled-interpreters "python3")



;; ***************************************************************************
;; Javascript
;;
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)

;; setup files ending in “.js” to open in js2-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))


(add-hook 'js-mode-hook (lambda () (tern-mode t)))

(require 'flycheck)
(add-hook 'js2-mode-hook
          (lambda () (flycheck-mode t)))

(setq js-indent-level 2)


;; ***************************************************************************
;; HTML and CSS
;;
(setq web-mode-markup-indent-offset 4)
(setq web-mode-css-indent-offset 4)
(setq web-mode-code-indent-offset 4)
(setq web-mode-indent-style 4)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

(add-to-list 'auto-mode-alist '("\\.scss\\'" . css-mode))


;; ***************************************************************************
;; Sorenson-specific stuff
;;
(add-to-list 'load-path "~/.emacs.d/lisp")
(load "editorconfig")
(require 'editorconfig)
(editorconfig-mode 1)









(helm-mode)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(setq helm-delete-minibuffer-contents-from-point 1)
(setq helm-exit-idle-delay 0)
(set-face-attribute 'helm-selection nil :background "blue" :foreground "white")

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      ;helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      ;helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)



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

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x C-g") 'magit-status)

(global-set-key (kbd "C-z") 'repeat)


(projectile-global-mode)
(setq projectile-completion-system 'helm)
(setq projectile-enable-caching nil)
(helm-projectile-on)



(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "s-h") 'help-command)


(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)


(setq tern-command '("tern" "--no-port-file"))





;; ***************************************************************************
;; Custom
;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cgs-step-search-path "/features/**/*.rb; /feature_tests/**/*.rb; ")
 '(custom-enabled-themes (quote (leuven)))
 '(custom-safe-themes
   (quote
    ("4f81886421185048bd186fbccc98d95fca9c8b6a401771b7457d81f749f5df75" "9dae95cdbed1505d45322ef8b5aa90ccb6cb59e0ff26fef0b8f411dfc416c552" "15990253bbcfb708ad6ee158d9969cf74be46e3fea2b35f1a0afbac7d4682fbf" default)))
 '(elpy-modules
   (quote
    (elpy-module-company elpy-module-eldoc elpy-module-flymake elpy-module-pyvenv elpy-module-yasnippet elpy-module-sane-defaults)))
 '(nyan-mode t)
 '(package-selected-packages
   (quote
    (json-mode aggressive-indent highlight-tail string-utils fish-mode column-marker web-mode sql-indent spaceline rope-read-mode org-agenda-property nyan-mode magit js2-mode jedi-direx helm-projectile helm-filesets helm-ag flycheck exec-path-from-shell elpy editorconfig csv-mode company-jedi anaconda-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
