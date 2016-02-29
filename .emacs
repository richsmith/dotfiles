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
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))


;; set frame title to buffer/filename followed by [username@machine]
(add-hook 'window-configuration-change-hook
	  (lambda ()
	    (setq frame-title-format
		  (concat
		   "%b ["
 		   user-login-name "@" system-name "]"))))


;;; ***************************************************************************
;;; Filesystem
;;;

;; Put autosave and backup files in their own directories rather than strewn
;; all over the filesystem
;; (Adapted from "http://snarfed.org/space/gnu emacs backup files")
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms `((".*", "~/.emacs.d/autosaves" t)))


;; (defvar backup-dir "~/.emacs-saves/backups/")
;; (setq backup-directory-alist (list (cons "." backup-dir)))
;; (defvar autosave-dir "~/.emacs-d/autosaves/")
;; (make-directory autosave-dir t)

;; (defun auto-save-file-name-p (filename)
;;   (string-match "^#.*#$" (file-name-nondirectory filename)))

;; (defun make-auto-save-file-name ()
;;   (concat autosave-dir
;;    (if buffer-file-name
;;       (concat "#" (file-name-nondirectory buffer-file-name) "#")
;;     (expand-file-name
;;      (concat "#%" (buffer-name) "#")))))


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

(defun rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))



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


;; frame transparency; below isn't perfect. From emacswiki.
;(set-frame-parameter (selected-frame) 'alpha '(85 50))
;(add-to-list 'default-frame-alist '(alpha 85 50))

(eval-when-compile (require 'cl)) ; toggle transparency
 (defun toggle-transparency ()
   (interactive)
   (if (/=
        (cadr (frame-parameter nil 'alpha))
        100)
       (set-frame-parameter nil 'alpha '(100 100))
     (set-frame-parameter nil 'alpha '(85 50))))

;;; ***************************************************************************
;;; Miscellaneous
;;;

(setq default-tab-width 4)
(global-subword-mode 1)
(setq dabbrev-case-fold-search nil)
(put 'upcase-region 'disabled nil)
(delete-selection-mode 1)  ; can overwrite text when it is selected
(setq sentence-end-double-space nil)
(setq fill-column 100)
(setq comment-fill-column 100)
;; (require 'uniquify) <- shouldn't be needed for latest Emacs
;; (setq uniquify-buffer-name-style 'forward)

;; Scrolling
(setq scroll-step 1)
(setq scroll-conservatively 100000)
(setq scroll-margin 10)
(setq linum-delay t)

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
;; (require 'python-mode)
;; (require 'auto-complete)
;; (global-auto-complete-mode t)

;; ;;(add-to-list 'load-path "/usr/share/emacs/site-lisp/pymacs")
;; (require 'pymacs)

;; (autoload 'pymacs-apply "pymacs")
;; (autoload 'pymacs-call "pymacs")
;; (autoload 'pymacs-eval "pymacs" nil t)
;; (autoload 'pymacs-exec "pymacs" nil t)
;; (autoload 'pymacs-load "pymacs" nil t)




;; (setq pymacs-load-path '("/usr/local/lib/python2.7/dist-packages/"))
;; (pymacs-load "ropemacs" "rope-")
;; (setq ropemacs-enable-autoimport t)



;; (setq py-install-directory "/home/rls/downloads/python-mode.el-6.1.0/")
;; (add-to-list 'load-path py-install-directory)
;; (require 'python-mode)

;; (setq py-load-pymacs-p t)

;; ;; (require 'auto-complete-config)
;; ;; (ac-config-default)    output.cl    output.cl

;; (autoload 'company-mode "company" nil t)



;; ***************************************************************************
;; Mac
;;
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)
;;(setq ns-function-modifier 'control)
(setq default-directory "~/")
(setq create-lockfiles nil)

(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  )

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;(global-set-key (kbd "C-x f") 'find-file-in-repository)



(setq sr-speedbar-right-side nil)
(setq sr-speedbar-width-x 150)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cgs-step-search-path "/features/**/*.rb; /feature_tests/**/*.rb; ")
 '(custom-enabled-themes (quote (leuven)))
 '(custom-safe-themes (quote ("4f81886421185048bd186fbccc98d95fca9c8b6a401771b7457d81f749f5df75" "9dae95cdbed1505d45322ef8b5aa90ccb6cb59e0ff26fef0b8f411dfc416c552" "15990253bbcfb708ad6ee158d9969cf74be46e3fea2b35f1a0afbac7d4682fbf" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )




;; ***************************************************************************
;; Javascript
;;
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)

;(global-set-key [f5] 'slime-js-reload)
;(add-hook 'js2-mode-hook
;          (lambda ()
;            (slime-js-minor-mode 1)))

;; setup files ending in “.js” to open in js2-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))

(require 'flycheck)
(add-hook 'js2-mode-hook
          (lambda () (flycheck-mode t)))
;(setq flycheck-jshintrc "/Users/rls/code/overlay-management-system/.jshintrc")

;;(require 'nodejs-repl)


;; ***************************************************************************
;; HTML
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


;; ***************************************************************************
;; Sorenson-specific stuff
;;
(add-to-list 'load-path "~/.emacs.d/lisp")
(load "editorconfig")


(add-to-list 'auto-mode-alist '("\\.scss\\'" . css-mode))






(helm-mode)
(global-set-key (kbd "M-x") 'helm-M-x)
(setq helm-delete-minibuffer-contents-from-point 1)
(setq helm-exit-idle-delay 0)
(set-face-attribute 'helm-selection nil :background "blue" :foreground "white")

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      ;helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      ;helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)




(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)



;;; ***************************************************************************
;;; Keys 
;;; (Keep at the bottom to avoid being clobbered by various modes


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

(global-set-key "\M-p" 'backward-paragraph)
(global-set-key "\M-n" 'forward-paragraph)

(global-set-key (kbd "C-S-j")
            (lambda ()
                  (interactive)
                  (join-line -1))) 

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.")
(global-set-key "\M-z" 'zap-up-to-char)

(global-set-key "\C-x\M-f" 'helm-projectile)
(global-set-key (kbd "s-s") 'helm-ag)

(global-set-key [remap goto-line] 'goto-line-with-feedback)

(global-set-key (kbd "C-'") 'close-quotes-and-move-point)
(global-set-key (kbd "M-\"") 'close-double-quotes-and-move-point)
(global-set-key (kbd "C-(") 'close-bracket-and-move-point)
(global-set-key (kbd "C-{") 'close-brace-and-move-point)


(define-key global-map (kbd "C-x O") 'previous-multiframe-window)
(global-set-key (kbd "C-c t") 'toggle-transparency)


(projectile-global-mode)
(setq projectile-completion-system 'helm)
(setq projectile-enable-caching t)
(helm-projectile-on)

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x C-g") 'magit-status)

(global-set-key (kbd "C-z") 'repeat)


