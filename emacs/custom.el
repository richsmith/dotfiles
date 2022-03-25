(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cgs-step-search-path "/features/**/*.rb; /feature_tests/**/*.rb; ")
 '(custom-enabled-themes '(atom-one-dark))
 '(custom-safe-themes
   '("37768a79b479684b0756dec7c0fc7652082910c37d8863c35b702db3f16000f8" "171d1ae90e46978eb9c342be6658d937a83aaa45997b1d7af7657546cae5985b" "57f95012730e3a03ebddb7f2925861ade87f53d5bbb255398357731a7b1ac0e0" "76dc63684249227d64634c8f62326f3d40cdc60039c2064174a7e7a7a88b1587" "6dd2b995238b4943431af56c5c9c0c825258c2de87b6c936ee88d6bb1e577cb9" "4f81886421185048bd186fbccc98d95fca9c8b6a401771b7457d81f749f5df75" "9dae95cdbed1505d45322ef8b5aa90ccb6cb59e0ff26fef0b8f411dfc416c552" "15990253bbcfb708ad6ee158d9969cf74be46e3fea2b35f1a0afbac7d4682fbf" default))
 '(elpy-modules
   '(elpy-module-company elpy-module-eldoc elpy-module-flymake elpy-module-pyvenv elpy-module-yasnippet elpy-module-sane-defaults))
 '(elpy-rpc-python-command "python3")
 '(elpy-syntax-check-command "flake8")
 '(elpy-test-discover-runner-command '("python3" "-m" "unittest"))
 '(elpy-test-runner 'elpy-test-discover-runner)
 '(package-selected-packages
   '(rainbow-mode highlight-indent-guides py-isort underline-with-char nord-theme avy python-isort python-black browse-kill-ring use-package flymake-python-pyflakes php-mode yaml-mode atom-dark-theme typescript-mode sr-speedbar atom-one-dark-theme multiple-cursors helm-company nose terraform-mode rainbow-delimiters git-timemachine json-mode aggressive-indent highlight-tail string-utils fish-mode column-marker web-mode sql-indent spaceline rope-read-mode org-agenda-property nyan-mode magit js2-mode jedi-direx helm-projectile helm-filesets helm-ag flycheck exec-path-from-shell elpy editorconfig csv-mode company-jedi anaconda-mode))
 '(pyvenv-mode t)
 '(safe-local-variable-values
   '((eval add-hook 'before-save-hook 'elpy-black-fix-code nil t)
     (eval add-hook 'before-save-hook 'py-isort-before-save)
     (eval add-hook 'before-save-hook #'elpy-black-fix-code nil t))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
