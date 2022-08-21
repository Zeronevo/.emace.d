;; --------------------
;; eglot

;;(require 'eglot)
;; 选择服务器
;; (add-to-list 'eglot-server-programs '(foo-mode . ("foo-language-server" "--args")))
;;(add-to-list 'eglot-server-programs '(c-mode . ("clangd")))
;; c语言启动eglot
;;(add-hook 'c-mode-hook 'eglot-ensure)
;; 绑定快捷键
;;(define-key eglot-mode-map (kbd "C-c h") 'eglot-help-at-point)
;;(define-key eglot-mode-map (kbd "C-c C-f") 'eglot-format-buffer)
;;(define-key eglot-mode-map (kbd "<f6>") 'xref-find-definitions)
;;(define-key eglot-mode-map (kbd "C-c o") 'eglot-code-action-organize-imports)

;; (use-package eglot
;;   :ensure t
;;   :defer 1
;;   :config
;;   (add-to-list 'eglot-server-programs '(rust-mode . ("rust-analyzer")))
;;   (add-hook 'rust-mode-hook 'eglot-ensure))
;;   ;; golang
;;   (add-hook 'go-mode-hook 'eglot-ensure))

;; (defun eglot-format-buffer-on-save ()
;;   (add-hook 'before-save-hook #'eglot-format-buffer -10 t)
;;   (add-hook 'before-save-hook #'eglot-code-action-organize-imports -10 t))
;; (add-hook 'go-mode-hook #'eglot-format-buffer-on-save)

;; --------------------
;; lsp

(use-package lsp-mode
  :ensure t
  ;;:pin melpa-stable
  :defer 1
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :commands (lsp lsp-deferred)
  :hook (go-mode . lsp-deferred)
  ;;:hook (rust-mode . lsp-deferred)
  ;;:hook (haskell-mode . lsp-deferred)
  ;;:hook (racket-mode . lsp-deferred)
  :custom
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  ;;(lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  ;;(lsp-rust-analyzer-display-parameter-hints nil)
  ;;(lsp-rust-analyzer-display-reborrow-hints nil)
  :config
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq lsp-modeline-code-actions-segments '(count icon name))
  ;;(setq lsp-modeline-diagnostics-enable t) ;;Project errors on modeline
  ;;(setq lsp-headerline-breadcrumb-enable-symbol-numbers t)
  ;;(setq lsp-idle-delay 0.500)
  ;;(setq lsp-log-io nil) ;;if set to true can cause a performance hit
  ;;(setq lsp-enable-file-watchers nil)
  (setq lsp-signature-render-documentation nil) ;; 关闭在minibuffer的用eldoc显示的文档
  (setq lsp-completion-provider :none) ;; lsp会自动设置company的backends，需要禁止此功能
  )

(use-package lsp-ui
  :ensure t
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil)
  :commands lsp-ui-mode)

;; --------------------
;; flycheck
(use-package flycheck
  :ensure t
  ;;:pin melpa-stable
  :defer 1)

;; (use-package flycheck-rust
;;   :ensure t
;;   ;;:pin melpa-stable
;;   :defer 1)

;; --------------------
;; yas
(use-package yasnippet
  :ensure t
  ;;:pin melpa-stable
  :config
  ;; main mode
  ;;(yas-global-mode 1)
  ;; minor mode
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode))

(use-package yasnippet-snippets
  :ensure t
  ;;:pin melpa-stable
  :defer 1)

;; --------------------
;; haskell

;; (use-package haskell-mode
;;   :ensure t
;;   :defer 1
;;   )

;; (use-package lsp-haskell
;;   :ensure t
;;   :defer 1
;;   )

;; (defun lsp-haskell-install-save-hooks ()
;;   (add-hook 'before-save-hook #'lsp-format-buffer))
;; (add-hook 'haskell-mode-hook #'lsp-haskell-install-save-hooks)

;; (add-hook 'haskell-interactive-mode-hook
;; 	  (lambda () (linum-mode -1)))

;; --------------------
;; common lisp slime

;;(use-package slime
;;  :config (setq inferior-lisp-program "/usr/bin/sbcl"))

;; --------------------
;; rust
;; (use-package rust-mode
;;   :ensure t
;;   :defer 1
;;   :config
;;   (setq rust-format-on-save t)
;;   (define-key rust-mode-map (kbd "C-c C-c") 'rust-run)
;;   )

;; --------------------
;; racket

;; (use-package racket-mode
;;   :ensure t
;;   :defer 1)

;; --------------------
;; golang

(use-package go-mode
  :ensure t
  ;; :pin melpa
  :defer 1)

;;:config
;;(setq gofmt-command "goimports")
;;(add-hook 'before-save-hook 'gofmt-before-save)
  
;; go-mode default tab is 8, now set to 4
;; (add-hook 'go-mode-hook
;;           (lambda ()
;;             ;;(setq indent-tabs-mode 1)
;;             (setq tab-width 4)))

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (setq tab-width 4)
  (add-hook 'before-save-hook #'lsp-format-buffer)
  (add-hook 'before-save-hook #'lsp-organize-imports))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)


(provide 'programs-install-cfg)
