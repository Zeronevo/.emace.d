;;;; --------------------------------------------------
;;;; 安装软件
;;;; --------------------------------------------------

;; --------------------
;; exec-path-from-shell
;; macOS解决shell和emacs路径不统一

(when (memq window-system '(mac ns))
  (use-package exec-path-from-shell
    :ensure t
    ;; :pin melpa-stable
    :config
    (exec-path-from-shell-initialize)))

;; --------------------
;; dracula-theme
;; 安装主题

(use-package dracula-theme
  :ensure t
  ;; :pin melpa-stable
  :config
  (load-theme 'dracula t))

;; --------------------
;; which-key

(use-package which-key
  :ensure t
  ;; :pin melpa-stable
  :config
  (which-key-mode))

;; --------------------
;; ivy

(use-package ivy
  :ensure t
  ;; :pin melpa-stable
  :config
  (ivy-mode)
  ;; (setq ivy-use-virtual-buffers t)
  ;; (setq ivy-initial-inputs-alist nil)
  ;; (setq enable-recursive-minibuffers t)
  ;; (setq ivy-re-builders-alist '((t . ivy--regex-ignore-order))))
  (setq ivy-count-format "(%d/%d) "))

;; --------------------
;; counsel

(use-package counsel
  :ensure t
  ;; :pin melpa-stable
  :after (ivy)
  :bind (("M-x" . counsel-M-x)
	 ;;("C-c g" . counsel-git)
	 ;;("C-c f" . counsel-recentf)
	 ("C-x b" . counsel-switch-buffer)
	 ("C-x C-b" . ibuffer)
	 ("C-x C-f" . counsel-find-file)))

;; --------------------
;; swiper

(use-package swiper
  :ensure t
  ;; :pin melpa-stable
  :after (ivy)
  :bind (;;("C-r" . swiper-isearch-backward)
	 ("C-s" . swiper)))
  ;; :config
  ;; (setq swiper-action-recenter t)
  ;; (setq swiper-include-line-number-in-search t)

;; --------------------
;; 彩虹括号等

(use-package rainbow-delimiters
  :ensure t
  ;; :pin melpa-stable
  :defer 1
  :config
  (rainbow-delimiters-mode)
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; --------------------
;; 补全 company

;; :bind
;;   (:map company-active-map
;; 	      ("C-n". company-select-next)
;; 	      ("C-p". company-select-previous)
;; 	      ("M-<". company-select-first)
;; 	      ("M->". company-select-last))

(use-package company
  :ensure t
  ;;:pin melpa-stable
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  ;;(setq company-begin-commands '(self-insert-command))
  ;;(setq company-tooltip-limit 20)
  ;;(setq company-require-match nil)
  ;;(setq company-dabbrev-ignore-case t)
  ;;(setq company-dabbrev-downcase nil)
  ;;(setq company-dabbrev-other-buffers 'all)
  ;;(setq company-dabbrev-code-everywhere t)
  ;;(setq company-dabbrev-code-modes t)
  ;;(setq company-dabbrev-code-other-buffers 'all)
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.0)
  (setq company-tooltip-offset-display 'lines)
  (setq company-show-numbers t)
  (setq company-backends
	'(
	   (company-capf 
	   :with company-yasnippet :separate
	   company-dabbrev-code company-gtags
	   company-etags company-keywords)
	   company-bbdb company-semantic company-cmake
	   company-clang company-files
	   company-oddmuse company-dabbrev
	  )))


(provide 'packages-install-cfg)
