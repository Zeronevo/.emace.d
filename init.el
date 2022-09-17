;;; package --- init file
;;; Commentary:
;;; Code:

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file) (load-file custom-file))

(defconst *is-mac* (eq system-type 'darwin))
(defconst *is-linux* (eq system-type 'gnu/linux))
(defconst *is-windows*
  (or (eq system-type 'ms-dos)
      (eq system-type 'windows-nt)))

(defun zero-config-setup-org()
  (interactive)
  (org-babel-load-file
   (expand-file-name "init.org" user-emacs-directory)))

(defun zero-open-init-file()
  (interactive)
  (find-file user-init-file))

(defun zero-duplicate-line()
  (interactive)
  (let ((col (current-column)))
    (kill-ring-save (line-beginning-position) (line-end-position))
    (move-end-of-line 1)
    (newline)
    (yank)
    (move-to-column col)))

(defun zero-move-line-up()
  (interactive)
  (or (= (point-min) (line-beginning-position))
      (let ((col (current-column)))
        (transpose-lines 1)
        (previous-line 2)
        (move-to-column col))))

(defun zero-move-line-down()
  (interactive)
  (or (= (point-max) (+ (line-end-position) 1))
      (let ((col (current-column)))
        (next-line 1)
        (transpose-lines 1)
        (previous-line 1)
        (move-to-column col))))

(defun zero-move-word-left()
  (interactive)
  (transpose-words -1))

(defun zero-move-word-right()
  (interactive)
  (transpose-words 1))

(defun zero-cycle-font-method()
  "Cycle font in emacs"
  (interactive)
  (or (boundp 'zero-font-list)
      (setq zero-font-list '("SauceCodePro Nerd Font Mono 13" "Noto Sans Mono CJK SC Regular 13")
            zero-current-font zero-font-list))
  (if (null (cdr zero-current-font))
      (setq zero-current-font zero-font-list)
    (setq zero-current-font (cdr zero-current-font)))
  (set-frame-font (car zero-current-font))
  (message (car zero-current-font)))

(setq package-archives
      '(("gnu"		. "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("melpa"            . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
        ("melpa-stable"	. "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa-stable/")))

(unless (bound-and-true-p package-initialized)
  (package-initialize))

(unless package-archive-contents (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :defer t)

(use-package which-key
  :ensure t
  :defer t
  :config
  (which-key-mode))

(use-package ivy
  :ensure t
  :config
  (ivy-mode)
  (setq ivy-count-format "(%d/%d) "))

(use-package counsel
  :ensure t
  :after (ivy)
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-switch-buffer)
         ("C-x C-b" . ibuffer)
         ("C-x C-f" . counsel-find-file)))

(use-package swiper
  :ensure t
  :after (ivy)
  :bind (("C-s" . swiper)))

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :init (rainbow-delimiters-mode)
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package company
  :ensure t
  :defer t
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.0)
  (setq company-tooltip-offset-display 'lines)
  (setq company-show-numbers t)
  (setq company-backends
        '((company-capf 
           :with company-yasnippet :separate
           company-dabbrev-code company-gtags
           company-etags company-keywords)
           company-bbdb company-semantic company-cmake
           company-clang company-files
           company-oddmuse company-dabbrev)))

(use-package lsp-mode
  :ensure t
  :defer t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :commands (lsp lsp-deferred)
  :hook (go-mode . lsp-deferred)
  :hook (c-mode . lsp-deferred)
  :custom
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  :config
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq lsp-modeline-code-actions-segments '(count icon name))
  (setq lsp-signature-render-documentation nil) ;; 关闭在minibuffer的用eldoc显示的文档
  (setq lsp-completion-provider :none) ;; lsp会自动设置company的backends，需要禁止此功能
  )

(use-package lsp-ui
  :ensure t
  :defer t
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil)
  :commands lsp-ui-mode)

(use-package flycheck
  :ensure t
  :defer t)

(use-package yasnippet
  :ensure t
  :defer t
  :config
  (yas-global-mode)
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "C-<tab>") #'yas-expand))

(use-package yasnippet-snippets
  :ensure t
  :after (yasnippet))

(defun lsp-go-install-save-hooks ()
  (setq tab-width 4)
  (add-hook 'before-save-hook #'lsp-format-buffer)
  (add-hook 'before-save-hook #'lsp-organize-imports))

(use-package go-mode
  :ensure t
  :defer t
  :config
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks))

(defun lsp-c-mode-hooks ()
  (setq c-basic-offset 4)
  (c-toggle-comment-style -1)
  (add-hook 'before-save-hook #'lsp-format-buffer))
(add-hook 'c-mode-hook #'lsp-c-mode-hooks)

;; 默认读取文件编码
(prefer-coding-system 'utf-8)
;; 默认写入文件的编码格式
(set-buffer-file-coding-system 'utf-8)
;; 新建文件编码
(set-default-coding-systems 'utf-8)
;; 键盘输入
(set-keyboard-coding-system 'utf-8)
;; 终端
(set-terminal-coding-system 'utf-8)
;; 文件名
(set-file-name-coding-system 'utf-8)

(set-face-attribute
 'default nil :font "SauceCodePro Nerd Font Mono 13")

(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font)
                    charset
                    (font-spec :family "Xiaolai Mono SC" :size 26)))

(menu-bar-mode 0)				;; 删除菜单栏
(scroll-bar-mode 0)			;; 删除滚动条
(tool-bar-mode 0)				;; 删除工具栏
(size-indication-mode t)			;; 显示文件大小
(show-paren-mode t)			;; 显示前面匹配的括号
(electric-pair-mode t)			;; 让括号成对的出现(打左括号出现右括号)
(toggle-truncate-lines t)                 ;; 关闭自动折行功能

(setq ring-bell-function 'ignore)		;; 关闭错误提示音
(setq auto-save-default nil)		;; 关闭自动保存文件
(setq make-backup-files nil)		;; 关闭自动备份文件
(setq inhibit-splash-screen t)		;; 关闭Emacs启动画面

(delete-selection-mode t)			;; 打字时删除选中区域
(global-hl-line-mode t)		        ;; 当前行高亮显示
(global-auto-revert-mode t)		;; 外部修改文件后从新加载
(setq default-directory "~/")	        ;; 设置默认路径 minibuffer的

;; 打开时画面大小
(setq default-frame-alist '((width . 99) (height . 29)))

(setq scroll-conservatively 100)          ;; 当光标在屏幕边缘时自动回滚到0中心-100不回滚

(global-linum-mode t)

(when *is-windows*
  (setq default-directory "e:/"))

(add-hook 'eshell-mode-hook
          (lambda ()
            (company-mode -1)))

(setq org-ellipsis " ◄")
(add-hook 'org-mode-hook
          (lambda ()
            (org-indent-mode)
            (linum-mode -1)))

;; 取消原来默认的定义
(global-unset-key (kbd "M-SPC"))

;; 定义自己的 keymap
(define-prefix-command 'zero-keymap)

;; 将自己的keymap绑定到快捷键
(global-set-key (kbd "M-SPC") 'zero-keymap)

;; 绑定原始的 keymap
(define-key zero-keymap (kbd "o") 'facemenu-keymap)

;; 在两个之间只留下一个空格
(define-key zero-keymap (kbd "n") 'just-one-space)
;; 清除每一行末尾没用的空白
(define-key zero-keymap (kbd "m") 'whitespace-cleanup)
;; 输入 eshell 命令
(define-key zero-keymap (kbd "e") 'eshell)
;; open init file
(define-key zero-keymap (kbd "<f2>") 'zero-open-init-file)
;; 手动切换不同输入法
(define-key zero-keymap (kbd "x") 'zero-cycle-font-method)
;; counsel-recentf
(define-key zero-keymap (kbd "r") 'counsel-recentf)
;; 重复上一个命令
(define-key zero-keymap (kbd "M-r") 'repeat)
;; copy current line to below
(define-key zero-keymap (kbd "M-d") 'zero-duplicate-line)

;; move word line
(define-key zero-keymap (kbd "M-p") 'zero-move-line-up)
(define-key zero-keymap (kbd "M-n") 'zero-move-line-down)
(define-key zero-keymap (kbd "M-b") 'zero-move-word-left)
(define-key zero-keymap (kbd "M-f") 'zero-move-word-right)

;; programming format buffer
(define-key zero-keymap (kbd "f") 'lsp-format-buffer)

;; 分屏窗口操作
(define-key zero-keymap (kbd "wp") 'windmove-swap-states-up)
(define-key zero-keymap (kbd "wn") 'windmove-swap-states-down)
(define-key zero-keymap (kbd "wb") 'windmove-swap-states-left)
(define-key zero-keymap (kbd "wf") 'windmove-swap-states-right)

(define-key zero-keymap (kbd "w[") 'shrink-window-horizontally)
(define-key zero-keymap (kbd "w]") 'enlarge-window-horizontally)
(define-key zero-keymap (kbd "w;") 'shrink-window)
(define-key zero-keymap (kbd "w'") 'enlarge-window)
(define-key zero-keymap (kbd "w/") 'balance-windows)

;; 多窗口切换分屏控制
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-below)
(global-set-key (kbd "M-3") 'split-window-right)

;; next or previous buffer
(global-set-key (kbd "M-[") 'previous-buffer)
(global-set-key (kbd "M-]") 'next-buffer)

;; page up or down one line
(global-set-key (kbd "C-M-n") 'scroll-up-line)
(global-set-key (kbd "C-M-p") 'scroll-down-line)

;;; init.el ends here
