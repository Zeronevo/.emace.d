;;;; --------------------------------------------------
;;;; 快捷键绑定
;;;; --------------------------------------------------

;; 三种配置步骤
;; 第一种:通用隐含与leader按键绑定配置
;; (define-prefix-command 'my-map)
;; (global-set-key (kbd "M-n") 'my-map)
;; (global-set-key (kbd "M-n b") 'find-file)

;; 第二种:明确与leader按键绑定配置
;; (define-prefix-command 'my-map)
;; (global-set-key (kbd "M-n") 'my-map)
;; (define-key my-map (kbd "b") 'find-file)

;; 第三种:多重leader按键嵌套配置
;; (define-prefix-command 'my-first-map)
;; (define-prefix-command 'my-second-map)
;; (define-prefix-command 'my-thried-map)

;; (global-set-key (kbd "M-n") 'my-first-map)
;; (define-key my-first-map (kbd "M-p") 'my-second-map)
;; (define-key my-first-map (kbd "M-k") 'my-thried-map)

;; (define-key my-thried-map (kbd "!") 'save-file)
;; (define-key my-thried-map (kbd "k") 'write-file)
;; (define-key my-thried-map (kbd "kj")
;;   (lambda ()
;;     (interactive)
;;     (save-buffer)
;;     (kill-emacs)))

;; =============
;; 开始定义

;; 取消原来默认的定义
(global-unset-key (kbd "M-SPC"))

;; 定义自己的 keymap
(define-prefix-command 'zero-keymap)

;; 将自己的keymap绑定到快捷键
(global-set-key (kbd "M-SPC") 'zero-keymap)

;; 绑定别的keymap
(define-key zero-keymap (kbd "o") 'facemenu-keymap)

;; ---绑定 zero-keymap 功能---

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

;; haskell load file in repl
(define-key zero-keymap (kbd "y") 'company-other-backend)

;; programming format buffer
(define-key zero-keymap (kbd "M-f") 'lsp-format-buffer)

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

;; --- 绑定全局功能---

;; 替换mac下 alt 和 meta 建
;;(when *is-mac*
;;  (setq mac-command-modifier 'meta)
;;  (setq mac-option-modifier 'none))

;; yasnippet
;; Trigger key
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
;; Bind `SPC' to `yas-expand' when snippet expansion available (it
;; will still call `self-insert-command' otherwise).
;;(define-key yas-minor-mode-map (kbd "SPC") yas-maybe-expand)
;; Bind `C-c y' to `yas-expand' ONLY.
(define-key yas-minor-mode-map (kbd "C-<tab>") #'yas-expand)
;;(define-key yas-minor-mode-map (kbd "C-<tab>") #'company-other-backend)

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

;; copy line or region
;;(global-set-key (kbd "M-w") 'zero-copy-line-or-region)

;; open down new line  (kbd "M-RET")
;; (global-set-key (kbd "M-RET") (lambda () (interactive)
;; 				(move-end-of-line nil) (newline)))

;; 手动切换不同输入法
;;(global-set-key (kbd "C-c x") 'zero-cycle-font-method)

;; 在Emacs中切换中文和日文
;;(global-set-key (kbd "M-\\") 'zero-cycle-input-method)


(provide 'key-bindings-cfg)
