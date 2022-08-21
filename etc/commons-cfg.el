;;;; --------------------------------------------------
;;;; 常规设置
;;;; --------------------------------------------------

;; 设置界面显示
(menu-bar-mode 0)				;; 删除菜单栏
(scroll-bar-mode 0)				;; 删除滚动条
(tool-bar-mode 0)				;; 删除工具栏
(size-indication-mode t)			;; 显示文件大小
(show-paren-mode t)				;; 显示前面匹配的括号
(electric-pair-mode t)				;; 让括号成对的出现(打左括号出现右括号)
(toggle-truncate-lines t)                       ;; 关闭自动折行功能

;; 设置功能
(setq ring-bell-function 'ignore)		;; 关闭错误提示音
(setq auto-save-default nil)			;; 关闭自动保存文件
(setq make-backup-files nil)			;; 关闭自动备份文件
(setq inhibit-splash-screen t)			;; 关闭Emacs启动画面
;;(setq inhibit-startup-screen t)               ;; 关闭Emacs启动画面方法2
;;(setq gnus-inhibit-startup-message t)         ;; 关闭gnus启动时的画面
;;(setq initial-scratch-message "Fuck")         ;; 启动画面文字
;;(setq frame-title-format "Fuck")              ;; 设置标题栏

;;(mouse-avoidance-mode 'animate)                 ;; 光标过来时鼠标自动离开
(delete-selection-mode t)			;; 打字时删除选中区域
(global-hl-line-mode t)				;; 当前行高亮显示
(global-auto-revert-mode t)			;; 外部修改文件后从新加载

;; 滚动设置
;;(setq scroll-step 0)                          ;; 好像是回滚多少行 默认是0
;;(setq scroll-margin 5)                        ;; 当光标上下差5行时 屏幕开始滚动
(setq scroll-conservatively 100)                ;; 当光标在屏幕边缘时自动回滚到0中心-100不回滚

(setq default-directory "~/")			;; 设置默认路径 minibuffer的
;;(setq initial-buffer-choice "~")		;; 设定打开emacs时的buffer是目录或是某个文件
;;(setq confirm-kill-emacs 'y-or-n-p)		;; 退出emacs时询问是否退出 'yes-or-no-p

;; 显示绝对行号 不要与相对行号同时存在
(global-linum-mode t)
;; 显示相对行号 visual relative
;;(setq display-line-numbers-type 'relative)
;;(global-display-line-numbers-mode t)

;; 全屏打开Emacs
(setq initial-frame-alist (quote ((fullscreen . maximized))))

;; 自动调整行高
;;(setq auto-window-vscroll nil)

;; 打开时画面大小
;;(setq default-frame-alist '((width . 99) (height . 29)))

;; 减少垃圾回收次数 数值调到最大
;;(setq gc-cons-threshold most-positive-fixnum)

;; 以下设置必须放在load主题后才能很好的生效
;;(setq-default cursor-type 'bar)		;;改变光标样式
;;(set-cursor-color "#FF8C00")			;;设置光标颜色
(set-cursor-color "#fcf16e")			;;设置光标颜色
;;(set-background-color "black")		;;屏幕黑色背景
;;(set-foreground-color "white")		;;屏幕白色前景
;;(set-face-background 'region "violet")	;;选中区域背景色
(set-face-background 'region "#b7ba6b")		;;选中区域背景色


(provide 'commons-cfg)
