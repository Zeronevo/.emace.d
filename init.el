;;; package --- init file
;;; Commentary:
;;; Code:

;; ===========================================
;; 使用 文件加载方式管理 emacs 配置文件
;; ===========================================

;;;; ------------------------------------
;;;; 初始化
;;;; ------------------------------------

;; 添加路径 require 文件
;;(add-to-list 'load-path (expand-file-name (concat user-emacs-directory "etc")))

;; 当用户手动从Emacs中修改设置，系统自动生成的文件放到下面目录
;;(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; 当有系统自动生成文件时，加载它
;;(when (file-exists-p custom-file) (load-file custom-file))

;; 不进行签名检查
;; ;;(setq package-check-signature nil)

;;;; ------------------------------------
;;;; 加载各种单独的配置文件
;;;; ------------------------------------

;; (require 'me-package)              ;; 加载自定义函数变量包
;; (require 'melpa-cfg)               ;; 配置软件源安装包管理工具 use-package
;; (require 'packages-install-cfg)    ;; 安装软件包并且配置
;; (require 'programs-install-cfg)    ;; 安装编程语言相关包
;; ;;(require 'pyim-install-cfg)      ;; 安装 Emacs 内置输入法
;; (require 'coding-systems-cfg)      ;; 配置编码格式
;; (require 'fonts-cfg)               ;; 配置字体
;; (require 'commons-cfg)             ;; 界面设定等通用配置
;; (require 'others-cfg)              ;; 内置包系统差异等配置
;; (require 'key-bindings-cfg)        ;; 自定义快捷键绑定
;; ;;(require 'lsp-racket)            ;; 参考使用 lsp 不支持 racket


;; ===========================================
;; 使用 org-mode 文学编程管理 emacs 配置文件
;; ===========================================

;; 第一种方式
;; (org-babel-load-file (expand-file-name "~/.emacs.d/myinit.org"))

;; 第二种方式
;; (require 'org)
(org-babel-load-file
 (expand-file-name "settings.org"
                   user-emacs-directory))


;;; init.el ends here
