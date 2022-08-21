;;;; --------------------------------------------------
;;;; 添加配置 melpa emacs 软件源 (镜像)
;;;; --------------------------------------------------

;; 初始的 gnu 源
;; ("gnu" . "http://elpa.gnu.org/packages/")

;; 清华大学镜像站点
;; ("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
;; ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
;; ("melpa-stable" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa-stable/")
;; ("org" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
;; ("marmalade" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/marmalade/")

;;          gnu 一般是必备的，其它的 elpa 中的包会依赖 gnu 中的包
;;        melpa 滚动升级，收录了的包的数量最大
;; melpa-stable 依据源码的 Tag （Git）升级，数量比 melpa 少，因为很多包作者根本不打 Tag
;;          org 仅仅为了 org-plus-contrib 这一个包，org 重度用户使用
;;    marmalade 似乎已经不维护了，个人不推荐

(setq package-archives
      '(("gnu"		. "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
	("melpa"        . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
	("melpa-stable"	. "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa-stable/")))


;;;; --------------------------------------------------
;;;; 安装包管理配置
;;;; --------------------------------------------------

;; 含有自动加载(###,,,autoload)，不加载也能使用
;;(require 'package)

;; 初始化包管理文件，貌似新版本也自动初始化
(unless (bound-and-true-p package-initialized)
  (package-initialize))

;; 刷新软件源
(unless package-archive-contents (package-refresh-contents))

;; 安装包管理工具 use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; 加载包管理工具 use-package
;;(require 'use-package)

;; 配置包管理工具 use-package
;;(setq use-package-always-ensure t)
;;(setq use-package-always-pin "melpa-stable")
;;(setq use-package-always-defer t)
;;(setq use-package-always-demand t)
;;(setq use-package-expand-minimally t)
;;(setq use-package-verbose t)


(provide 'melpa-cfg)
