# 这是我自己的 Emacs 配置
## 采用了 org-mode 文学编程模式
不一样的地方是：

(org-babel-load-file
 (expand-file-name "settings.org" user-emacs-directory))
 
在 init.el 文件这样配置会生成 settings.el 文件
这样会减慢 Emacs 的启动速度

## 详细配置 init.org
直接修改 init.org 文件
手动生成 init.le  文件
这样就可以直接加载配置文件不会减慢启动速度
