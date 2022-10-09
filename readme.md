# On Linux
# 轻量级 Emacs 配置
基本上都是默认配置
简单安装了常用软件
调整了主题和一些特性

详细参考 init.org 文件
## 采用了 org-mode 文学编程模式
### 配置 init.el
```
(org-babel-load-file
    (expand-file-name "settings.org" user-emacs-directory))
```
然后编写   settings.org 文件
会自动生成 settings.el  文件
这样会减慢 Emacs 的启动速度
### 变种思路
直接编写 init.org 文件
手动生成 init.el  文件
这样就可以直接加载配置文件不会减慢启动速度

可以在 *scratch* 文件中加入
```
(org-babel-load-file
    (expand-file-name "init.org" user-emacs-directory))
```
C-x C-e 后就会生成 init.el
之后可以直接执行 M-x
zero-config-setup-org
快速生成 init.el
