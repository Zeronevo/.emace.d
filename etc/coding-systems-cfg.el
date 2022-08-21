;;;; --------------------------------------------------
;;;; 字符编码
;;;; --------------------------------------------------

;; 这个如果设定了在windows下会出现莫名的乱码不认字体
;; (setq locale-coding-system 'utf-8)

;; windows下设定语言环境会出现字体混乱
;; (set-language-environment 'utf-8)

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

;; 下面两个是设置剪切板的字符集
;; 因为windows是utf-16-le所以设置后复制粘贴会出现乱码
;;(set-clipboard-coding-system 'utf-8)
;;(set-selection-coding-system 'utf-8)


(provide 'coding-systems-cfg)
