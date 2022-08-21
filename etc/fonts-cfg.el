;;;; --------------------------------------------------
;;;; 字体设计
;;;; --------------------------------------------------

;; Monaco    Source Code Pro    微软雅黑Monaco
;; Microsoft YaHei Mono    Fira Code Regular
;; Inziu Roboto SC Bold    Inziu Iosevka Slab SC
;; Fira Code Regular 10  Source Han Sans
;; YaHei Consolas Hybrid
;; Noto Sans Mono CJK SC Regular
;; Source Han Sans HW SC VF
;; Sarasa Term Slab SC
;; Sarasa Mono Slab SC     Cascadia Code PL SemiBold
;; 星汉等宽 CN Medium　　　文泉驿等宽正黑

;; 设置字体和大小
;; (set-frame-font "Cascadia Code PL 13")
;;(set-frame-font "Fira Code Regular 13")

;; 单独设置英文字体
(set-face-attribute
 'default nil :font "Cascadia Code PL SemiBold 13")

;; 单独设置中文字体
(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font)
		    charset
		    (font-spec :family "星汉等宽 CN" :size 21)))

;; 设置不同字体比例
;; (setq face-font-rescale-alist
;;      '(("Fira Code" . (/ 20 12.0))
;;        ("Inziu Iosevka SC" . 1.2)
;;        ("Source Han Sans" . 1.2)))

;; 选择性的设置
;;(if *is-windows*
;;    (progn
;;      (set-face-attribute 'default nil :font "Source Code Pro 12")
;;      (dolist (charset '(kana han symbol cjk-misc bopomofo))
;;	(set-fontset-font (frame-parameter nil 'font)
;;			  charset (font-spec :family "Inziu Iosevka SC" :size 22))))
;;  (set-face-attribute 'default nil :font "Source Code Pro 12"))

;; (set-face-attribute 'default nil :font
;;                     (format   "%s:pixelsize=%d" "Fira Code Regular" 20))
;; (dolist (charset '(kana han cjk-misc bopomofo))
;;   (set-fontset-font (frame-parameter nil 'font) charset
;;                     (font-spec :family "Source Han Sans" :size 20)))

;; 设定行间距
;; 如果设置为整数，行间距是像素个数，如果是浮点数将会被视作相对倍数。
;;(setq line-spacing 1.5)
;; or if you want to set it globaly
;;(setq-default line-spacing 0.3)


(provide 'fonts-cfg)
