;;;; --------------------------------------------------
;;;; 自定义变量
;;;; --------------------------------------------------

;; 定义判断操作系统类型的变量
(defconst *is-mac* (eq system-type 'darwin))
(defconst *is-linux* (eq system-type 'gnu/linux))
(defconst *is-windows*
  (or (eq system-type 'ms-dos)
      (eq system-type 'windows-nt)))


;;;; --------------------------------------------------
;;;; 自定义函数
;;;; --------------------------------------------------

;; --------------------
;; 打开配置文件
(defun zero-open-init-file()
  (interactive)
  (find-file user-init-file))

;; --------------------
;; 复制当前行或选中的区域
;; (defun zero-copy-line-or-region()
;;   (interactive)
;;   (if mark-active
;;       (kill-ring-save (region-beginning)
;; 		      (region-end))
;;     (progn
;;       (kill-ring-save (line-beginning-position)
;; 		      (line-end-position))
;;       (message "copied line"))))

;; --------------------
;; 复制一行到当前行下面
;; (line-beginning-position) (line-end-position))
(defun zero-duplicate-line()
  (interactive)
  (let ((col (current-column)))
    ;;(move-beginning-of-line 1)
    ;;(set-mark-command nil)
    ;;(move-end-of-line 1)
    ;;(kill-ring-save (region-beginning) (region-end))
    (kill-ring-save (line-beginning-position) (line-end-position))
    (move-end-of-line 1)
    (newline)
    (yank)
    (move-to-column col)))


;; ====================
;; 移动 word line
;; ====================

;; --------------------
;; 移动当前行到上面一行
(defun zero-move-line-up()
  (interactive)
  (or (= (point-min) (line-beginning-position))
      (let ((col (current-column)))
	(transpose-lines 1)
	(previous-line 2)
	(move-to-column col))))

;; --------------------
;; 移动当前行到下面一行
(defun zero-move-line-down()
  (interactive)
  (or (= (point-max) (+ (line-end-position) 1))
      (let ((col (current-column)))
	(next-line 1)
	(transpose-lines 1)
	(previous-line 1)
	(move-to-column col))))

;; --------------------
;; 移动当前单词向左
(defun zero-move-word-left()
  (interactive)
  (transpose-words -1))

;; --------------------
;; 移动当前单词向右
(defun zero-move-word-right()
  (interactive)
  (transpose-words 1))


;; ====================
;; 字体与输入法
;; ====================

;; --------------------
;; 手动切换不同字体
(defun zero-cycle-font-method()
  "Cycle font in emacs"
  (interactive)
  (or (boundp 'zero-font-list)
      (setq zero-font-list '("Cascadia Code PL SemiBold 13" "Noto Sans Mono CJK SC Regular 13")
	    zero-current-font zero-font-list))
  (if (null (cdr zero-current-font))
      (setq zero-current-font zero-font-list)
    (setq zero-current-font (cdr zero-current-font)))
  (set-frame-font (car zero-current-font))
  (message (car zero-current-font)))

;; --------------------
;; 手动切换中文和日文输入法
;; (defun zero-cycle-input-method()
;;   "Cycle input in emacs"
;;   (interactive)
;;   (or (boundp 'zero-input-list)
;;       (setq zero-input-list '("japanese-katakana" "chinese-py" "japanese")
;; 	    zero-current-input zero-input-list))
;;   (if (null (cdr zero-current-input))
;;       (setq zero-current-input zero-input-list)
;;     (setq zero-current-input (cdr zero-current-input)))
;;   (set-input-method (car zero-current-input)))


(provide 'me-package)
