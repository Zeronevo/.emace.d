;; --------------------
;; 输入法
(use-package pyim
  :ensure t
  ;; :pin melpa
  :defer 1
  :config
  ;;(require 'pyim)
  (setq default-input-method "pyim")
  ;; 金手指设置，可以将光标处的编码，比如：拼音字符串，转换为中文。
  ;;(global-set-key (kbd "C-M-j") 'pyim-convert-string-at-point)
  ;;(pyim-default-scheme 'quanpin)  ;; 我使用全拼  'wubi
  ;;(setq pyim-page-length 5)     ;; 显示5个候选词。
  (setq pyim-page-style 'one-line)
  ;;(define-key pyim-mode-map "." 'pyim-page-next-page)
  ;;(define-key pyim-mode-map "," 'pyim-page-previous-page)
  (setq pyim-indicator-cursor-color '("red" nil)))

(use-package pyim-basedict
  :ensure t
  ;; :pin melpa
  :after (pyim)
  :config
  ;;(require 'pyim-basedict)
  (pyim-basedict-enable))

(provide 'pyim-install-cfg)
