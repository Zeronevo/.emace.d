;;;; --------------------------------------------------
;;;; 其它一些须要设置的东西
;;;; --------------------------------------------------

;; --------------------
;; 如果是windows就把默认minibuffer打开路径设置为下面的 
(when *is-windows*
  (setq default-directory "e:/"))

;; --------------------
;; eshell no need company
(add-hook 'eshell-mode-hook
            (lambda ()
              (company-mode -1)))

;; --------------------
;; org-mode config

;; 把org-mode的时间显示改成英文
;; (setq system-time-locale "C")
;; (add-hook 'org-mode-hook
;; 	  (lambda ()
;; 	    (set (make-local-variable 'system-time-locale) "C")))

(setq org-ellipsis " ◄")
(add-hook 'org-mode-hook
	  (lambda ()
	    ;;(setq truncate-lines nil) ;; org-mode 自动折行功能
	    (org-indent-mode)
	    (linum-mode -1)))

;; --------------------
;; dired-mode config
;;(put 'dired-find-alternate-file 'disabled nil)

;; (with-eval-after-load 'dired
;;   (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file))

;; (setq dired-recursive-deletes 'always)
;; (setq dired-recursive-copies 'always)

;; (add-hook 'dired-mode-hook
;;           (lambda ()
;;             (define-key dired-mode-map (kbd "^")
;;               (lambda () (interactive) (find-alternate-file "..")))))


(provide 'others-cfg)
