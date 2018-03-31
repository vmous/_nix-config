;;;;;;;; html.el
;;;;
;; File defining configuration for HTML development.

;;;;;; HTML

;;;; impatient-mode
;; https://github.com/skeeto/impatient-mode
(use-package impatient-mode
  :ensure t)


;;;;;; XML

(require 'hideshow)
(require 'sgml-mode)
(require 'nxml-mode)


(add-to-list 'hs-special-modes-alist
             '(nxml-mode
               "<!--\\|<[^/>]*[^/]>"
               "-->\\|</[^/>]*[^/]>"

               "<!--"
               sgml-skip-tag-forward
               nil))


(add-hook 'nxml-mode-hook 'hs-minor-mode)


;; optional key bindings, easier than hs defaults
(define-key nxml-mode-map (kbd "C-c h") 'hs-toggle-hiding)


;;;; CC-mode
;;(add-hook 'nxml-mode-hook '(lambda ()
;;        (setq ac-sources (append '(ac-source-semantic) ac-sources))
;;	(local-set-key (kbd "RET") 'newline-and-indent)
;;	(linum-mode t)
;;	(semantic-mode t)
;;	(hs-minor-mode t)
;;	(local-set-key (kbd "C-h") 'hs-toggle-hiding)
;;))
