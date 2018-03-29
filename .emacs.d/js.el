;;;;;;;; js.el
;;;;
;; File defining configuration for Javascript development.

;;;; json-mode
(use-package json-mode
  :ensure t
  :mode "\\.json"
  :init
  (add-hook 'json-mode-hook #'flycheck-mode)
  :config
  (setq json-reformat:indent-width 4)
  (setq js-indent-level 4))
