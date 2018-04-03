;;;;;;;; markdown.el
;;;;
;; File defining configuration for markdown.

;;;; markdown-mode
;; http://jblevins.org/projects/markdown-mode/
;; For the preview required to install
;;  - Ubuntu: sudo apt-get install libtext-multimarkdown-perl
;;  - MacOSX: brew install multimarkdown
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (use-package markdown-mode+ :ensure t :defer t)
  (use-package markdown-preview-mode :ensure t :defer t)
  (use-package markdown-preview-eww :ensure t :defer t)
  ;; splitting vertically when openning preview in eww
  (setq split-height-threshold nil)
  :init (setq markdown-command "multimarkdown")
  :bind ("<f5>" . markdown-live-preview-mode))
