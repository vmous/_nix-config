;;;;;;;; markdown.el
;;;;
;; File defining configuration for markdown.

;;;; markdown-mode
;; http://jblevins.org/projects/markdown-mode/
;; For the preview required to install
;;  - Ubuntu: sudo apt-get install libtext-multimarkdown-perl
;;  - MacOSX: brew install multimarkdown
;;
;; Know Issues:
;; - Using `markdown-live-preview-mode` (`C-c C-c l`) on a markdown file over
;;   TRAMP results in error:
;;   ```
;;   url-file: File does not exist: file:///ssh:<remote_host>:<path_to_html_file>
;;   ```
;;   The problem is that markdown tries to open a local file by adding the
;;   `file:///` prefix even though the preview html is a remote file.
;;
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
  (add-hook 'markdown-mode-hook 'turn-on-flyspell)
  :init (setq markdown-command "multimarkdown")
  :bind ("<f5>" . markdown-live-preview-mode))
