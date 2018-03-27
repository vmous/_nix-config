;;;;;;;; latex.el
;;;;
;; File defining configuration for LaTeX editing.

;;;;;; AUCTex/LaTeX
(defvar jazzy/env/latex/bin "")


(when macosx-p
  (setq jazzy/env/latex/bin "/Library/TeX/texbin"))


;;;; AUCTeX
;; https://www.gnu.org/software/auctex/
(use-package tex-site
  :ensure auctex
  :init
  (setenv "PATH" (concat jazzy/env/latex/bin ":" (getenv "PATH")))
  (add-to-list 'exec-path jazzy/env/latex/bin)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (setq LaTeX-math-menu-unicode t)
  ;; Wrap text
  (add-hook 'LaTeX-mode-hook 'turn-on-visual-line-mode)
  (add-hook 'TeX-mode-hook 'turn-on-visual-line-mode)
  ;; Enable flyspell
  (when (require 'flyspell nil t)
    (add-hook 'LaTeX-mode-hook 'turn-on-flyspell)
    (add-hook 'TeX-mode-hook 'turn-on-flyspell)
    (add-hook 'LaTeX-mode-hook 'flyspell-buffer)
    (add-hook 'TeX-mode-hook 'flyspell-buffer))
  ;; Enable math shortcuts (trigger char `)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'TeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  ;; https://www.gnu.org/software/auctex/reftex.html
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (add-hook 'TeX-mode-hook 'turn-on-reftex)
  (setq reftex-cite-format 'natbib)
  (setq reftex-plug-into-AUCTeX t)
  ;; PDFLaTeX mode enabled by-default
  (setq TeX-PDF-mode t))


;;;; cdlatex
;; https://github.com/cdominik/cdlatex
;;
;; A full list of defined abbreviations is available with the command
;; C-c ? (cdlatex-command-help)
(use-package cdlatex
  :ensure t
  :init
  (add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)
  (add-hook 'TeX-mode-hook 'turn-on-cdlatex))


;;;; latex-math-preview
;; https://github.com/emacsmirror/latex-math-preview
;; https://www.emacswiki.org/emacs/LaTeXMathPreview
;;
;; latex-math-preview-expression
;; latex-math-preview-insert-symbol
;(use-package latex-math-preview
;  :ensure t)


;;;; px
;; https://github.com/aaptel/preview-latex
(use-package px
  :ensure t)


;;;; latex-preview-pane
;; https://www.emacswiki.org/emacs/LaTeXPreviewPane
;;
;; Commands
;; * Refresh Preview (bound to M-p)
;; * Open in External Program (Bound to M-P)
(use-package latex-preview-pane
  :ensure t
  :diminish "LPP"
  :pin mepla-latex-preview-pane
  :init
  (latex-preview-pane-enable))
