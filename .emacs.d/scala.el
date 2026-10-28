;;;;;;;; scala.el
;;;;
;; File defining configuration for Scala development.
;;
;; We use Metals (https://scalameta.org/metals), the Scala language server
;; built on the Language Server Protocol and developed by Scala Center
;; and VirtusLab. Follows the official Metals setup for Emacs:
;; https://scalameta.org/metals/docs/editors/emacs
;;
;; `scala-mode' provides the major mode, `sbt-mode' runs sbt, and `lsp-mode'
;; with `lsp-metals' connects to Metals for completion, navigation,
;; diagnostics and code actions. `lsp-mode', completion (`company'),
;; diagnostics (`flycheck') and snippets (`yasnippet') reuse the setup in
;; prog.el.
;;
;; Requirements: Java 17 or later (`JAVA_HOME'). On first use in a Scala buffer
;; lsp-mode offers to download Metals (or run `M-x lsp-install-server'; or
;; install it manually with `coursier install metals' and put it on `PATH').

;;;; Scala mode
;; https://github.com/hvesalai/emacs-scala-mode
(use-package scala-mode
  :ensure t
  :interpreter ("scala" . scala-mode)
  :init
  (add-hook 'scala-mode-hook
            (lambda () (setq prettify-symbols-alist scala-prettify-symbols-alist)
              (prettify-symbols-mode)))
  ;; Start Metals through lsp-mode (see prog.el) in Scala buffers.
  (add-hook 'scala-mode-hook #'lsp)
  :config
  ;; Adding asterisk in newline for multiline comments.
  (defun lunaryorn-newline-and-indent-with-asterisk ()
    (interactive)
    (newline-and-indent)
    (scala-indent:insert-asterisk-on-multiline-comment))
  (define-key scala-mode-map (kbd "RET")
    #'lunaryorn-newline-and-indent-with-asterisk)
  (define-key scala-mode-map (kbd "C-c C-v c") #'sbt-clear))

;;;; sbt
;; https://github.com/hvesalai/emacs-sbt-mode
(use-package sbt-mode
  :ensure t
  :commands sbt-start sbt-command sbt-clear
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  ;; sbt-supershell kills sbt-mode: https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false")))

;;;; Metals backend for lsp-mode
;; https://github.com/emacs-lsp/lsp-metals
;;
;; Pinned to MELPA: the latest MELPA Stable release (1.2.0, 2021) fails to load
;; against current treemacs ("Eager macro-expansion failure:
;; (wrong-number-of-arguments (1 . 1) 0)" in `lsp-metals-treeview.el').
(use-package lsp-metals
  :ensure t
  :pin melpa
  :after lsp-mode
  :init
  ;; lsp-metals installs Metals with coursier, which it downloads first, but it
  ;; always picks the x86_64 build on macOS. Under Rosetta that build aborts
  ;; ("The current machine does not support all of the following CPU features
  ;; ...") and the install silently produces no server. Use the native build on
  ;; Apple Silicon. Must be set before lsp-metals loads (read at load time).
  (when jazzy/env/is-macosx-arm
    (setq lsp-metals-coursier-download-url
          "https://github.com/coursier/launchers/raw/master/cs-aarch64-apple-darwin.gz")))
