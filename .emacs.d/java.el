;;;;;;;; java.el
;;;;
;; File defining configuration for Java and other JVM-based development.

;;;; Navigate into dependency sources
;; JVM language servers (e.g., Metals, see scala.el) answer go-to-definition
;; (`M-.', `xref-find-definitions') on symbols defined in external libraries
;; with `jar:file:' URIs into the libraries' source jars; jarchive lets Emacs
;; open them.
;; https://git.sr.ht/~dannyfreeman/jarchive
(use-package jarchive
  :ensure t
  :after lsp-mode
  :config
  (jarchive-mode 1))
