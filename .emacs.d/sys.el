;;;;;;;; sys.el
;;;;
;; File defining system features configuration.

;;;; shell
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Shell.html
(use-package shell
  :ensure t
  :config
  (define-key comint-mode-map (kbd "<up>") 'comint-previous-input)
  (define-key comint-mode-map (kbd "<down>") 'comint-next-input))


;;;; TRAMP (Transparent Remote Access, Multiple Protocols)
;; https://www.emacswiki.org/emacs/TrampMode
;;
;; First add the following block to your ~/.ssh/config
;; #+BEGIN_EXAMPLE
;; Host *
;;     # ...
;;     # reuse/multiplex connections
;;     ControlPath ~/.var/ssh/sshmux-%C
;;     ControlMaster auto
;;     ControlPersist yes
;; #+END_EXAMPLE
;;
;; NOTE: You might need to reload the fs attributes for ido autocompletion to
;;       work with newly created files with TRAMP. To do so hit C-l.
(use-package tramp
  :config
  (setq tramp-remote-path '(tramp-default-remote-path "/bin" "/usr/bin" "/sbin" "/usr/sbin")
        tramp-terminal-type "tramp"
        ;; play nice with .ssh/config
        tramp-ssh-controlmaster-options ""
        ;; adds debug buffers that can help with troubleshooting
        ;; check buffers named `*debug tramp/[cache|local/ssh] <BLAH>*`
        tramp-verbose 10))

;;;; * auto-decompress files
(auto-compression-mode 1)

;;;; * auto encrypt/decrypt gpg files
(require 'epa-file)
(epa-file-enable)
