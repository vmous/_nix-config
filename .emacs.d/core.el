;;;;;;;; core.el
;;;;
;; File defining core Emacs configuration.


;;;;;; savehist-mode
;;;; https://github.com/emacs-mirror/emacs/blob/master/lisp/savehist.el
;; Remember minibuffer history (e.g., searches, commands etc.).
;;
;; Vertico use savehist information to put recently selected options at the top.
;;
;; Further reading: https://protesilaos.com/emacs/dotemacs#h:25765797-27a5-431e-8aa4-cc890a6a913a
(savehist-mode 1)

;;;;;; save-place-mode
;;;; https://github.com/emacs-mirror/emacs/blob/master/lisp/saveplace.el
;; Remember cursor position in files you visit.
(save-place-mode 1)

;;;;;; recentf-mode
;;;; https://github.com/emacs-mirror/emacs/blob/master/lisp/recentf.el
;; Remember recently visited files.
;;
;; You can then access those through the `consult-buffer' interface or
;; with `recentf-open'/`recentf-open-files'.
;;
;; Further reading:
;; - https://protesilaos.com/emacs/dotemacs#h:f9aa7523-d88a-4080-add6-073f36cb8b9a
(recentf-mode 1)     ; Remember recently opened files

;;;;;; command-completion-default-include-p
;;;; https://emacs.stackexchange.com/questions/77599
;; Filters out command completion candidates that are irrelevant to the buffer's
;; major mode.
;;
;; This is particularly useful, together with interactive completion tools
;; (e.g., Vertico) whose rendered list of available recommendations can be
;; overwhelming if not properly targeted. This configuration, for example, if
;; you are in 'python-mode', you won't see 'org-columns' in the Vertico list, as
;; that command only works in Org mode. This keeps the completion list shorter
;;and more relevant.
(setq read-extended-command-predicate
      #'command-completion-default-include-p)


;;;; desktop
;; https://www.emacswiki.org/emacs/Desktop
(use-package desktop
  :ensure t
  :config
  (setq desktop-auto-save-timeout 3600
        desktop-files-not-to-save nil
        desktop-buffers-not-to-save nil)
  (add-to-list 'desktop-modes-not-to-save 'dired-mode)
  (add-to-list 'desktop-modes-not-to-save 'Info-mode)
  (add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
  (add-to-list 'desktop-modes-not-to-save 'fundamental-mode)
  (desktop-save-mode 1))


;;;; window-numbering
;; https://github.com/nschum/window-numbering.el
(use-package window-numbering
  :ensure t
  :custom-face
  ;; highlight the window number in pink color
  (window-numbering-face ((t (:foreground "DeepPink" :underline "DeepPink" :weight bold)) ))
  :init
  (window-numbering-mode 1))


;;;;;;;;;; START Modern Emacs Completion Stack ;;;;;;;;;;
;; Vertico	The UI providing a list of candidates when pressing `M-x'.
;; Orderless	The search logic (e.g., typing "config org" let's you find `org-mode-config'.
;; Marginalia	The information provider (e.g., descriptions, file sizes, icons next to commands etc.).
;; Consult	Pro toolset providing enhanced versions of standard command
;; Embark	Act on selected files. Vertico only allows to open files; embark lets you do things like rename, copy etc.)

;;;;;; vertico
;;;; https://github.com/minad/vertico
(use-package vertico
  :ensure t
  :demand t
  :custom
  (vertico-cycle t)
  (vertico-resize nil)
  (vertico-count 10)
  (vertico-preselect 'first)
  :bind (:map vertico-map ("C-g" . minibuffer-keyboard-quit))
  :config
  (setq vertico-multiform-commands
        '((consult-imenu buffer indexed)
          (org-refile grid reverse indexed)
          (consult-grep buffer)
          (consult-yank-pop indexed)
          (consult-flycheck)
          (consult-lsp-diagnostics)))
  :init
  (vertico-mode)
  (vertico-mouse-mode)
  (vertico-multiform-mode))


;;;;;; vertico-directory
;;;; https://github.com/minad/vertico/blob/main/extensions/vertico-directory.el
(use-package vertico-directory
  :after vertico
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char))
  :config
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))


;;;;;; orderless
;;;; https://github.com/oantolin/orderless
;;
;; Inspired (copy-pasted) configuration from:
;; - https://kristofferbalintona.me/posts/202202211546/#orderless
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless flex basic partial-completion))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))


;;;;;; marginalia
;;;; https://github.com/minad/marginalia
(use-package marginalia
  :after vertico
  :ensure t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))


;;;;;; nerd-icons-completion
;;;; https://github.com/rainstormstudio/nerd-icons-completion
;;
;; Requires to run the following once: M-x nerd-icons-install-fonts
;; (check https://github.com/rainstormstudio/nerd-icons.el )
(use-package nerd-icons-completion
  :after marginalia
  :ensure t
  :hook
  ;; Instead of the following
  ;; :init (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)
  (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :init
  (nerd-icons-completion-mode))


;;;;;; consult
;;;; https://github.com/minad/consult
(use-package consult
  :ensure t
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind
  ( :map global-map
    ;; C-x bindings in `ctl-x-map'
    ("C-x b" . consult-buffer)
    ;; C-c bindings in `mode-specific-map'
    ("C-c h" . consult-history)
    ;; ("C-c m" . consult-man)
    ;; M-g bindings in `goto-map'
    ("M-g g" . consult-goto-line)
    ("M-g M-g" . consult-goto-line)
    ("M-g o" . consult-outline)
    ("M-g i" . consult-imenu)
    ("M-g I" . consult-imenu-multi)
    ("M-g e" . consult-compile-error)
    ("M-g f" . consult-flymake)
    ("M-g m" . consult-mark)
    ("M-g k" . consult-global-mark)
    ;; M-s bindings in `search-map'
    ("M-s g" . consult-grep)
    ("M-s G" . consult-git-grep)
    ("M-s f" . consult-find)
    ("M-s l" . consult-locate)
    ;; Misc.
    ("M-y" . consult-yank-pop))
  :config
  ;; consult-history: setup which histories to show
  (add-to-list 'consult-mode-histories '(vc-git-log-edit-mode . log-edit-comment-ring))
  ;; consult-find: ignore directory patterns
  (setq consult-find-args
          (concat "find . -not ( "
                  "-path */.git* -prune "
                  "-or -path */.cache* -prune )"))
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
)


;;;;;; embark
;;;; https://github.com/oantolin/embark
;;
;; Further reading:
;; - https://karthinks.com/software/fifteen-ways-to-use-embark/
;; - https://lambdaland.org/posts/2023-05-31_warp_factor_refactor/;;
;; - https://github.com/oantolin/embark?tab=readme-ov-file#selecting-commands-via-completions-instead-of-key-bindings
(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)         ;; The "Right Click" menu
   ("C-;" . embark-dwim)        ;; Do what I mean (context sensitive)
   ("C-h B" . embark-bindings)) ;; Better way to explore keybindings
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  :init
  ;; Use the minibuffer (Vertico) to select actions
  (setq embark-prompter #'embark-completing-read-prompter)
  ;; Replace the standard 'describe-prefix-bindings' with a searchable Embark version
  (setq prefix-help-command #'embark-prefix-help-command))


;;;;;; embark-consult
;;;; https://github.com/oantolin/embark/blob/master/embark-consult.el
(use-package embark-consult
  :ensure t ; helps Embark and Consult talk to each other
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;;;;;;;;; END Modern Emacs Completion Stack ;;;;;;;;;;


;;;;;; which-key
;;;; https://github.com/justbur/emacs-which-key
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init
  (which-key-mode))


;;;; discover-my-major
;; https://github.com/jguenther/discover-my-major
(use-package discover-my-major
  :ensure t
  :bind ("C-h M" . discover-my-major))


;;;;;; keycast
;;;; https://github.com/tarsius/keycast
(use-package keycast
  :ensure t
  :custom
  (keycast-mode-line-format "%2s%k%c%R")
  (keycast-mode-line-remove-tail-elements nil)
  :config
  (dolist (input '(self-insert-command org-self-insert-command))
    (add-to-list 'keycast-substitute-alist `(,input "." "Typing…")))
  (dolist (event '( mouse-event-p mouse-movement-p mwheel-scroll handle-select-window
                    mouse-set-point mouse-drag-region))
    (add-to-list 'keycast-substitute-alist `(,event nil)))
  :init
  (keycast-mode-line-mode))
