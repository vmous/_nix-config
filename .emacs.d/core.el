;;;;;;;; core.el
;;;;
;; File defining core Emacs configuration.

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
  (desktop-save-mode 1)
  (save-place-mode 1))


;;;; window-numbering
;; https://github.com/nschum/window-numbering.el
(use-package window-numbering
  :ensure t
  :custom-face
  ;; highlight the window number in pink color
  (window-numbering-face ((t (:foreground "DeepPink" :underline "DeepPink" :weight bold)) ))
  :init
  (window-numbering-mode 1))


;;;;;; neotree
;;;; https://github.com/jaypei/emacs-neotree
;;(use-package neotree
;;  :ensure t
;;  :config
;;  (setq neo-smart-open t)
;;  (setq-default neo-dont-be-alone t)
;;  ;; work with projectile
;;  (setq projectile-switch-project-action 'neotree-projectile-action)
;;  (setq neo-theme 'ascii) ; 'classic, 'nerd, 'ascii, 'arrow
;;  :bind
;;  ("<f8>" . neotree-toggle))


;;;; occur
;;
;;(use-package color-moccur
;;  :ensure t
;;  :commands (isearch-moccur isearch-all)
;;  :bind (
;;  ("M-s O" . moccur)
;;    :map
;;    isearch-mode-map
;;    ("M-o" . isearch-moccur)
;;    ("M-O" . isearch-moccur-all))
;;  :init
;;  (setq isearch-lazy-highlight t)
;;  :config
;;  (use-package moccur-edit
;;    :ensure t))
;;
;; (define-key moccur-mode-map "r" 'moccur-edit-mode-in)
;; (define-key moccur-mode-map "\C-x\C-q" 'moccur-edit-mode-in)

;; (define-key moccur-ee-mode-map "r" 'moccur-edit-mode-in)

;;(use-package occur
;;  :config
;;  (add-hook 'occur-mode-hook 'next-error-follow-minor-mode)
;;  :bind ("C-x l o" . occur))
;;
;;(use-package multi-occur
;;  :bind ("C-x l O" . multi-occur))


;;;;; ido
;;; http://wikemacs.org/wiki/Ido
;;; http://www.gnu.org/software/emacs/manual/html_mono/ido.html
;(use-package ido
;  :ensure t
;  :init
;  (ido-mode)
;  (ido-everywhere 1)
;  :config
;  (setq ido-enable-prefix nil
;        ido-enable-flex-matching t
;        ido-case-fold nil
;        ido-auto-merge-work-directories-length -1
;        ido-create-new-buffer 'always
;        ido-use-filename-at-point 'guess
;        ;; show dot (current directory) as initial suggestion
;;        ido-show-dot-for-dired t
;        ;; disable ido faces to see flx highlights
;        ido-use-faces nil))
;
;;;(add-to-list 'load-path "~/.emacs.d/myels/")
;;;(load "ido-describe-prefix-bindings.el")
;;;(require 'ido-describe-prefix-bindings)
;;;(ido-describe-prefix-bindings-mode)
;
;;;;; flx-ido
;;; https://github.com/lewang/flx
;(use-package flx-ido
;  :ensure t
;  :init (flx-ido-mode 1))
;
;;;;; ido-vertical-mode
;;; https://github.com/creichert/ido-vertical-mode.el
;;;(use-package ido-vertical-mode
;;;  :ensure t
;;;  :init (ido-vertical-mode)
;;;  :config
;;;  (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right))
;
;;;;; ido-grid-mode
;;; https://github.com/larkery/ido-grid-mode.el
;(use-package ido-grid-mode
;  :ensure t
;;  :custom-face
;;  (ido-first-match ((t (:inverse-video t))))
;  :init (ido-grid-mode)
;  :config
;  (setq ido-grid-mode-max-columns nil
;        ido-grid-mode-prefix-scrolls t
;        ido-grid-mode-prefix " → "
;        ido-grid-mode-exact-match-prefix " ⇶ "))
;
;;;;; ido-ubiquitous
;;; https://github.com/DarwinAwardWinner/ido-completing-read-plus
;(use-package ido-completing-read+
;  :ensure t
;  :init (ido-ubiquitous-mode 1))
;
;;; Fix ido-ubiquitous for newer packages
;(defmacro ido-ubiquitous-use-new-completing-read (cmd package)
;  `(eval-after-load ,package
;     '(defadvice ,cmd (around ido-ubiquitous-new activate)
;        (let ((ido-ubiquitous-enable-compatibility nil))
;          ad-do-it))))












;; https://lambdaland.org/posts/2023-05-31_warp_factor_refactor/
;; https://kristofferbalintona.me/posts/202202211546/#vertico

(use-package vertico
  :ensure t
  :custom
  (vertico-cycle t)
  (vertico-resize nil)
  (vertico-count 10)
  :bind (:map vertico-map ("<escape>" . minibuffer-keyboard-quit))
  :config
  (setq vertico-multiform-commands
        '((consult-imenu buffer indexed)
          (org-refile grid reverse indexed)
          (consult-grep buffer)))
  :init
  (vertico-mode)
  (vertico-mouse-mode)
  (vertico-multiform-mode))

;; The built-in `savehist-mode' saves minibuffer histories.  Vertico
;; can then use that information to put recently selected options at
;; the top.
;;
;; Further reading: https://protesilaos.com/emacs/dotemacs#h:25765797-27a5-431e-8aa4-cc890a6a913a
(savehist-mode 1)

;(use-package vertico-directory
;  :after vertico
;  ;; More convenient directory navigation commands
;  :bind (:map vertico-map
;              ("RET" . vertico-directory-enter)
;              ("DEL" . vertico-directory-delete-char)
;              ("M-DEL" . vertico-directory-delete-word))
;  :config
;  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
;  )
;
;(setq read-extended-command-predicate
;      #'command-completion-default-include-p)

(use-package marginalia
  :after vertico
  :ensure t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  :init
  (marginalia-mode))

;; https://kristofferbalintona.me/posts/202202211546/
;; https://karthinks.com/software/fifteen-ways-to-use-embark/

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless flex basic partial-completion))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult
  :ensure t
  :bind (;; C-c bindings in `mode-specific-map'
         ;; ("C-c m" . consult-man)
         ;; ("C-c M-x" . consult-mode-command)
         ;; ("C-c h" . consult-history)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ;; ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ;; C-x bindings in `ctl-x-map'
         ;; ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ;; ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ;; ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Other custom bindings
         ;; ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ;; ("M-g e" . consult-compile-error)
         ;; ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ;; ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ;; ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ;; ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ;; ("M-g m" . consult-mark)
         ;; ("M-g k" . consult-global-mark)
         ;; ("M-g i" . consult-imenu)
         ;; ("M-g I" . consult-imenu-multi)
         ;; Minibuffer history
         ;; :map minibuffer-local-map
         ;; ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ;; ("M-r" . consult-history))                ;; orig. previous-matching-history-element
         )

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
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

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)
  )

;; The built-in `recentf-mode' keeps track of recently visited files.
;; You can then access those through the `consult-buffer' interface or
;; with `recentf-open'/`recentf-open-files'.
;;
;; I do not use this facility, because the files I care about are
;; either in projects or are bookmarked.
(recentf-mode 1)

;; https://github.com/oantolin/embark?tab=readme-ov-file#selecting-commands-via-completions-instead-of-key-bindings
(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ;; alternative for `describe-bindings'. Shows local by default. For global,
   ;; prefix with C-u
   ("C-h B" . embark-bindings))

  :init

  ;; Optionally replace the key help with a completing-read interface
  ;; (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config
  (setq prefix-help-command #'embark-prefix-help-command)
  (add-to-list 'vertico-multiform-categories '(embark-keybinding grid))

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))













;;;; Smex
;; https://github.com/nonsequitur/smex
(use-package smex
  :ensure t
  :init (smex-initialize)
  :config (setq smex-save-file (concat user-emacs-directory "smex-items"))
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)))

(add-hook 'prog-mode-hook #'goto-address-prog-mode)


;;;; which-key
;; TODO: Check "ido-describe-prefix-bindings.el" for an alternative
;; https://github.com/justbur/emacs-which-key
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode)
  ;;(which-key-setup-minibuffer)
  ;;(which-key-setup-side-window-right)
  (which-key-setup-side-window-bottom))


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

