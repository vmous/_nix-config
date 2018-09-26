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
  :config
  ;; highlight the window number in pink color
  (custom-set-faces '(window-numbering-face
		      ((t(:foreground "DeepPink" :underline "DeepPink" :weight bold)))))
  (window-numbering-mode))


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


;;;; ido
;; http://wikemacs.org/wiki/Ido
;; http://www.gnu.org/software/emacs/manual/html_mono/ido.html
(use-package ido
  :ensure t
  :init
  (ido-mode)
  (ido-everywhere 1)
  :config
  (setq ido-enable-prefix nil
	ido-enable-flex-matching t
	ido-case-fold nil
	ido-auto-merge-work-directories-length -1
	ido-create-new-buffer 'always
	ido-use-filename-at-point 'guess
        ;; show dot (current directory) as initial suggestion
;        ido-show-dot-for-dired t
	;; disable ido faces to see flx highlights
	ido-use-faces nil))

;;(add-to-list 'load-path "~/.emacs.d/myels/")
;;(load "ido-describe-prefix-bindings.el")
;;(require 'ido-describe-prefix-bindings)
;;(ido-describe-prefix-bindings-mode)

;;;; flx-ido
;; https://github.com/lewang/flx
(use-package flx-ido
  :ensure t
  :init (flx-ido-mode 1))

;;;; ido-vertical-mode
;; https://github.com/creichert/ido-vertical-mode.el
;;(use-package ido-vertical-mode
;;  :ensure t
;;  :init (ido-vertical-mode)
;;  :config
;;  (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right))

;;;; ido-grid-mode
;; https://github.com/larkery/ido-grid-mode.el
(use-package ido-grid-mode
  :ensure t
  :init (ido-grid-mode)
  :config
  (setq-default ido-grid-mode-prefix-scrolls t))

;;;; ido-ubiquitous
;; https://github.com/DarwinAwardWinner/ido-completing-read-plus
(use-package ido-completing-read+
  :ensure t
  :init (ido-ubiquitous-mode 1))

;; Fix ido-ubiquitous for newer packages
(defmacro ido-ubiquitous-use-new-completing-read (cmd package)
  `(eval-after-load ,package
     '(defadvice ,cmd (around ido-ubiquitous-new activate)
	(let ((ido-ubiquitous-enable-compatibility nil))
	  ad-do-it))))

;;;; ido-occur
;; https://github.com/danil/ido-occur
(use-package ido-occur
  :ensure t
  :config
  (define-key isearch-mode-map (kbd "C-o") 'ido-occur-from-isearch))


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

