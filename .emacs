;;;; General Settings
(column-number-mode 1)
(delete-selection-mode 1)
(display-time)

;;;; package
;; http://wikemacs.org/wiki/Package.el
;; http://www.emacswiki.org/emacs/ELPA
(require 'package)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

;;;; use-package
;; https://github.com/jwiegley/use-package
(require 'use-package)

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
		   ;; disable ido faces to see flx highlights.
		   ido-use-faces nil))

;;;; flx-ido
;; https://github.com/lewang/flx
(use-package flx-ido
	     :ensure t
	     :init (flx-ido-mode 1))

;;;; ido-vertical-mode
;; https://github.com/creichert/ido-vertical-mode.el
(use-package ido-vertical-mode
	     :ensure t
	     :init (ido-vertical-mode))

;;;; ido-ubiquitous
;; https://github.com/DarwinAwardWinner/ido-ubiquitous
(use-package ido-ubiquitous
	     :ensure t
	     :init (ido-ubiquitous-mode 1))
;; Fix ido-ubiquitous for newer packages
(defmacro ido-ubiquitous-use-new-completing-read (cmd package)
  `(eval-after-load ,package
     '(defadvice ,cmd (around ido-ubiquitous-new activate)
	(let ((ido-ubiquitous-enable-compatibility nil))
	  ad-do-it))))

;;;; Smex
;; https://github.com/nonsequitur/smex
(use-package smex
	     :ensure t
	     :init (smex-initialize)
	     :config (setq smex-save-file (concat user-emacs-directory "smex-items"))
	     :bind ("M-x" . smex))

(add-hook 'prog-mode-hook #'goto-address-prog-mode)

;;;; which-key
;; https://github.com/justbur/emacs-which-key
(use-package which-key
	     :ensure t
	     :diminish which-key-mode
	     :config
	     (which-key-mode)
	     ;;(which-key-setup-minibuffer)
	     ;;(which-key-setup-side-window-right)
	     (which-key-setup-side-window-bottom))

;;;; Markdown
;; TODO change to use the use-package
;; TODO add doc
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))


;;;; Jedi (python autocompletion)
;; TODO change to use the use-package
;; TODO add doc
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:server-args
      '("--sys-path" "/usr/lib/python3/dist-packages"
        "--sys-path" "/usr/local/lib/python3.4/dist-packages"))
(setq jedi:complete-on-dot t)
