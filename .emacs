;;;; customize
;; http://ergoemacs.org/emacs/emacs_custom_system.html
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("40f6a7af0dfad67c0d4df2a1dd86175436d79fc69ea61614d668a635c2cd94ab" default)))
 '(package-selected-packages
   (quote
    (highlight-symbol magit flycheck-tip irony-eldoc flycheck-irony flycheck company-irony-c-headers company-gtags company-irony company ggtags yasnippet sr-speedbar zenburn-theme which-key use-package smex ido-vertical-mode ido-ubiquitous flx-ido auto-complete))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;; General Settings
(column-number-mode 1)
(delete-selection-mode 1)
(display-time)

;;;; General Key Bindings
(global-set-key (kbd "C-x c") 'customize)
(global-set-key (kbd "C-x r p") 'string-insert-rectangle)
;; windmove
(global-set-key (kbd "C-c <left>") 'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>") 'windmove-up)
(global-set-key (kbd "C-c <down>") 'windmove-down)
;; sr-speedbar
(global-set-key (kbd "<f1>") 'sr-speedbar-toggle)


;;;; package
;; http://wikemacs.org/wiki/Package.el
;; http://www.emacswiki.org/emacs/ELPA
(require 'url-handlers) ;; TODO: This line is a workaround to fix a bug. Remove at some point!
(require 'package)
;;(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
;;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
;;(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

;;;; use-package
;; https://github.com/jwiegley/use-package
;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;;;; themes
;;(use-package solarized-theme
;;  :ensure t)
;;(load-theme 'solarized-dark)
(use-package zenburn-theme
  :ensure t)
(load-theme 'zenburn)

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

;;;; markdown-mode
;; http://jblevins.org/projects/markdown-mode/ 
;; For the preview required to install libtext-multimarkdown-perl
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  ;; splitting vertically when openning preview in eww
  (setq split-height-threshold nil)
  :init (setq markdown-command "multimarkdown")
  :bind ("<f5>" . markdown-live-preview-mode))


;;;;;;;; Developement



;;;;;; Generic



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CEDET
;;
;; Semantic
;;(use-package semantic
;;  :ensure t)
;;(semantic-mode 1)
;;(global-semantic-idle-scheduler-mode t)
;;(global-semantic-idle-completions-mode t)
;;(global-semantic-decoration-mode t)
;;(global-semantic-highlight-func-mode t)
;;(global-semantic-show-unmatched-syntax-mode t)
;;
;;;; CC-mode
;;(add-hook 'c-mode-common-hook '(lambda ()
;;        (setq ac-sources (append '(ac-source-semantic) ac-sources))
;;	(local-set-key (kbd "RET") 'newline-and-indent)
;;	(linum-mode t)
;;	(semantic-mode t)
;;	(hs-minor-mode t)
;;	(local-set-key (kbd "C-h") 'hs-toggle-hiding)
;;))

;; Autocomplete
;;(require 'auto-complete-config)
;;(add-to-list 'ac-dictionary-directories (expand-file-name
;;             "~/.emacs.d/elpa/auto-complete-20160310.2248/dict"))
;;(setq ac-comphist-file (expand-file-name
;;             "~/.emacs.d/ac-comphist.dat"))
;;(ac-config-default)


;;;; end CEDET




;;;;;; Alternative CEDET
;;(semantic-mode 1)

;;(use-package auto-complete
;;  :ensure t
;;  :config
;;  ;; replace the `completion-at-point' and `complete-symbol' bindings in
;;  ;; irony-mode's buffers by irony-mode's function
;;  (defun jazzy/mode-cedet-hook ()
;;    (add-to-list 'ac-sources 'ac-source-gtags)
;;    (add-to-list 'ac-sources 'ac-source-semantic))
;;  (add-hook 'c-mode-common-hook 'jazzy/c-mode-cedet-hook))

;;;; EDE
;;(global-ede-mode 1)
;;(ede-enable-generic-projects)
;;

;;;;;; end Alternative CEDET
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;






;;;; CSCOPE
;;
;;
;; Installation
;; - Mac
;;   $ brew install cscope
;; TODO

;;;; GTAGS
(use-package ggtags
  :ensure t)
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
              (ggtags-mode 1))))

(define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
(define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
(define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
(define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
(define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
(define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)

(define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)

;;;; Find files in Tags File
;;;; https://www.emacswiki.org/emacs/InteractivelyDoThings#toc12
;;(setq ggtags-completing-read-function
;;      (lambda (&rest args)
;;        (apply #'ido-completing-read
;;               (car args)
;;               (all-completions "" ggtags-completion-table)
;;               (cddr args))))


;;;;;; C/C++/Obj-C


;;;; irony-mode
;; https://github.com/Sarcasm/irony-mode
;;
;; Installation instructions
;;
;; - Mac
;;   Install the gtags
;;     $ brew install global
;;     If you cannot find the utility run `brew doctor` to see what went wrong
;;     Then on the package/project root run
;;     $ gtags
;;
;;   Install the irony server
;;     M-x irony-install-server
;;     In case of "Could NOT find LibClang (missing: LIBCLANG_LIBRARY LIBCLANG_INCLUDE_DIR)"
;;     i.   make sure llvm is installed on your system
;;          $ brew install llvm --with-clang --with-asan
;;     ii. if llvm is install then add a flag
;;         -DCMAKE_PREFIX_PATH=/usr/local/opt/llvm after cmake to specify where the
;;         llvm is
;;
;; - Linux
;;   TODO
;;     https://gist.github.com/soonhokong/7c2bf6e8b72dbc71c93b
;;
;; Configuration
;; - By hand
;; - Using customize
;;   M-x customize-group RET irony RET
;;
(use-package irony
  :ensure t
  :defer t
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  :config
  ;; replace the `completion-at-point' and `complete-symbol' bindings in
  ;; irony-mode's buffers by irony-mode's function
  (defun jazzy/irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))
  (add-hook 'irony-mode-hook 'jazzy/irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

;;;; company-mode/company-irony
;; https://github.com/company-mode/company-mode
;; https://github.com/Sarcasm/company-irony
(use-package company
  :ensure t
  :defer t
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  (use-package company-irony :ensure t :defer t)
  (use-package company-irony-c-headers :ensure t :defer t)
  (setq company-idle-delay              nil
	company-minimum-prefix-length   2
	company-show-numbers            t
	company-tooltip-limit           20
	company-dabbrev-downcase        nil
	company-backends                '((company-semantic company-irony-c-headers company-irony company-gtags))
	;;company-backends                '((company-irony company-gtags))
	)
  :bind ("C-;" . company-complete-common))

;;;; yasnippet
;; https://github.com/capitaomorte/yasnippet
;; http://capitaomorte.github.io/yasnippet/
(use-package yasnippet
  :ensure t
  :defer t)
(yas-global-mode 1)

;; Integrate YASnippet with IDO
(defun jazzy/yasnipet-ido-expand ()
  "Lets you select (and expand) a yasnippet key"
  (interactive)
    (let ((original-point (point)))
      (while (and
              (not (= (point) (point-min) ))
              (not
               (string-match "[[:space:]\n]" (char-to-string (char-before)))))
        (backward-word 1))
    (let* ((init-word (point))
           (word (buffer-substring init-word original-point))
           (list (yas-active-keys)))
      (goto-char original-point)
      (let ((key (remove-if-not
                  (lambda (s) (string-match (concat "^" word) s)) list)))
        (if (= (length key) 1)
            (setq key (pop key))
          (setq key (ido-completing-read "key: " list nil nil word)))
        (delete-char (- init-word original-point))
        (insert key)
        (yas-expand)))))

(define-key yas-minor-mode-map (kbd "<C-tab>") 'jazzy/yasnipet-ido-expand)

;;;; flycheck/flycheck-irony
;; http://www.flycheck.org/
;; https://github.com/Sarcasm/flycheck-irony
(use-package flycheck
  :ensure t
  :defer t
  :init
  (add-hook 'c++-mode-hook 'flycheck-mode)
  (add-hook 'c-mode-hook 'flycheck-mode)
  :config
  (use-package flycheck-irony :ensure t :defer t)
  (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;;;; flycheck-tip
;; https://github.com/yuutayamada/flycheck-tip
(use-package flycheck-tip
  :ensure t
  :config
  (flycheck-tip-use-timer 'verbose))

;;;;; eldoc-mode
;; https://github.com/ikirill/irony-eldoc
(use-package irony-eldoc
  :ensure t
  :defer t
  :config
  (add-hook 'irony-mode-hook 'irony-eldoc))

;;;; SrSpeedbar
(use-package sr-speedbar
  :ensure t
  :config
  (setq speedbar-use-images nil
	sr-speedbar-right-side nil
	sr-speedbar-width-x 8
	sr-speedbar-width-console 8
	sr-speedbar-max-width 8
	sr-speedbar-skip-other-window-p t
	sr-speedbar-delete-windows t))

;;;; (optional) adds CC special commands to `company-begin-commands' in order to
;;;; trigger completion at interesting places, such as after scope operator
;;;;     std::|
;;(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)


;;;;; ==========================================
;;;;; (optional) bind TAB for indent-or-complete
;;;;; ==========================================
;;;(defun irony--check-expansion ()
;;;(save-excursion
;;;  (if (looking-at "\\_>") t
;;;    (backward-char 1)
;;;    (if (looking-at "\\.") t
;;;      (backward-char 1)
;;;      (if (looking-at "->") t nil)))))
;;;(defun irony--indent-or-complete ()
;;;"Indent or Complete"
;;;(interactive)
;;;(cond ((and (not (use-region-p))
;;;            (irony--check-expansion))
;;;       (message "complete")
;;;       (company-complete-common))
;;;      (t
;;;       (message "indent")
;;;       (call-interactively 'c-indent-line-or-region))))
;;;(defun irony-mode-keys ()
;;;"Modify keymaps used by `irony-mode'."
;;;(local-set-key (kbd "TAB") 'irony--indent-or-complete)
;;;(local-set-key [tab] 'irony--indent-or-complete))
;;;(add-hook 'c-mode-common-hook 'irony-mode-keys)


;;;;;; Python


;;;; Jedi (python autocompletion)
;; TODO change to use the use-package
;; TODO add doc
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:server-args
      '("--sys-path" "/usr/lib/python3/dist-packages"
        "--sys-path" "/usr/local/lib/python3.4/dist-packages"))
(setq jedi:complete-on-dot t)


;;;;;; Revision Control


;;;; magit
;; https://github.com/magit/magit
;; http://daemianmack.com/magit-cheatsheet.html
(use-package magit
  :ensure t
  :config
  (setq magit-completing-read-function 'magit-ido-completing-read)
  :bind ("<f9>" . magit-status))


;;;;;;; Editing


(use-package highlight-symbol
  :ensure t
  :config
  (set-face-attribute 'highlight-symbol-face nil
		      :background "default")
  (setq highlight-symbol-idle-delay 0)
  (setq highlight-symbol-on-navigation-p t)
  (add-hook 'prog-mode-hook #'highlight-symbol-mode)
  (add-hook 'prog-mode-hook #'highlight-symbol-nav-mode))


;;;;;;; Networking



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
(use-package tramp
  :config
  (setq tramp-remote-path '(tramp-default-remote-path "/bin" "/usr/bin" "/sbin" "/usr/sbin")
  ;; play nice with .ssh/config
  tramp-ssh-controlmaster-options ""))

;;;; * auto-decompress files
(auto-compression-mode 1)

;;;; * auto encrypt/decrypt gpg files
(require 'epa-file)
(epa-file-enable)
