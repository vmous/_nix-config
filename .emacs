;;;;; System Conditionals
(defvar macosx-p (string= system-name "f45c89ad1c47.ant.amazon.com"))
(defvar linux-p (string= system-name "ifrit"))
(defvar amznlinux-p (string= system-name "dev-dsk-vmous-1c-f558cb86.eu-west-1.amazon.com"))

;;;;; Load secrets
;; I keep sensitive information separately so that I can publish my main config.
(load "~/.emacs.d/secrets")


;;;;;; General Settings


;;(setq initial-scratch-message ";;
;;;; I'm sorry, Emacs failed to start correctly.
;;;; Hopefully the issue will be simple to resolve.
;;;;
;;;;                _.-^^---....,,--
;;;;            _--                  --_
;;;;           <          SONIC         >)
;;;;           |       BOOOOOOOOM!       |
;;;;            \._                   _./
;;;;               ```--. . , ; .--'''
;;;;                     | |   |
;;;;                  .-=||  | |=-.
;;;;                  `-=#$%&%$#=-'
;;;;                     | ;  :|
;;;;            _____.,-#%&$@%#&#~,._____
;;")


;;;; Font: Adobe Source Code Pro
;; https://github.com/adobe-fonts/source-code-pro
;;
;; Note: You need to install the fonts on your system in order for this
;; to take effect.
(set-default-font "Source Code Pro-12")
(column-number-mode t)
(delete-selection-mode t)
(display-time)
;; backup files
;; https://www.emacswiki.org/emacs/BackupDirectory
(setq
 ;; don't clobber symlinks
 backup-by-copying t
 ;; don't litter my fs tree
 backup-directory-alist '(("." . "~/.emacs.d/backup"))
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 ;; use versioned backups
 version-control t)
;;smooth scrolling
(setq scroll-margin 5
      scroll-conservatively 9999
      scroll-step 1)


;;;; General Key Bindings
(global-set-key (kbd "C-x c") 'customize)
(global-set-key (kbd "C-x r p") 'string-insert-rectangle)
;; windmove
(global-set-key (kbd "C-c <left>") 'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>") 'windmove-up)
(global-set-key (kbd "C-c <down>") 'windmove-down)
;; sr-speedbar
;;(global-set-key (kbd "<f1>") 'sr-speedbar-toggle)
;; window resize
(global-set-key (kbd "C-s-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-s-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-s-<down>") 'shrink-window)
(global-set-key (kbd "C-s-<up>") 'enlarge-window)
;; quick notes
(global-set-key (kbd "C-c c") 'org-capture)

(defun jazzy/delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (delete-region
   (point)
   (progn
     (forward-word arg)
     (point))))
(global-set-key (kbd "M-d") 'jazzy/delete-word)

(defun jazzy/backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (my-delete-word (- arg)))
(global-set-key (kbd "<M-backspace>") 'jazzy/backward-delete-word)

;; Commenting out the delete-line function.
;; Pros: Does not add the deleted text to `kill-ring'
;; Cons: I don't like removing the line and its '\n' char in one go
;;
;;(defun jazzy/delete-line ()
;;  "Delete text from current position to end of line char.
;;This command does not push text to `kill-ring'."
;;  (interactive)
;;  (delete-region
;;   (point)
;;   (progn (end-of-line 1) (point)))
;;  (delete-char 1))
;;(global-set-key (kbd "C-k") 'jazzy/delete-line)

(defun jazzy/delete-line-backward ()
  "Delete text between the beginning of the line to the cursor position.
This command does not push text to `kill-ring'."
  (interactive)
  (let (p1 p2)
    (setq p1 (point))
    (beginning-of-line 1)
    (setq p2 (point))
    (delete-region p1 p2)))
(global-set-key (kbd "C-S-k") 'jazzy/delete-line-backward) ;; Ctrl+Shift+k

(defun jazzy/move-this-buffer-and-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
               (error "A buffer named '%s' already exists!" new-name))
	      (t
	       (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)
               (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name))))))))
(global-set-key (kbd "C-c m") 'jazzy/move-this-buffer-and-file)


;;;; customize
;; http://ergoemacs.org/emacs/emacs_custom_system.html
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("4e753673a37c71b07e3026be75dc6af3efbac5ce335f3707b7d6a110ecb636a3" "afbb40954f67924d3153f27b6d3399df221b2050f2a72eb2cfa8d29ca783c5a8" default)))
 '(ensime-goto-test-config-defaults
   (quote
    (:test-class-names-fn ensime-goto-test--test-class-names :test-class-suffixes
			  ("Suite" "Spec" "Test" "Check" "Specification")
			  :impl-class-name-fn ensime-goto-test--impl-class-name :impl-to-test-dir-fn ensime-goto-test--impl-to-test-dir :is-test-dir-fn ensime-goto-test--is-test-dir :test-template-fn ensime-goto-test--test-template-scalatest-funsuite)))
 '(package-selected-packages
   (quote
    (org-mode json-mode minimap sublimity ensime markdown-preview-eww markdown-preview-mode markdown-mode+ jedi java-snippets projectile markdown-mode visual-regexp flyspell-popup smartparens ido-grid-mode popup-imenu window-numbering scala-mode ido-occur impatient-mode flycheck-pos-tip highlight-symbol magit flycheck-tip irony-eldoc flycheck-irony flycheck company-irony-c-headers company-gtags company-irony company ggtags yasnippet sr-speedbar zenburn-theme which-key use-package smex ido-vertical-mode ido-ubiquitous flx-ido auto-complete))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(window-numbering-face ((t (:foreground "DeepPink" :underline "DeepPink" :weight bold))) t))

(when macosx-p
  ;; Fixing exec-path discrepancy between shell and Max OSX Finder launch 
  (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
  (setq exec-path (append exec-path '("/usr/local/bin"))))


;;;; package
;; http://wikemacs.org/wiki/Package.el
;; http://www.emacswiki.org/emacs/ELPA
(setq load-prefer-newer t)

(require 'url-handlers) ;; TODO: This line is a workaround to fix a bug. Remove at some point!
(require 'package)
(setq
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("org" . "http://orgmode.org/elpa/")
                    ("melpa" . "http://melpa.org/packages/")
		    ("mepla-latex-preview-pane" . "http://melpa.milkbox.net/packages/")
                    ("melpa-stable" . "http://stable.melpa.org/packages/"))
 package-archive-priorities '(("melpa-stable" . 1)))

(package-initialize)


;;;; use-package
;; https://github.com/jwiegley/use-package
;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;;;; themes
;;(use-package solarized-theme
;;  :ensure t)
;;(load-theme 'solarized-dark)
(use-package zenburn-theme
  :ensure t)
(load-theme 'zenburn t)


;;;; time
;;
(use-package time
  :bind (("C-c t" . display-time-world))
  :config
  (setq display-time-world-time-format "%H:%M %Z, %d. %b"
        display-time-world-list '(("Europe/Berlin"    "Berlin")
                                  ("Europe/London"    "London")
                                  ("Europe/Istanbul"  "Istanbul")
                                  ("America/Winnipeg" "Winnipeg (CA)")
                                  ("America/New_York" "New York (USA)")
                                  ("Asia/Tokyo"       "Tokyo (JP)"))))


;;;; desktop
;; https://www.emacswiki.org/emacs/Desktop
(use-package desktop
  :ensure t
  :config
  (setq desktop-auto-save-timeout 3600
	desktop-file-not-to-save nil
	desktop-buffers-not-to-save "^$")
  (desktop-save-mode)
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


;;;; expand-region
;; https://github.com/magnars/expand-region.el
(use-package expand-region
  :ensure t
  :bind (("M-r" . er/expand-region)))


(use-package shell
  :ensure t
  :config
  (define-key comint-mode-map (kbd "<up>") 'comint-previous-input)
  (define-key comint-mode-map (kbd "<down>") 'comint-next-input))


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


;;;; popup-imenu
;; https://github.com/ancane/popup-imenu
(use-package popup-imenu
  :ensure t
  :commands popup-imenu
  :bind ("M-i" . popup-imenu))


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
  :bind ("M-x" . smex))

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


;;;;;; Editing


;;;; org-mode
;; https://orgmode.org/
(use-package org
  :ensure t
  :init
  (setq visual-line-mode t
        adaptive-wrap-prefix-mode t
        org-completion-use-ido t
        org-export-coding-system 'utf-8
        org-directory "~/.emacs.d/org"
        org-default-notes-file (concat org-directory "/scratch-pad.org"))
  (defconst jazzy/org/journal (concat org-directory "/journal.org"))
  (require 'org-capture)
  (setq org-capture-templates
        ;; https://orgmode.org/manual/Capture-templates.html
        '(("a" "Appointment" entry (file jazzy/org/gcal/primary) "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")
          ("j" "Journal" entry (file+datetree jazzy/org/journal) "* %?\nEntered on %U\n  %i\n  %a")
          ("l" "Link" entry (file+headline org-default-notes-file "Links") "* %? %^L %^g \n%T" :prepend t)
          ("n" "Note" entry (file+headline org-default-notes-file "Notes") "* %?\n%u" :prepend t)
          ("t" "To Do Item" entry (file+headline org-default-notes-file "To Dos") "* TODO %?\n%u" :prepend t)))
  (add-hook 'org-mode-hook (lambda () (org-indent-mode t)))
  :bind(("C-c a" . org-agenda)))

;;;; org-bullets
;; https://github.com/sabof/org-bullets
(use-package org-bullets
  :ensure t
  :init
  ;; make available "org-bullet-face" such that I can control the font size individually
  (setq org-bullets-face-name (quote org-bullet-face))
;  (setq org-bullets-bullet-list
;        '("✡" "⎈" "✽" "✲" "✱" "✻" "✼" "✽" "✾" "✿" "❀" "❁" "❂" "❃" "❄" "❅" "❆" "❇") ; hexagrams
;        '("○" "☉" "◎" "◉" "○" "◌" "◎" "●" "◦" "◯" "⚪" "⚫" "⚬" "❍" "￮" "⊙" "⊚" "⊛" "∙" "∘") ; circles
;        '("◐" "◑" "◒" "◓" "◴" "◵" "◶" "◷" "⚆" "⚇" "⚈" "⚉" "♁" "⊖" "⊗" "⊘") ; special circles
;        '("✙" "♱" "♰" "☥" "✞" "✟" "✝" "†" "✠" "✚" "✜" "✛" "✢" "✣" "✤" "✥") ; crosses
;        '("♠" "♣" "♥" "♦" "♤" "♧" "♡" "♢") ; pocker symbols
;        '("☯" "☰" "☱" "☲" "☳" "☴" "☵" "☶" "☷") ; yinyang
;        '("☀" "♼" "☼" "☾" "☽" "☣" "§" "¶" "‡" "※" "✕" "△" "◇" "▶" "◀" "◈") ; special symbols 
;  )
  ;; Collection of org-ellipsis candidate symbols
  ;; * right arrows
  ;;     "↝" "⇉" "⇝" "⇢" "⇨" "⇰" "➔" "➙" "➛" "➜" "➝" "➞"
  ;;     "➟" "➠" "➡" "➥" "➦" "➧" "➨"
  ;;     "➩" "➪" "➮" "➯" "➱" "➲"
  ;;     "➳" "➵" "➸" "➺" "➻" "➼" "➽"
  ;; * arrow heads
  ;;     "➢" "➣" "➤" "≪", "≫", "«", "»"
  ;; * other arrows
  ;;     "↞" "↠" "↟" "↡" "↺" "↻"
  ;; * lightening
  ;;     "⚡"
  ;; * other symbols
  ;;     "…" "▼" "↴" "∞" "⬎" "⤷" "⤵"
  (setq org-ellipsis " ⤵")
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode t))))

;;;; org-gcal
;; https://github.com/myuhe/org-gcal.el
;;
;; Note: If you get an error simmilar to
;; "Error (use-package): Failed to install org-gcal: http://orgmode.org/elpa/org-20171204.tar: Moved permanently"
;; then try to install org-gcal via the package manager.
(use-package org-gcal
  :ensure t
  :init
  (add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync)))
  (add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync)))
  (defconst jazzy/org/gcal/primary (concat org-directory "/gcal-primary.org"))
  (setq org-gcal-client-id jazzy/secrets/org/gcal/client-id
        org-gcal-client-secret jazzy/secrets/org/gcal/client-secret
        org-gcal-file-alist `(
          (,jazzy/secrets/org/gcal/calendar-primary . ,jazzy/org/gcal/primary)))
  (setq org-agenda-files (list jazzy/org/gcal/primary)))

;;;; calfw
;; https://github.com/kiwanami/emacs-calfw
(use-package calfw-org
  :ensure t)
(use-package calfw-ical
  :ensure t)
(use-package calfw-gcal
  :ensure t)
(use-package calfw
  :ensure t
  :init
  (setq cfw:org-overwrite-default-keybinding t)
  (defun jazzy/calendar ()
    (interactive)
    (cfw:open-calendar-buffer
     :contents-sources
     (list
      (cfw:ical-create-source "primary" jazzy/secrets/org/gcal/url-primary "IndianRed")
      (cfw:ical-create-source "de-holidays" jazzy/secrets/org/gcal/url-de-holidays "Green")
      (cfw:ical-create-source "gr-holidays" jazzy/secrets/org/gcal/url-gr-holidays "Blue")
      (cfw:ical-create-source "gr-names" jazzy/secrets/org/gcal/url-gr-names "Yellow")))))

;;;;;; AUCTex/LaTeX
(defvar jazzy/latex/bin "")

(when macosx-p
  (setq jazzy/latex/bin "/Library/TeX/texbin"))

;;;; AUCTeX
;; https://www.gnu.org/software/auctex/
(use-package tex-site
  :ensure auctex
  :init
  (setenv "PATH" (concat jazzy/latex/bin ":" (getenv "PATH")))
  (add-to-list 'exec-path jazzy/latex/bin)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (setq LaTeX-math-menu-unicode t)
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  ;; https://www.gnu.org/software/auctex/reftex.html
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq reftex-cite-format 'natbib)
  (setq reftex-plug-into-AUCTeX t)
;  ;; https://superuser.com/questions/142450/getting-emacs-to-use-pdflatex
;  (setq latex-run-command "pdflatex")
  (setq TeX-PDF-mode t))

;;;; latex-preview-pane
;; https://www.emacswiki.org/emacs/LaTeXPreviewPane
;;
;; Commands
;; * Refresh Preview (bound to M-p)
;; * Open in External Program (Bound to M-P)
(use-package latex-preview-pane
  :ensure t
  :pin mepla-latex-preview-pane
  :init
  (latex-preview-pane-enable))

;;;; code/smartparens
;; http://emacsredux.com/blog/2013/11/01/highlight-matching-delimiters-with-smartparens/
(use-package smartparens-config
  :ensure smartparens
  ;; :diminish -mode "()")
  :config
  (smartparens-global-mode +1)
  (show-smartparens-global-mode +1)
  (sp-with-modes '(html-mode sgml-mode)
    (sp-local-pair "<" ">")))

;;;; highlight-symbol
;; https://github.com/nschum/highlight-symbol.el
(use-package highlight-symbol
  :ensure t
  :diminish highlight-symbol-mode
  :config
  (set-face-attribute 'highlight-symbol-face nil
		      :background "default")
  (setq highlight-symbol-idle-delay 0)
  (setq highlight-symbol-on-navigation-p t)
  (add-hook 'prog-mode-hook #'highlight-symbol-mode)
  (add-hook 'prog-mode-hook #'highlight-symbol-nav-mode))


;;;; flyspell
;; https://www.emacswiki.org/emacs/FlySpell
;;
;; Bellow the ispell probram is hunspell (http://hunspell.github.io/).
;; You need to install it on your system:
;; - MacOSX: brew install hunspell
;; - Debian: sudo apt-get install hunspell
;;
;; Check the following:
;; $ hunspell -D
;; If you don't find the dictionary you want in the output then do it manually.
;; Go to LibreOffice repo: https://cgit.freedesktop.org/libreoffice/dictionaries/tree
;; and wget *.aff *.dic into location a designated location in your machine.
;; The above command (hunspell -D) will tell you the default locations it searches.
;; e.g. Mac:    ~/Library/Spelling/
;;      AL2002: ~/.openoffice.org2/user/wordbook/
;;
(use-package flyspell
;;  :diminish " 🔡" ;; 🐝
  :init
  (add-hook 'org-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-buffer)
  :config
  (setq-default ispell-program-name "hunspell")
  (setq ispell-dictionary "en_US")
  (setq ispell-dictionary-alist
	;; Please note the list `("-d" "en_US")` contains ACTUAL parameters passed to hunspell
	;; You could use `("-d" "en_US,en_US-med")` to check with multiple dictionaries
	'(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)
;;	  ("el_GR" "[a-zA-Z``A_long_list_of_octal_characters'']" "[^[:alpha:]]" "[']" nil ("-d" "el_GR") nil utf-8)
	  ))
  )

;;;; flyspell-popup
;; https://github.com/xuchunyang/flyspell-popup
(use-package flyspell-popup
  :ensure t
  :config
  (add-hook 'flyspell-mode-hook #'flyspell-popup-auto-correct-mode)
  :bind (:map flyspell-mode-map
              ("C-;" . flyspell-popup-correct)))


(use-package visual-regexp
  :ensure t
  :bind (("C-c r" . vr/replace)
         ("C-c q" . vr/query-replace)))


;;;; markdown-mode
;; http://jblevins.org/projects/markdown-mode/ 
;; For the preview required to install
;;  - Ubuntu: sudo apt-get install libtext-multimarkdown-perl
;;  - MacOSX: brew install multimarkdown
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
  :init (setq markdown-command "multimarkdown")
  :bind ("<f5>" . markdown-live-preview-mode))


;;;;;;;; Developement

;;;; TABS
;; https://www.emacswiki.org/emacs/NoTabs
;; https://www.emacswiki.org/emacs/TabsAreEvil
;; https://www.emacswiki.org/emacs/IndentationBasics
;; https://www.masteringemacs.org/article/converting-tabs-whitespace
;;
;;;; I prefer NoTabs, but sometimes I work on a project that does use tab indentation. I don’t want to cause problems for these source files. As a result, I use the following snippet to default to no tabs, but to use tabs if that’s what a pre-existing file is primarily using for indentation:
;;(defun how-many-region (begin end regexp &optional interactive)
;;  "Print number of non-trivial matches for REGEXP in region.
;;Non-interactive arguments are Begin End Regexp"
;;  (interactive "r\nsHow many matches for (regexp): \np")
;;  (let ((count 0) opoint)
;;    (save-excursion
;;      (setq end (or end (point-max)))
;;      (goto-char (or begin (point)))
;;      (while (and (< (setq opoint (point)) end)
;;                  (re-search-forward regexp end t))
;;        (if (= opoint (point))
;;            (forward-char 1)
;;          (setq count (1+ count))))
;;      (if interactive (message "%d occurrences" count))
;;      count)))
;;
;;(defun infer-indentation-style ()
;;  ;; if our source file uses tabs, we use tabs, if spaces spaces, and if
;;  ;; neither, we use the current indent-tabs-mode
;;  (let ((space-count (how-many-region (point-min) (point-max) "^  "))
;;        (tab-count (how-many-region (point-min) (point-max) "^\t")))
;;    (if (> space-count tab-count) (setq indent-tabs-mode nil))
;;    (if (> tab-count space-count) (setq indent-tabs-mode t))))

;; Since indent-tabs-mode is a buffer-local we need to setq-default
(setq-default indent-tabs-mode nil)

;;;; magit
;; https://github.com/magit/magit
;; http://daemianmack.com/magit-cheatsheet.html
(use-package magit
  :ensure t
  :config
  (setq magit-completing-read-function 'magit-ido-completing-read)
  (add-hook 'git-commit-mode-hook 'turn-on-flyspell)
  :bind ("C-c g s" . magit-status))


;;;; company-mode
;; https://github.com/company-mode/company-mode
(use-package company
  ;; https://github.com/emacsmirror/diminish
  :diminish company-mode
  :ensure t
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-idle-delay                1
        company-minimum-prefix-length     1
        company-tooltip-align-annotations t
        company-tooltip-flip-when-above   t
        ;; Easy navigation to candidates with M-<n>
        company-show-numbers              t
        company-tooltip-limit             20
        company-dabbrev-downcase          nil))


;;;; yasnippet
;; https://github.com/capitaomorte/yasnippet
;; http://capitaomorte.github.io/yasnippet/
(use-package yasnippet :ensure t)
(use-package java-snippets :ensure t)
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


;;;; flycheck
;; http://www.flycheck.org/
;; https://github.com/Sarcasm/flycheck-irony
(use-package flycheck
  :ensure t
  :defer t
  :init
  (add-hook 'c++-mode-hook 'flycheck-mode)
  (add-hook 'c-mode-hook 'flycheck-mode)
  ;; flycheck-java
  ;; https://github.com/akorobov/flycheck-java
  ;; $ brew install ecj
  (add-hook 'java-mode-hook
          (lambda () (setq flycheck-java-ecj-jar-path "/usr/local/Cellar/ecj/4.9/share/java/ecj.jar")))
  :config
  (use-package flycheck-irony :ensure)
  (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;;;; flycheck-pos-tip
;; https://github.com/flycheck/flycheck-pos-tip
(use-package flycheck-pos-tip
  :ensure t
  :config
  (flycheck-pos-tip-mode))



;;;;;; EMACS Lisp
(add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)


;;;;;; Python


;; To find out what to put in this list, do
;; $ python3.5
;; >>> import site; site.getsitepackages()
;; ['x', 'y']
;;
;; Use 'x' and 'y'
(defvar jazzy/dist-packages)

(when macosx-p
  (setq jazzy/dist-packages
	'("--sys-path" "/usr/local/Cellar/python3/3.5.1/Frameworks/Python.framework/Versions/3.5/lib/python3.5/site-packages"
	  "--sys-path" "/Library/Python/3.5/site-packages")))

(when linux-p
  (setq jazzy/dist-packages
	'("--sys-path" "/usr/lib/python3/dist-packages"
	  "--sys-path" "/usr/local/lib/python3.4/dist-packages")))

(when amznlinux-p
  (setq jazzy/dist-packages
	'("--sys-path" "")))

;;;; Jedi
;; https://github.com/tkf/emacs-jedi
;;
;; The first time you need to install the Jedi server
;; M-x jedi:install-server
;;
;; In case it complains about virtualenv not being available, install it:
;; $ pip3 install virtualenv
(use-package jedi
  :ensure t
  :init
  (autoload 'jedi:setup "jedi" nil t)
  (add-hook 'python-mode-hook 'jedi:setup)
  :config
  (setq jedi:server-args jazzy/dist-packages)
  (setq jedi:complete-on-dot t))


;;;;;; Java


;;(defvar jazzy/eclim-eclipse-dirs)
;;(defvar jazzy/eclim-executable)
;;(defvar jazzy/eclimd-executable)
;;(defvar jazzy/eclimd-default-workspace)


(defvar jazzy/JAVA_HOME)
(defvar jazzy/JAVA)


(when macosx-p
;;  (setq jazzy/eclim-eclipse-dirs "/Users/vmous/Applications/Eclipse.app/Contents/Eclipse")
;;  (setq jazzy/eclim-executable "/Users/vmous/Applications/Eclipse.app/Contents/Eclipse/eclim")
;;  (setq jazzy/eclimd-executable "/Users/vmous/Applications/Eclipse.app/Contents/Eclipse/eclimd")
;;  (setq jazzy/eclimd-default-workspace "/Users/vmous/Workspace/eclipsews")

  (setq jazzy/JAVA_HOME "/Library/Java/JavaVirtualMachines/jdk1.8.0_151.jdk/Contents/Home")
  (setq jazzy/JAVA "/Library/Java/JavaVirtualMachines/jdk1.8.0_151.jdk/Contents/Home/bin/java")
)


(when linux-p
;;  (setq jazzy/eclim-eclipse-dirs "")
;;  (setq jazzy/eclim-executable "")
;;  (setq jazzy/eclimd-executable "")
;;  (setq jazzy/eclimd-default-workspace "")

  (setq jazzy/JAVA_HOME "")
  (setq jazzy/JAVA "")
)


(when amznlinux-p
;;  (setq jazzy/eclim-eclipse-dirs "")
;;  (setq jazzy/eclim-executable "")
;;  (setq jazzy/eclimd-executable "")
;;  (setq jazzy/eclimd-default-workspace "")

  (setq jazzy/JAVA_HOME "/apollo/env/JavaSE8/jdk1.8")
  (setq jazzy/JAVA "/apollo/env/JavaSE8/jdk1.8/bin/java")
)


(setenv "JAVA_HOME" jazzy/JAVA_HOME)
(setenv "JAVA" jazzy/JAVA)


;;;;;; eclim
;;;; http://eclim.org/
;;;;
;;;; Install eclim as described at http://eclim.org/install.html
;;;;
;;;; Install eclipse installer
;;;; $ brew cask install eclipse-java
;;;; Configure Eclipse root: /User/$USER/Applications/Eclipse.app (Mac) or /opt/eclipse (Linux)
;;;; Configure Eclipse workspace: /User/$USER/Workspace/eclipsews (Mac) or /home/$USER/Workspace/eclipsews (Linux)
;;;;
;;;; Download official eclim installer
;;;; $ java -Dvim.skip=true -Declipse.home=/Users/$USER/Applications/Eclipse.app/Contents/Eclipse -jar eclim_X.X.X.jar install
;;;;
;;;; Start the eclim daemon
;;;; $ $ECLIPSE_HOME/eclimd
;;;;
;;;; Install headless Eclipse
;;(use-package emacs-eclim
;;  :ensure t
;;  :commands
;;  (eclim-mode global-eclim-mode)
;;  :init
;;  ;; Eclipse installation
;;  ;; Mac
;;  (custom-set-variables
;;   '(eclim-eclipse-dirs '(jazzy/eclim-eclipse-dirs))
;;   '(eclim-executable jazzy/eclim-executable)
;;   '(eclimd-executable jazzy/eclimd-executable)
;;   '(eclimd-default-workspace jazzy/eclimd-default-workspace))
;;  (require 'eclimd)
;;  ;; add eclim hook
;;  (add-hook 'java-mode-hook 'eclim-mode)
;;  :config
;;  (setq help-at-pt-display-when-idle t)
;;  (setq help-at-pt-timer-delay 0.1)
;;  (help-at-pt-set-timer)
;;  :bind
;;  ("C-c C-e p o" . eclim-project-open)
;;  ("C-c C-e p m" . eclim-project-manage))


;;;;;; Javascript

;;;; JSON mode
(use-package json-mode
  :ensure t
  :mode "\\.json"
  :init
  (add-hook 'json-mode-hook #'flycheck-mode)
  :config
  (setq json-reformat:indent-width 4)
  (setq js-indent-level 4))

;;;;;; Scala


;;;; Scala mode
;; https://github.com/ensime/emacs-scala-mode
(use-package scala-mode
  :ensure t
  :init
  (add-hook 'scala-mode-hook (lambda ()
			       (setq prettify-symbols-alist scala-prettify-symbols-alist)
			       (prettify-symbols-mode)))
  :interpreter
  ("scala" . scala-mode))


;;;; ENhanced Scala Interaction Mode for Emacs
;; https://github.com/ensime
;; http://ensime.github.io/
;;
;; Add the following three lines in your global sbt,
;; e.g., ~/.sbt/0.13/plugins/plugins.sbt
;;
;; ```sbt
;; 1 if (sys.props("java.version").startsWith("1.6"))
;; 2   addSbtPlugin("org.ensime" % "sbt-ensime" % "1.0.0")
;; 3 else
;; 4   addSbtPlugin("org.ensime" % "sbt-ensime" % "1.11.0")
;; ```
;;
;; Then, in you project directory run:
;; $ sbt ensimeConfig
;; $ sbt ensimeConfigProject
;;
;; Lastly, open a Scala buffer and use ```M-x ensime``` to start a connection.
(use-package ensime
  :ensure t
  :pin melpa-stable
  :after scala-mode
  :commands ensime ensime-mode
  :init
  (add-hook 'scala-mode-hook 'ensime-mode)
  (defun ensime-goto-test--test-template-scalatest-wordspec-2 ()
  "ENSIME template for ScalaCheck WordSpec style test."
  "package %TESTPACKAGE%

import org.scalatest.{ BeforeAndAfter, BeforeAndAfterAll, Matchers, WordSpec }

class %TESTCLASS% extends WordSpec with Matchers with BeforeAndAfter with BeforeAndAfterAll {

  override def beforeAll() = {
  }

  after {
  }

  \"\" should {
    \"\" in pending
  }

}
")
  (defun ensime-goto-test--test-template-scalatest-funsuite ()
  "ENSIME template for ScalaCheck FunSuite style test."
  "package %TESTPACKAGE%

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class %TESTCLASS% extends FunSuite {

  test(\"Testing\") {
    assert(true)
  }

}
")
  (custom-set-variables
   '(ensime-goto-test-config-defaults
     (quote (:test-class-names-fn ensime-goto-test--test-class-names
				  :test-class-suffixes ("Suite" "Spec" "Test" "Check" "Specification")
				  :impl-class-name-fn ensime-goto-test--impl-class-name
				  :impl-to-test-dir-fn ensime-goto-test--impl-to-test-dir
				  :is-test-dir-fn ensime-goto-test--is-test-dir
				  :test-template-fn ensime-goto-test--test-template-scalatest-funsuite))))
  :config
  ;; Disable message for tracking SNAPSHOT version of ensime-server
  (setq ensime-startup-snapshot-notification nil
	ensime-graphical-tooltips t
	ensime-auto-generate-config t
	ensime-completion-style 'company)
  (push '(ensime-company) company-backends)
  ;; Adding asterisk in newline for multiline comments.
  (defun lunaryorn-newline-and-indent-with-asterisk ()
    (interactive)
    (newline-and-indent)
    (scala-indent:insert-asterisk-on-multiline-comment))
  (define-key ensime-mode-map (kbd "C-c C-v m") 'ensime-import-type-at-point)
  (define-key ensime-mode-map (kbd "C-c C-v M") 'ensime-imported-type-path-at-point)
  (define-key scala-mode-map (kbd "RET")
    #'lunaryorn-newline-and-indent-with-asterisk)
  (define-key scala-mode-map (kbd "C-c C-v c") 'sbt-clear))


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

  (use-package company-irony :ensure t :defer t)
  (use-package company-irony-c-headers :ensure t :defer t)

;;;;; eldoc-mode
;; https://github.com/ikirill/irony-eldoc
(use-package irony-eldoc
  :ensure t
  :defer t
  :config
  (add-hook 'irony-mode-hook 'irony-eldoc))

;;;; SrSpeedbar
;;(use-package sr-speedbar
;;  :ensure t
;;  :config
;;  (setq speedbar-use-images nil
;;	sr-speedbar-right-side nil
;;	sr-speedbar-width-x 8
;;	sr-speedbar-width-console 8
;;	sr-speedbar-max-width 8
;;	sr-speedbar-skip-other-window-p t
;;	sr-speedbar-delete-windows t))

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


;;;; CC-mode
;;(add-hook 'nxml-mode-hook '(lambda ()
;;        (setq ac-sources (append '(ac-source-semantic) ac-sources))
;;	(local-set-key (kbd "RET") 'newline-and-indent)
;;	(linum-mode t)
;;	(semantic-mode t)
;;	(hs-minor-mode t)
;;	(local-set-key (kbd "C-h") 'hs-toggle-hiding)
;;))


(require 'hideshow)
(require 'sgml-mode)
(require 'nxml-mode)

(add-to-list 'hs-special-modes-alist
             '(nxml-mode
               "<!--\\|<[^/>]*[^/]>"
               "-->\\|</[^/>]*[^/]>"

               "<!--"
               sgml-skip-tag-forward
               nil))



(add-hook 'nxml-mode-hook 'hs-minor-mode)

;; optional key bindings, easier than hs defaults
(define-key nxml-mode-map (kbd "C-c h") 'hs-toggle-hiding)


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
;;
;; Note: You might need to reload the fs attributes for ido autocompletion to
;;       work with newly created files with TRAMP. To do so hit C-l.
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


;;;;;; HTML


;;;; impatient-mode
;; https://github.com/skeeto/impatient-mode
(use-package impatient-mode
  :ensure t)
