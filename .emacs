(load "~/.emacs.d/environment.el")

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
;; https://pawelbx.github.io/emacs-theme-gallery/
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


;;;;;; Editing

;;;; xah-math-input
;; https://github.com/xahlee/xah-math-input
;; http://ergoemacs.org/emacs/xmsi-math-symbols-input.html
(use-package xah-math-input
  :ensure t
  :diminish
  :init
  (global-xah-math-input-mode t))

;;;;;; org-mode

(load "~/.emacs.d/org.el")

;;;;;; LaTeX

(load "~/.emacs.d/latex.el")

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
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
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


;;;; csv-mode
;; https://www.emacswiki.org/emacs/CsvMode
(use-package csv-mode
  :ensure t
  :config
  (add-hook 'csv-mode-hook (lambda () (setq indent-tabs-mode t))))


;;;; pdf-tools
;; https://github.com/politza/pdf-tools
;;
;; http://babbagefiles.blogspot.de/2017/11/more-pdf-tools-tricks.html?m=1
(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install)
  ;; open pdfs scaled to fit page
  (setq-default pdf-view-display-size 'fit-page)
  ;; automatically annotate highlights
  (setq pdf-annot-activate-created-annotations t)
  ;; use normal isearch
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  ;; turn off cua so copy works
  (add-hook 'pdf-view-mode-hook (lambda () (cua-mode 0)))
  ;; more fine-grained zooming
  (setq pdf-view-resize-factor 1.1)
  ;; keyboard shortcuts
  (define-key pdf-view-mode-map (kbd "h") 'pdf-annot-add-highlight-markup-annotation)
  (define-key pdf-view-mode-map (kbd "t") 'pdf-annot-add-text-annotation)
  (define-key pdf-view-mode-map (kbd "D") 'pdf-annot-delete))


;;;;;;;; Developement


;;;; Projectile
;; https://github.com/bbatsov/projectile
;; https://projectile.readthedocs.io/en/latest/
;;
;; TODO:
;; - https://projectile.readthedocs.io/en/latest/configuration/#regenerate-tags (make also sure you do https://projectile.readthedocs.io/en/latest/configuration/#idle-timer)
;; - https://projectile.readthedocs.io/en/latest/configuration/#configure-a-projects-compilation-test-and-run-commands
(use-package projectile
  :ensure t
  :init (projectile-global-mode)
  :config
  (setq projectile-mode-line '(:eval (format " Prjl[%s]" (projectile-project-name)))
        projectile-enable-caching t)
  (projectile-discover-projects-in-directory jazzy/env/workspace))


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


;;;; company-quickhelp
;; https://github.com/expez/company-quickhelp
(use-package company-quickhelp
  :ensure t)

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

(load "~/.emacs.d/python.el")

;;;;;; Java

(load "~/.emacs.d/java.el")

;;;;;; Javascript

(load "~/.emacs.d/js.el")

;;;;;; Scala

(load "~/.emacs.d/scala.el")

;;;;;; C/C++/Obj-C

(load "~/.emacs.d/c.el")

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
