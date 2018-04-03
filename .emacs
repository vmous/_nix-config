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


(load "~/.emacs.d/core.el")


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




;;;; popup-imenu
;; https://github.com/ancane/popup-imenu
(use-package popup-imenu
  :ensure t
  :commands popup-imenu
  :bind ("M-i" . popup-imenu))





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

;;;;;; Markdown

(load "~/.emacs.d/markdown.el")

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

;;;;;;;; Development

(load "~/.emacs.d/prog.el")

;;;;;; Emacs Lisp

(load "~/.emacs.d/elisp.el")

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

;;;;;; HTML/XML

(load "~/.emacs.d/html.el")

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
