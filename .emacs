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
(set-frame-font "Source Code Pro-12")
(column-number-mode t)
(delete-selection-mode t)
(display-time)

;;;; Backup and auto-save
;; ftp://ftp.gnu.org/old-gnu/Manuals/elisp-manual-20-2.5/html_chapter/elisp_26.html
(setq
 ;; Backup
 ;; https://www.emacswiki.org/emacs/BackupFiles
 ;; TODO: Disable backups for su and sudo methods
 ;; https://www.gnu.org/software/emacs/manual/html_node/tramp/Auto_002dsave-and-Backup.html
 ;;
 ;; don't clobber symlinks
 backup-by-copying t
 ;; backup even version controlled files
 vc-make-backup-files t
 ;; don't litter my FS tree
 backup-directory-alist `(("." . ,(file-name-as-directory jazzy/env/backup-dir)))
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 ;; use versioned backups
 version-control t
 ;; Auto-save
 ;; https://www.emacswiki.org/emacs/AutoSave
 ;; NOTE: If I want to disable auto-save for files opened as roo check:
 ;; https://emacs.stackexchange.com/questions/14846/how-to-disable-auto-save-in-emacs-only-for-files-opened-as-root
 auto-save-file-name-transforms `((".*" ,(file-name-as-directory jazzy/env/backup-dir) t))
 ; auto-save every 50 chars. 20 is the minimum.
 auto-save-interval 50)

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
;; split-window
;; Change to new window after split
;; Matching tmux: https://github.com/tmux-plugins/tmux-pain-control
(global-set-key (kbd "C-x -") (lambda () (interactive)(split-window-vertically) (other-window 1)))
(global-set-key (kbd "C-x |") (lambda () (interactive)(split-window-horizontally) (other-window 1)))
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
)


;;;; package
;; http://wikemacs.org/wiki/Package.el
;; http://www.emacswiki.org/emacs/ELPA
(setq load-prefer-newer t)

(require 'url-handlers) ;; TODO: This line is a workaround to fix a bug. Remove at some point!
(require 'package)
(setq
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("melpa" . "http://melpa.org/packages/")
                    ("melpa-stable" . "http://stable.melpa.org/packages/"))
 package-archive-priorities '(("gnu" . 100)
                              ("melpa-stable" . 10)
                              ("melpa" . 1)))

(package-initialize)


;;;; use-package
;; https://github.com/jwiegley/use-package
;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(use-package diminish
  :ensure t)
(use-package bind-key
  :ensure t)


;;;; themes
;; https://pawelbx.github.io/emacs-theme-gallery/
;;
;; Note: In order to better distinguish between the default (local) and other
;; (remote) instances of Emacs, use a theme only for your primary machine.
(when (string= system-name jazzy/secrets/system-name/primary-system)
  (use-package zenburn-theme
    :ensure t
    :config
    ; use variable-pitch fonts for some headings and titles
    (setq zenburn-use-variable-pitch t)
    ;; scale headings in org-mode
    (setq zenburn-scale-org-headlines t)
    ;; scale headings in outline-mode
    (setq zenburn-scale-outline-headlines t))
  (load-theme 'zenburn t))
(when (string= system-name jazzy/secrets/system-name/amznlinux)
  (load-theme 'tsdh-dark t))

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

;;;;;;;; Core

(load "~/.emacs.d/core.el")

;;;;;;;; Text

(load "~/.emacs.d/txt.el")

;;;;;; org-mode

(load "~/.emacs.d/org.el")

;;;;;; Markdown

(load "~/.emacs.d/markdown.el")

;;;;;; LaTeX

;;(load "~/.emacs.d/latex.el")

;;;;;; pdf

(load "~/.emacs.d/pdf.el")

;;;;;;;; Development

(load "~/.emacs.d/prog.el")

;;;;;; Emacs Lisp

(load "~/.emacs.d/elisp.el")

;;;;;; Python

(load "~/.emacs.d/python.el")

;;;;;; Java (-script)

;;(load "~/.emacs.d/java.el")

;;;;;; Scala

;;(load "~/.emacs.d/scala.el")

;;;;;; C/C++/Obj-C

;;(load "~/.emacs.d/c.el")

;;;;;; HTML/XML

(load "~/.emacs.d/html.el")

;;;;;;;; System

(load "~/.emacs.d/sys.el")

;;;;;;;; Misc

(load "~/.emacs.d/misc.el")
