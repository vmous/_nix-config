;;;;; System Conditionals
(defvar macosx-p (string= system-name "f45c89ad1c47.ant.amazon.com"))
(defvar linux-p (string= system-name "ifrit"))
(defvar amazonbox-p (string= system-name "search-dev-relevance-vmous-64004.pdx4.amazon.com"))


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


;;;; customize
;; http://ergoemacs.org/emacs/emacs_custom_system.html
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-safe-themes
   (quote
    ("40f6a7af0dfad67c0d4df2a1dd86175436d79fc69ea61614d668a635c2cd94ab" "ff02e8e37c9cfd192d6a0cb29054777f5254c17b1bf42023ba52b65e4307b76a" default)))
 '(display-time-mode t)
 '(eclim-eclipse-dirs (quote (jazzy/eclim-eclipse-dirs)))
 '(eclim-executable jazzy/eclim-executable)
 '(eclimd-default-workspace jazzy/eclimd-default-workspace)
 '(eclimd-executable jazzy/eclimd-executable)
 '(package-selected-packages
   (quote
    (highlight-symbol magit flycheck-tip irony-eldoc flycheck-irony flycheck company-irony-c-headers company-gtags company-irony company ggtags yasnippet sr-speedbar zenburn-theme which-key use-package smex ido-vertical-mode ido-ubiquitous flx-ido auto-complete))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


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
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
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


;;;;;; Editing


;;;; highlight-symbol
;; https://github.com/nschum/highlight-symbol.el
(use-package highlight-symbol
  :ensure t
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
;; Bellow the ispell probram is hunspell. You need to install it on your system:
;; MacOSX: brew install hunspell
;; Debian: sudo apt-get install hunspell
;;
;; Note: you might need to install the respective dictionaries as well.
(use-package flyspell
  :diminish " 🔡" ;; 🐝
  :init
  (add-hook 'org-mode-hook 'flyspell-mode)
  ;; this enables flyspell for prog-modes
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  :config
  (setq-default ispell-program-name "hunspell"))

;;;; flyspell-correct-popup
;; https://github.com/xuchunyang/flyspell-popup
(use-package flyspell-correct-popup
  :ensure t
  :bind (:map flyspell-mode-map
              ("C-;" . flyspell-correct-word-generic)))


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


;;;; magit
;; https://github.com/magit/magit
;; http://daemianmack.com/magit-cheatsheet.html
(use-package magit
  :ensure t
  :config
  (setq magit-completing-read-function 'magit-ido-completing-read)
  :bind ("<f9>" . magit-status))


;;;; company-mode
;; https://github.com/company-mode/company-mode
(use-package company
  :ensure t
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-idle-delay                nil
        company-minimum-prefix-length     2
        company-tooltip-align-annotations t
        company-tooltip-flip-when-above   t
        ;; Easy navigation to candidates with M-<n>
        company-show-numbers              t
        company-tooltip-limit             20
        company-dabbrev-downcase          nil
        company-backends                  '((company-semantic company-irony-c-headers company-irony company-gtags company-emacs-eclim)))
  ;; https://github.com/emacsmirror/diminish
  :diminish company-mode
  :bind ("C-;" . company-complete-common))


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

;;;; flycheck-tip
;; https://github.com/yuutayamada/flycheck-tip
(use-package flycheck-tip
  :ensure t
  :config
  (flycheck-tip-use-timer 'verbose))


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

(when amazonbox-p
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


(defvar jazzy/eclim-eclipse-dirs)
(defvar jazzy/eclim-executable)
(defvar jazzy/eclimd-executable)
(defvar jazzy/eclimd-default-workspace)


(defvar jazzy/JAVA_HOME)
(defvar jazzy/JAVA)


(when macosx-p
  (setq jazzy/eclim-eclipse-dirs "/Users/vmous/Applications/Eclipse.app/Contents/Eclipse")
  (setq jazzy/eclim-executable "/Users/vmous/Applications/Eclipse.app/Contents/Eclipse/eclim")
  (setq jazzy/eclimd-executable "/Users/vmous/Applications/Eclipse.app/Contents/Eclipse/eclimd")
  (setq jazzy/eclimd-default-workspace "/Users/vmous/Workspace/eclipsews")

  (setq jazzy/JAVA_HOME "/Library/Java/JavaVirtualMachines/jdk1.8.0_91.jdk/Contents/Home")
  (setq jazzy/JAVA "/Library/Java/JavaVirtualMachines/jdk1.8.0_91.jdk/Contents/Home/bin/java"))


(when linux-p
  (setq jazzy/eclim-eclipse-dirs "")
  (setq jazzy/eclim-executable "")
  (setq jazzy/eclimd-executable "")
  (setq jazzy/eclimd-default-workspace "")

  (setq jazzy/JAVA_HOME "")
  (setq jazzy/JAVA ""))


(when amazonbox-p
  (setq jazzy/eclim-eclipse-dirs "")
  (setq jazzy/eclim-executable "")
  (setq jazzy/eclimd-executable "")
  (setq jazzy/eclimd-default-workspace "")

  (setq jazzy/JAVA_HOME "/apollo/env/JavaSE8/jdk1.8")
  (setq jazzy/JAVA "/apollo/env/JavaSE8/jdk1.8/bin/java"))


(setenv "JAVA_HOME" jazzy/JAVA_HOME)
(setenv "JAVA" jazzy/JAVA)


;;;; eclim
;; http://eclim.org/
;;
;; Install eclim as described at http://eclim.org/install.html
;;
;; Install eclipse installer
;; $ brew cask install eclipse-java
;; Configure Eclipse root: /User/$USER/Applications/Eclipse.app (Mac) or /opt/eclipse (Linux)
;; Configure Eclipse workspace: /User/$USER/Workspace/eclipsews (Mac) or /home/$USER/Workspace/eclipsews (Linux)
;;
;; Download official eclim installer
;; $ java -Dvim.skip=true -Declipse.home=/Users/$USER/Applications/Eclipse.app/Contents/Eclipse -jar eclim_X.X.X.jar install
;;
;; Start the eclim daemon
;; $ $ECLIPSE_HOME/eclimd
;;
;; Install headless Eclipse
(use-package emacs-eclim
  :ensure t
  :commands
  (eclim-mode global-eclim-mode)
  :init
  ;; Eclipse installation
  ;; Mac
  (custom-set-variables
   '(eclim-eclipse-dirs '(jazzy/eclim-eclipse-dirs))
   '(eclim-executable jazzy/eclim-executable)
   '(eclimd-executable jazzy/eclimd-executable)
   '(eclimd-default-workspace jazzy/eclimd-default-workspace))
  (require 'eclimd)
  ;; add eclim hook
  (add-hook 'java-mode-hook 'eclim-mode)
  :config
  (setq help-at-pt-display-when-idle t)
  (setq help-at-pt-timer-delay 0.1)
  (help-at-pt-set-timer)
  :bind
  ("C-c C-e p o" . eclim-project-open)
  ("C-c C-e p m" . eclim-project-manage))


;;;;;; Scala


;;;; ENhanced Scala Interaction Mode for Emacs
;; https://github.com/ensime
;; http://ensime.github.io/
;;
;; Add the following three lines in your global sbt,
;; e.g., ~/.sbt/0.13/plugins/plugins.sbt
;;
;; 1 resolvers += Resolver.sonatypeRepo("snapshots")
;; 2
;; 3 addSbtPlugin("org.ensime" % "ensime-sbt" % "0.1.5-SNAPSHOT")
;;
;; Then, in you project directory run:
;; $ sbt gen-ensime
;;
;; Lastly, open a Scala buffer and use ```M-x ensime``` to start a connection.
(use-package ensime
  :ensure t
  :commands ensime ensime-mode
  :init
  (add-hook 'scala-mode-hook 'ensime-mode))


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
