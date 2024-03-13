;;;;;;;; txt.el
;;;;
;; File defining generic text functionality configuration.

;;;; flyspell
;; https://www.emacswiki.org/emacs/FlySpell
;;
;; Below the ispell program is hunspell (http://hunspell.github.io/).
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
  (setq ispell-local-dictionary "en_US")
  (setq ispell-local-dictionary-alist
	;; Please note the list `("-d" "en_US")` contains ACTUAL parameters passed to hunspell
	;; You could use `("-d" "en_US,en_US-med")` to check with multiple dictionaries
	'(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)
;;	  ("el_GR" "[a-zA-Z``A_long_list_of_octal_characters'']" "[^[:alpha:]]" "[']" nil ("-d" "el_GR") nil utf-8)
	  )))

;;;; flyspell-popup
;; https://github.com/xuchunyang/flyspell-popup
(use-package flyspell-popup
  :ensure t
  :config
  (add-hook 'flyspell-mode-hook #'flyspell-popup-auto-correct-mode)
  :bind (:map flyspell-mode-map
              ("C-;" . flyspell-popup-correct)))


;;;; google-translate
;; https://github.com/atykhonov/google-translate
(use-package google-translate
  :ensure t
  :config
  (require 'google-translate)
  (require 'google-translate-default-ui)
  (setq google-translate-enable-ido-completion t)
  (global-set-key "\C-ct" 'google-translate-at-point)
  (global-set-key "\C-cT" 'google-translate-query-translate))


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


;;;; expand-region
;; https://github.com/magnars/expand-region.el
(use-package expand-region
  :ensure t
  :bind (("M-r" . er/expand-region)))


;;;; popup-imenu
;; https://github.com/ancane/popup-imenu
(use-package popup-imenu
  :ensure t
  :commands popup-imenu
  :bind ("M-i" . popup-imenu))


;;;; visual-regexp
;; https://github.com/benma/visual-regexp.el
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


;;;; adaptive-wrap
;; https://elpa.gnu.org/packages/adaptive-wrap.html
(use-package adaptive-wrap
  :ensure t
  :config
  (add-hook 'visual-line-mode-hook (lambda () (adaptive-wrap-prefix-mode t))))
