;;;;;;;; prog.el
;;;;
;; File defining generic configuration for development.

;;;; company-mode
;; https://github.com/company-mode/company-mode
(use-package company
  ;; https://github.com/emacsmirror/diminish
;;  :diminish company-mode
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


;;;; magit
;; https://github.com/magit/magit
;; http://daemianmack.com/magit-cheatsheet.html
(use-package magit
  :ensure t
  :config
  (setq magit-completing-read-function 'magit-ido-completing-read)
  (add-hook 'git-commit-mode-hook 'turn-on-flyspell)
  :bind
  ("C-c g s" . magit-status)
  ("C-c g b" . magit-blame))


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


;;;; projectile
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
;  (projectile-discover-projects-in-directory jazzy/env/workspace)
  (setq projectile-mode-line '(:eval (format " ☄[%s]" (projectile-project-name)))
        projectile-enable-caching t))


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
