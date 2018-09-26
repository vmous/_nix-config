;;;;;;;; python.el
;;;;
;; File defining configuration for Python development.

;;;; Elpy
;; https://github.com/jorgenschaefer/elpy
;; https://elpy.readthedocs.io/en/latest/index.html
;;
;; Bootstrap a virtualenv for your python project
;; $ cd <path_to_py_project>
;; $ virtualenv --python=<path_to_py_binary> venv
;; Activate the virtualenv
;; $ source venv/bin/activate
;; Inside, install the following utilities
;; $ python -m pip install jedi autopep8 yapf flake8
;; To verify your python buffer
;; M-x elpy-config
(use-package elpy
  :ensure t
  :config
  (elpy-enable)
  (setq elpy-rpc-backend "jedi")
  ;; Use Flycheck instead of Flymake
  ;; IMPORTANT: Below snippet was commented out because it causes Emacs to hang
  ;; at startup if there are TRAMP connection to be restored with `desktop`.
;  (when (require 'flycheck nil t)
;    (remove-hook 'elpy-modules 'elpy-module-flymake)
;    (remove-hook 'elpy-modules 'elpy-module-yasnippet)
;    (remove-hook 'elpy-mode-hook 'elpy-module-highlight-indentation)
;    (add-hook 'elpy-mode-hook 'flycheck-mode))
  (add-hook 'python-mode-hook 'elpy-mode)
  (add-hook 'python-mode-hook 'company-quickhelp-mode)
  (define-key elpy-mode-map (kbd "M-.") 'elpy-goto-definition)
  (define-key elpy-mode-map (kbd "M-,") 'pop-tag-mark)
  (define-key elpy-mode-map (kbd "M-?") 'elpy-doc))


;;;; auto-virtualenvwrapper
;; https://github.com/robert-zaremba/auto-virtualenvwrapper.el
(use-package auto-virtualenvwrapper
  :ensure t
  :config
  (add-hook 'python-mode-hook #'auto-virtualenvwrapper-activate)
  ;; Activate on changing buffers (only on python mode)
  (add-hook 'window-configuration-change-hook (lambda () (if (bound-and-true-p python-mode) #'auto-virtualenvwrapper-activate)))
  ;; Activate on focus in (only on python mode)
  (add-hook 'focus-in-hook (lambda () (if (bound-and-true-p python-mode) #'auto-virtualenvwrapper-activate))))
