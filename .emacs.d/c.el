;;;;;;;; c.el
;;;;
;; File defining configuration for C/C++/Obj-C development.

;;;; irony-mode
;; https://github.com/Sarcasm/irony-mode
;;
;; Installation instructions
;;
;; - Mac
;;   Install the gtags
;;     $ brew install globaly
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
