;;;;;;;; org.el
;;;;
;; File defining configuration for org-mode.

;;;; org-mode
;; https://orgmode.org/
(use-package org
  :ensure t
  :init
  (setq org-startup-folded t
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
  (setq org-todo-keywords
        '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))
  ; https://orgmode.org/worg/org-contrib/babel/languages.html#configure
  (org-babel-do-load-languages 'org-babel-load-languages '((python . t)))
  (add-hook 'org-mode-hook (lambda () (org-indent-mode t)))
  (add-hook 'org-mode-hook (lambda () (visual-line-mode t)))
  ;; Enable flyspell
  (when (require 'flyspell nil t)
    (add-hook 'org-mode-hook 'turn-on-flyspell))
  :config
  ;; Render literal blocks in the code font (`jazzy/font-family-name-code',
  ;; see environment.el; also used by prog.el). The `org-block' face backs
  ;; both #+begin_src and #+begin_example content; prose blocks
  ;; (#+begin_quote, #+begin_verse) are unaffected and keep the default font.
  ;; Only the family is overridden; the font-lock faces that native
  ;; fontification layers on top of code set only colours/weight, so they
  ;; inherit this family while keeping their syntax highlighting.
  ;;
  ;; NOTE: You can restrict the code font to `#+begin_src' blocks only (leaving
  ;; `#+begin_example' in the default font) by setting `org-src-block-faces'
  ;; instead. Each entry maps a source-block language to a face; there is no
  ;; catch-all that stays code-only (the "" key also matches example blocks),
  ;; so every language must be listed explicitly and unlisted languages fall
  ;; back to the default font. Extend as needed:
  ;;   (setq org-src-block-faces
  ;;         `(("python"     (:family ,jazzy/font-family-name-code))
  ;;           ("emacs-lisp" (:family ,jazzy/font-family-name-code))
  ;;           ("shell"      (:family ,jazzy/font-family-name-code))))
  (set-face-attribute 'org-block nil :family jazzy/font-family-name-code)
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
(use-package calfw-org
  :ensure t)
(use-package calfw-ical
  :ensure t)
(use-package calfw-gcal
  :ensure t)

;;;; org-babel
;; https://orgmode.org/worg/org-contrib/babel/

; Languages to be evaluated within source blocks in org-mode
(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)))

; results block in lowercase to match case of modern C-c C-, source templates
(setq org-babel-results-keyword "results")
