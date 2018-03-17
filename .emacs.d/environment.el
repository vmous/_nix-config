;;;;;;;; environment.el
;;;;
;; File defining the runtime environment sensitive variables.

;;;;;; Load secrets
;; Keep sensitive information separately in order to be able to safely
;; publish the Emacs configuration.
(load "~/.emacs.d/secrets.el")


;;;;; System conditionals
(defconst macosx-p (string= system-name jazzy/secrets/system-name/macosx)
  "True if Emacs is running on my Mac OS X system; nil otherwise.")
(defconst linux-p (string= system-name jazzy/secrets/system-name/linux)
  "True if Emacs is running on my Linux system; nil otherwise.")
(defconst amznlinux-p (string= system-name jazzy/secrets/system-name/amznlinux)
  "True if Emacs is running on my Amazon Linux system; nil otherwise.")


;;;; Environment Variables
(when macosx-p
  ;; Fixing exec-path discrepancy between shell and Max OSX Finder launch 
  (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
  (setq exec-path (append exec-path '("/usr/local/bin"))))
