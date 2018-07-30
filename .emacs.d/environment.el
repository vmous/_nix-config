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
  (defun jazzy/funcs/set-exec-path-from-shell-PATH ()
    "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell. Ensures that PATH is taken from shell. Necessary on some environments without virtualenv. Taken from: http://stackoverflow.com/questions/8606954/path-and-exec-path-set-but-emacs-does-not-find-executable."
    (interactive)
    (let ((path-from-shell (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'")))
;      (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
;      (setq exec-path (append exec-path '("/usr/local/bin")))
      ;; strip cowsay
      (setq path-from-shell (replace-regexp-in-string "^[ <].*$" "" path-from-shell))
      ;; trim spaces, tabs and new liners from end of lines
      (setq path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" path-from-shell))
      (setenv "PATH" path-from-shell)
      (setq exec-path (split-string path-from-shell path-separator))))

  ;; Fixing exec-path discrepancy between shell and Max OSX Finder launch 
  (jazzy/funcs/set-exec-path-from-shell-PATH))


;;;; Workspace
(defvar jazzy/env/workspace nil
  "The workspace directory. It is assumed that it contains projects as first level directories. Known usages: projectile.")

(when (or linux-p macosx-p amznlinux-p)
  (defconst _nix-workspace "~/Workspace")
  (if (file-directory-p _nix-workspace)
      (setq jazzy/env/workspace _nix-workspace)
    (setq jazzy/env/workspace "~")
    (message "Directory '%s' does not exist. Defaulting to user home '~'." _nix-workspace)))
