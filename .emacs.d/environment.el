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


;;;; Environment Variabes
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

;;;; Backup/auto-save directory
(defvar jazzy/env/backup-dir temporary-file-directory
  "Directory where the backup (*~) and auto-save (#*#) files are saved.

Defaults to the system's temporary directory (e.g., /tmp/ for *nix).")
(let ((_backup-dir (expand-file-name (concat user-emacs-directory "backup"))))
  (if (file-exists-p _backup-dir)
      (setq jazzy/env/backup-dir _backup-dir)
    (if (y-or-n-p (format "Directory `%s' does not exist! Create it?" _backup-dir))
        (progn
          (make-directory _backup-dir)
          (setq jazzy/env/backup-dir _backup-dir)))))

;;;; Workspace
(when (or linux-p macosx-p amznlinux-p)
  (defvar jazzy/env/workspace (substitute-in-file-name "${HOME}")
    "The workspace directory which is assumed to contain project folders as first level directories (known usages: `projectile').

Defaults to ${HOME}")
  (let ((_nix-workspace (substitute-in-file-name "${HOME}/Workspace")))
    (if (file-directory-p _nix-workspace)
        (setq jazzy/env/workspace _nix-workspace)
      (setq jazzy/env/workspace "~")
      (message "Directory '%s' does not exist. Defaulting to user home '~'." _nix-workspace))))
