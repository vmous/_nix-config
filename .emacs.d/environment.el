;;;;;;;; environment.el
;;;;
;; File defining the runtime environment sensitive variables.

;;;;;; Load secrets
;; Keep sensitive information separately in order to be able to safely
;; publish the Emacs configuration.
(load "~/.emacs.d/secrets.el")

;;;;; System conditionals
;; operating system identification
(defconst jazzy/env/is-macosx (eq system-type 'darwin)
  "True if Emacs is running on macOS; nil otherwise.")
(defconst jazzy/env/is-linux (eq system-type 'gnu/linux)
  "True if Emacs is running on Linux; nil otherwise.")

;; system architecture identification
(defconst jazzy/env/is-arch-x86
  (and (string-match-p "\\`\\(x86_64\\|i[3-6]86\\)-" system-configuration) t)
  "True if Emacs was built for an x86 CPU; nil otherwise.")
(defconst jazzy/env/is-arch-arm
  (and (string-match-p "\\`\\(aarch64\\|arm\\)" system-configuration) t)
  "True if Emacs was built for an ARM CPU; nil otherwise.")

;; operating system and architecture identification
(defconst jazzy/env/is-macosx-x86
  (and jazzy/env/is-macosx jazzy/env/is-arch-x86)
  "True if Emacs is running on macOS on an x86 (Intel) CPU; nil otherwise.")
(defconst jazzy/env/is-macosx-arm
  (and jazzy/env/is-macosx jazzy/env/is-arch-arm)
  "True if Emacs is running on macOS on an ARM (Apple Silicon) CPU; nil
otherwise.")
(defconst jazzy/env/is-linux-x86
  (and jazzy/env/is-linux jazzy/env/is-arch-x86)
  "True if Emacs is running on Linux on an x86 CPU; nil otherwise.")
(defconst jazzy/env/is-linux-arm
  (and jazzy/env/is-linux jazzy/env/is-arch-arm)
  "True if Emacs is running on Linux on an ARM CPU; nil otherwise.")

;; host name identification
(defconst jazzy/env/is-my-macosx
  (string= system-name jazzy/secrets/system-name/macosx)
  "True if Emacs is running on my Mac OS X system; nil otherwise.")
(defconst jazzy/env/is-my-linux
  (string= system-name jazzy/secrets/system-name/linux)
  "True if Emacs is running on my Linux system; nil otherwise.")
(defconst jazzy/env/is-my-amznlinux
  (string= system-name jazzy/secrets/system-name/amznlinux)
  "True if Emacs is running on my Amazon Linux system; nil otherwise.")

;;;; Environment Variables
;; Emacs started from the macOS Dock or Finder is launched by launchd and gets
;; none of the environment set up by the shell's startup files, so import it.
(when jazzy/env/is-my-macosx
  (defconst jazzy/env/shell-variables '("PATH" "JAVA_HOME")
    "Environment variables imported from the user's login shell.")

  (defun jazzy/funcs/import-env-from-shell ()
    "Set `jazzy/env/shell-variables' as in the user's interactive login shell.
Also set `exec-path' to match `PATH'."
    (interactive)
    ;; Mark each value, so other output of the interactive shell (e.g., a
    ;; greeting) is ignored.
    (let ((output (shell-command-to-string
                   (format "$SHELL --login -i -c '%s'"
                           (mapconcat
                            (lambda (var)
                              (format
                               "printf \"\\n__jazzy_env__ %s=%%s\\n\" \"$%s\""
                               var var))
                            jazzy/env/shell-variables "; ")))))
      (dolist (var jazzy/env/shell-variables)
        (when (string-match (format "^__jazzy_env__ %s=\\(.+\\)$" var) output)
          (setenv var (match-string 1 output))))
      (setq exec-path (append (parse-colon-path (getenv "PATH"))
                              (list exec-directory)))))

  (jazzy/funcs/import-env-from-shell))

;;;; Fonts
;; Font family names and size reused across the configuration so they are
;; defined in a single place. `jazzy/font-family-name-default' is the
;; `default' face font applied frame-wide in `.emacs' (prose, dired, magit,
;; org, help, ...); `jazzy/font-family-name-code' is the family remapped onto
;; code buffers in `prog.el' and onto org literal blocks in `org.el'.
;; `jazzy/font-size-default' is the point size of the `default' face; code
;; buffers and org blocks inherit it through their relative face remaps.
;;
;; Each family is resolved from a preference list by
;; `jazzy/funcs/preferred-font-family': the first installed candidate wins, and
;; if none are installed it falls back to the running frame's default family.
;; The result is therefore ALWAYS a valid family string, so consumers (`.emacs',
;; `prog.el', `org.el') can use these constants directly without guarding
;; against a missing font -- all the fallback logic stays here.
;;
;; The lists are platform-agnostic: Minion Pro is preferred for prose but is
;; proprietary (macOS gets it bundled with Adobe Acrobat), so on Linux -- where
;; it is absent -- resolution simply advances to the open Crimson Pro.
;;
;; Note: The named fonts must be installed on the system to take effect (see
;; the `j-install-fonts' shell alias).
(defun jazzy/funcs/preferred-font-family (families)
  "Return the first installed family from FAMILIES, else the frame default.
FAMILIES is a list of family-name strings tried in order (nil entries are
skipped); a family is available when `find-font' locates it. When none are
installed, the running frame's `default' face family is returned, so the
result is always a usable family string rather than nil."
  (or (catch 'found
        (dolist (family families)
          (when (and family (find-font (font-spec :family family)))
            (throw 'found family)))
        nil)
      (face-attribute 'default :family)))

(defconst jazzy/font-family-name-default
  (jazzy/funcs/preferred-font-family '("Source Code Pro"))
  "Font family for the `default' face, used everywhere that is not code.
Always a valid family: the first installed of the preferred prose serifs,
or the frame default when none are installed.")
(defconst jazzy/font-family-name-code
  (jazzy/funcs/preferred-font-family '("Monaspace Xenon NF"))
  "Font family for code buffers and org literal blocks.
Always a valid family: the preferred code font when installed, otherwise the
frame default.")
(defconst jazzy/font-size-default 12
  "Point size of the `default' face, used everywhere that is not code.")
(defconst jazzy/font-size-code 12
  "Point size for code buffers and org literal blocks.
Applied as an absolute face height by the code-font remap in `prog.el' and
the `org-block' face in `org.el', so it is independent of
`jazzy/font-size-default'.")

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
(when (or jazzy/env/is-my-linux
          jazzy/env/is-my-macosx
          jazzy/env/is-my-amznlinux)
  (defvar jazzy/env/workspace (substitute-in-file-name "${HOME}")
    "The workspace directory which is assumed to contain project folders as first level directories (known usages: `projectile').

Defaults to ${HOME}")
  (let ((_nix-workspace (substitute-in-file-name "${HOME}/Workspace")))
    (if (file-directory-p _nix-workspace)
        (setq jazzy/env/workspace _nix-workspace)
      (setq jazzy/env/workspace "~")
      (message "Directory '%s' does not exist. Defaulting to user home '~'." _nix-workspace))))
