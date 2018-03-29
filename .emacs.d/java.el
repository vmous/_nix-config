;;;;;;;; java.el
;;;;
;; File defining configuration for Java development.

;;(defvar jazzy/eclim-eclipse-dirs)
;;(defvar jazzy/eclim-executable)
;;(defvar jazzy/eclimd-executable)
;;(defvar jazzy/eclimd-default-workspace)


(defvar jazzy/JAVA_HOME)
(defvar jazzy/JAVA)


(when macosx-p
;;  (setq jazzy/eclim-eclipse-dirs "/Users/vmous/Applications/Eclipse.app/Contents/Eclipse")
;;  (setq jazzy/eclim-executable "/Users/vmous/Applications/Eclipse.app/Contents/Eclipse/eclim")
;;  (setq jazzy/eclimd-executable "/Users/vmous/Applications/Eclipse.app/Contents/Eclipse/eclimd")
;;  (setq jazzy/eclimd-default-workspace "/Users/vmous/Workspace/eclipsews")

  (setq jazzy/JAVA_HOME "/Library/Java/JavaVirtualMachines/jdk1.8.0_151.jdk/Contents/Home")
  (setq jazzy/JAVA "/Library/Java/JavaVirtualMachines/jdk1.8.0_151.jdk/Contents/Home/bin/java")
)


(when linux-p
;;  (setq jazzy/eclim-eclipse-dirs "")
;;  (setq jazzy/eclim-executable "")
;;  (setq jazzy/eclimd-executable "")
;;  (setq jazzy/eclimd-default-workspace "")

  (setq jazzy/JAVA_HOME "")
  (setq jazzy/JAVA "")
)


(when amznlinux-p
;;  (setq jazzy/eclim-eclipse-dirs "")
;;  (setq jazzy/eclim-executable "")
;;  (setq jazzy/eclimd-executable "")
;;  (setq jazzy/eclimd-default-workspace "")

  (setq jazzy/JAVA_HOME "/apollo/env/JavaSE8/jdk1.8")
  (setq jazzy/JAVA "/apollo/env/JavaSE8/jdk1.8/bin/java")
)


(setenv "JAVA_HOME" jazzy/JAVA_HOME)
(setenv "JAVA" jazzy/JAVA)


;;;;;; eclim
;;;; http://eclim.org/
;;;;
;;;; Install eclim as described at http://eclim.org/install.html
;;;;
;;;; Install eclipse installer
;;;; $ brew cask install eclipse-java
;;;; Configure Eclipse root: /User/$USER/Applications/Eclipse.app (Mac) or /opt/eclipse (Linux)
;;;; Configure Eclipse workspace: /User/$USER/Workspace/eclipsews (Mac) or /home/$USER/Workspace/eclipsews (Linux)
;;;;
;;;; Download official eclim installer
;;;; $ java -Dvim.skip=true -Declipse.home=/Users/$USER/Applications/Eclipse.app/Contents/Eclipse -jar eclim_X.X.X.jar install
;;;;
;;;; Start the eclim daemon
;;;; $ $ECLIPSE_HOME/eclimd
;;;;
;;;; Install headless Eclipse
;;(use-package emacs-eclim
;;  :ensure t
;;  :commands
;;  (eclim-mode global-eclim-mode)
;;  :init
;;  ;; Eclipse installation
;;  ;; Mac
;;  (custom-set-variables
;;   '(eclim-eclipse-dirs '(jazzy/eclim-eclipse-dirs))
;;   '(eclim-executable jazzy/eclim-executable)
;;   '(eclimd-executable jazzy/eclimd-executable)
;;   '(eclimd-default-workspace jazzy/eclimd-default-workspace))
;;  (require 'eclimd)
;;  ;; add eclim hook
;;  (add-hook 'java-mode-hook 'eclim-mode)
;;  :config
;;  (setq help-at-pt-display-when-idle t)
;;  (setq help-at-pt-timer-delay 0.1)
;;  (help-at-pt-set-timer)
;;  :bind
;;  ("C-c C-e p o" . eclim-project-open)
;;  ("C-c C-e p m" . eclim-project-manage))
