;;;;;;;; scala.el
;;;;
;; File defining configuration for Scala development.

;;;; Scala mode
;; https://github.com/ensime/emacs-scala-mode
(use-package scala-mode
  :ensure t
  :init
  (add-hook 'scala-mode-hook
            (lambda () (setq prettify-symbols-alist scala-prettify-symbols-alist)
              (prettify-symbols-mode)))
  :interpreter
  ("scala" . scala-mode))


;;;; ENhanced Scala Interaction Mode for Emacs
;; https://github.com/ensime
;; http://ensime.github.io/
;;
;; Add the following three lines in your global sbt,
;; e.g., ~/.sbt/0.13/plugins/plugins.sbt
;;
;; ```sbt
;; 1 if (sys.props("java.version").startsWith("1.6"))
;; 2   addSbtPlugin("org.ensime" % "sbt-ensime" % "1.0.0")
;; 3 else
;; 4   addSbtPlugin("org.ensime" % "sbt-ensime" % "1.11.0")
;; ```
;;
;; Then, in you project directory run:
;; $ sbt ensimeConfig
;; $ sbt ensimeConfigProject
;;
;; Lastly, open a Scala buffer and use ```M-x ensime``` to start a connection.
(use-package ensime
  :ensure t
  :pin melpa-stable
  :after scala-mode
  :commands ensime ensime-mode
  :init
  (add-hook 'scala-mode-hook 'ensime-mode)
  (defun ensime-goto-test--test-template-scalatest-wordspec-2 ()
  "ENSIME template for ScalaCheck WordSpec style test."
  "package %TESTPACKAGE%

import org.scalatest.{ BeforeAndAfter, BeforeAndAfterAll, Matchers, WordSpec }

class %TESTCLASS% extends WordSpec with Matchers with BeforeAndAfter with BeforeAndAfterAll {

  override def beforeAll() = {
  }

  after {
  }

  \"\" should {
    \"\" in pending
  }

}
")
  (defun ensime-goto-test--test-template-scalatest-funsuite ()
  "ENSIME template for ScalaCheck FunSuite style test."
  "package %TESTPACKAGE%

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class %TESTCLASS% extends FunSuite {

  test(\"Testing\") {
    assert(true)
  }

}
")
  (custom-set-variables
   '(ensime-goto-test-config-defaults
     (quote
      (:test-class-names-fn ensime-goto-test--test-class-names
       :test-class-suffixes ("Suite" "Spec" "Test" "Check" "Specification")
       :impl-class-name-fn ensime-goto-test--impl-class-name
       :impl-to-test-dir-fn ensime-goto-test--impl-to-test-dir
       :is-test-dir-fn ensime-goto-test--is-test-dir
       :test-template-fn ensime-goto-test--test-template-scalatest-funsuite))))
  :config
  ;; Disable message for tracking SNAPSHOT version of ensime-server
  (setq ensime-startup-snapshot-notification nil
	ensime-graphical-tooltips t
	ensime-auto-generate-config t
	ensime-completion-style 'company)
  (push '(ensime-company) company-backends)
  ;; Adding asterisk in newline for multiline comments.
  (defun lunaryorn-newline-and-indent-with-asterisk ()
    (interactive)
    (newline-and-indent)
    (scala-indent:insert-asterisk-on-multiline-comment))
  (define-key ensime-mode-map (kbd "C-c C-v m") 'ensime-import-type-at-point)
  (define-key ensime-mode-map (kbd "C-c C-v M") 'ensime-imported-type-path-at-point)
  (define-key scala-mode-map (kbd "RET")
    #'lunaryorn-newline-and-indent-with-asterisk)
  (define-key scala-mode-map (kbd "C-c C-v c") 'sbt-clear))
