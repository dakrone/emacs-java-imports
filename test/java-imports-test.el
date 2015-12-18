;;; java-imports-test.el --- tests for java imports

;; Copyright (C) 2015  Matthew Lee Hinman

;;; Code:

(require 'ert)
(load-file "java-imports.el")


(ert-deftest t-import-for-line ()
  (with-temp-buffer
    (insert "import java.util.List;")
    (should (equal (java-imports-import-for-line)
                   "java.util.List")))
  (with-temp-buffer
    (insert "		  import org.writequit.Thingy;  ")
    (should (equal (java-imports-import-for-line)
                   "org.writequit.Thingy"))))


(ert-deftest t-go-to-imports-start ()
  ;; both package and imports present? Goto to the first import line beginning
  (with-temp-buffer
    (insert "package mypackage;\n")
    (insert "\n")
    (insert "import java.util.List;\n")
    (insert "import java.util.ArrayList;\n")
    (insert "\n\n")
    (java-imports-go-to-imports-start)
    (should (equal (line-number-at-pos) 3)))

  ;; no package and imports present? First import line
  (with-temp-buffer
    (insert "\n")
    (insert "\n")
    (insert "\n")
    (insert "import java.util.List;\n")
    (insert "import java.util.ArrayList;\n")
    (insert "\n\n")
    (java-imports-go-to-imports-start)
    (should (equal (line-number-at-pos) 4)))

  ;; package present, no imports? Add a correct import place, keeping the empty
  ;; lines
  (with-temp-buffer
    (insert "\n")
    (insert "package mypackage;\n")
    (insert "\n")
    (insert "\n")
    (insert "class A {}\n")
    (java-imports-go-to-imports-start)
    (should (equal (line-number-at-pos) 4))
    (should (equal (count-lines (point-min) (point-max)) 7)))

  ;; no package, no imports? Stay in the beginning, add lines required
  (with-temp-buffer
    (insert "\n")
    (insert "\n")
    (insert "\n")
    (insert "class A {}\n")
    (java-imports-go-to-imports-start)
    (should (equal (line-number-at-pos) 1))
    (should (equal (count-lines (point-min) (point-max)) 5))))

(ert-deftest t-add-imports ()
  (with-temp-buffer
    (setq-local java-imports-find-block-function
                #'java-imports-find-place-after-last-import)
    (insert "package mypackage;\n\n")
    (insert "import java.util.List;\n\n\n")
    (java-imports-add-import-with-package "ArrayList" "java.util")
    (should
     (equal
      (buffer-string)
      (concat
       "package mypackage;\n\n"
       "import java.util.List;\n"
       "import java.util.ArrayList;\n\n\n"))))

  (with-temp-buffer
    (setq-local java-imports-find-block-function
                #'java-imports-find-place-sorted-block)
    (insert "package mypackage;\n\n")
    (insert "import java.util.List;\n\n\n")
    (java-imports-add-import-with-package "ArrayList" "java.util")
    (should
     (equal
      (buffer-string)
      (concat
       "package mypackage;\n\n"
       "import java.util.ArrayList;\n"
       "import java.util.List;\n\n\n")))))

;; End:
;;; java-imports-test.el ends here
