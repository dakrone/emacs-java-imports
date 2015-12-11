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

;; End:
;;; java-imports-test.el ends here
