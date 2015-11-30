;;; java-imports.el --- Code for dealing with Java imports

;; Copyright (C) 2015 Matthew Lee Hinman

;; Author: Lee Hinman <lee@writequit.org>
;; URL: http://www.github.com/dakrone/emacs-java-imports
;; Version: 0.1.0
;; Keywords: java
;; Package-Requires: ((s "1.10.0") (pcache "0.3.2"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides a way to easily add `import' statements for Java classes

;;; Usage:

;; (define-key java-mode-map (kbd "M-I") 'import-java-class)

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'thingatpt)
(require 's)
(require 'pcache)

(defgroup java-imports nil
  "Customization for java imports package"
  :group 'languages)

(defcustom java-imports-save-buffer-after-import-added t
  "`t' to save the current buffer after inserting an import statement."
  :group 'java-imports
  :type 'boolean)

(defcustom java-imports-use-cache t
  "Whether packages for classes should be cached"
  :group 'java-imports
  :type 'boolean)

(defun java-imports-go-to-imports-start ()
  "Go to where java import statements should start"
  (goto-char (point-min))
  (or (re-search-forward "package .*;" nil t)
      (progn
        (goto-char (point-min))
        (re-search-forward "import .*;" nil t)))
  (forward-line 2))

(defun java-imports-current-line-text ()
  "The current line's text. There's probably an elisp function
for this already, but I don't know it."
  (save-excursion
    (let* ((line-start (progn (beginning-of-line-text) (point)))
           (line-end (progn (end-of-line) (point))))
      (buffer-substring line-start line-end))))

(defun java-imports-import-for-line ()
  "Returns the fully-qualified class name for the import line."
  (cadr
   (s-match "import \\\(.*\\\);" (java-imports-current-line-text))))

;;;###autoload
(defun import-java-class (class-name)
  "Import the Java class for the symbol at point. Uses the symbol
at the point for the class name.

Checks the import cache to see if a package entry exists for the
given class. If found, adds an import statement for the class. If
not found, prompts for the package and saves it to the cache.

If called with a prefix argument, overwrites the package for an
already-existing class name."
  (interactive (list (read-string "Class name: " (thing-at-point 'symbol))))
  (save-excursion
    (let* ((key (intern class-name))
           (cache (pcache-repository "java-imports"))
           ;; Check if we have seen this class's package before
           (cached-package (and java-imports-use-cache
                                (pcache-get cache key)))
           ;; If called with a prefix, overwrite the cached value always
           (add-to-cache? (or current-prefix-arg
                              (eq nil cached-package)))
           (package (or (car (s-match ".*\\\..*" class-name))
                        (and (not current-prefix-arg)
                             cached-package)
                        (read-string "Package: ")))
           (full-name (or (car (s-match ".*\\\..*" class-name))
                          (concat package "." class-name))))
      (java-imports-go-to-imports-start)
      ;; If it is a Java import, it goes one empty line below project-specific
      ;; imports
      (when (s-starts-with? "java." full-name)
        (re-search-forward "^$" nil t)
        (forward-line 1))
      (while (and (java-imports-import-for-line)
                  (string< (java-imports-import-for-line) full-name))
        (forward-line 1))
      (open-line 1)
      (insert "import " full-name ";")
      ;; Now we may need to add empty lines
      (forward-line 1)
      (when (not (java-imports-imports-for-line))
        (if (s-match "^$" (current-line-text))
            nil
          (open-line 1)))
      (when java-imports-save-buffer-after-import-added
        (save-buffer))
      (when add-to-cache?
        (message "Adding '%s' -> '%s' to java imports cache"
                 class-name package)
        (pcache-put cache key package)
        (pcache-save cache))
      full-name)))

(provide 'import-java-class)

;;; java-imports.el ends here
