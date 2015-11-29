;;; java-imports.el --- Code for dealing with Java imports

;; Copyright (C) 2015 Matthew Lee Hinman

;; Author: Lee Hinman <lee@writequit.org>
;; URL: http://www.github.com/dakrone/emacs-java-imports
;; Version: 0.1.0
;; Keywords: java
;; Package-Requires: ((s "1.10.0"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides TODO: fill me in

;;; Usage:

;; TODO: write me

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
(require 'cl-lib)
(require 's)

(defun goto-java-imports-start ()
  "Go to where java import statements should start"
  (goto-char (point-min))
  (or (re-search-forward "package .*;" nil t)
      (progn
        (goto-char (point-min))
        (re-search-forward "import .*;" nil t)))
  (forward-line 2))

(defun load-java-class-packages ()
  "TODO: implement me"
  (make-hash-table))

(defun current-line-text ()
  "The current line's text. There's probably an elisp function
for this already, but I don't know it."
  (save-excursion
    (let* ((line-start (progn (beginning-of-line-text) (point)))
           (line-end (progn (end-of-line) (point))))
      (buffer-substring line-start line-end))))

(defun import-for-line ()
  "Returns the fully-qualified class name for the import line."
  (cadr
   (s-match "import \\\(.*\\\);" (current-line-text))))

;;;###autoload
(defun import-java-class (class-name)
  "Import the Java class for the symbol at point."
  (interactive (list (read-string "Class name: " (thing-at-point 'symbol))))
  (save-excursion
    (let* ((known-packages (load-java-class-packages))
           (package (or (car (s-match ".*\\\..*" class-name))
                        (gethash class-name known-packages)
                        (read-string "Package: ")))
           (full-name (or (car (s-match ".*\\\..*" class-name))
                          (concat package "." class-name))))
      (goto-java-imports-start)
      ;; If it is a Java import, it goes one empty line below project-specific
      ;; imports
      (when (s-starts-with? "java." full-name)
        (re-search-forward "^$" nil t)
        (forward-line 1))
      (while (and (import-for-line)
                  (string< (import-for-line) full-name))
        (forward-line 1))
      (open-line 1)
      (insert-string "import " full-name ";")
      ;; Now we may need to add empty lines
      (forward-line 1)
      (when (not (import-for-line))
        (if (s-match "^$" (current-line-text))
            nil
          (open-line 1)))
      full-name)))

;;; java-imports.el ends here
