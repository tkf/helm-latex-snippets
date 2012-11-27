;;; helm-latex-snippets.el --- Helm/anything interface to search latex snippets

;; Copyright (C) 2012 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; helm-latex-snippets.el is free software: you can redistribute it
;; and/or modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; helm-latex-snippets.el is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied warranty
;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with helm-latex-snippets.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile (require 'cl))

(declare-function anything-other-buffer "anything")
(declare-function helm-other-buffer "helm")


;;; Utility functions

(defun hls--directory-files-recursively (directory)
  "List files under DIRECTORY."
  (setq directory (file-name-as-directory directory))
  (let ((aspath (lambda (f) (concat directory f))))
    (loop for f in (directory-files directory)
          for p = (funcall aspath f)
          if (or (equal f ".") (equal f ".."))
          do (ignore)
          else if (file-directory-p p)
          append (hls--directory-files-recursively p)
          else
          append (list p))))


;;;

(defvar hls--source-dir
  (or (and load-file-name (file-name-directory load-file-name))
      default-directory)
  "Directory in which ``helm-latex-snippets.el`` locate.")

(defun hls--find-images (directory)
  "Find all png files under DIRECTORY."
  (loop for f in (hls--directory-files-recursively directory)
        when (string-match-p ".png\\'" f)
        collect f))

(defun hls--insert-lines-math ()
  "Insert images and search keywords into the current buffer.
Images are fetched from the directory HLS--SOURCE-DIR/build/math.
This directory have sub-directories. These sub-directories are
interpreted as symbol \"category\".  Example::

    build/math/amsmath/iint.png
                 |         |
              category   symbol
"
  (loop with base-dir = "build/math"
        with directory = (expand-file-name base-dir hls--source-dir)
        for f in (hls--find-images directory)
        for name = (file-name-sans-extension (file-name-nondirectory f))
        for category = (substring
                        (file-relative-name (file-name-directory f) directory)
                        0 -1)
        do (insert-image (create-image f))
        do (insert (format
                    "%s \\%s (%s)\n"
                    ;; Serialize data to pass to display-to-real
                    ;; (which is `read') into a S-expression and hide
                    ;; it using text property.
                    (propertize (format "%S" (format "\\%s" name))
                                'invisible t)
                    name category))))

(defvar hls-candidate-buffer
  (if (locate-library "helm")
      #'helm-candidate-buffer
    #'anything-candidate-buffer))


;;; Math symbols source

(defun hls--math-init ()
  (with-current-buffer (funcall hls-candidate-buffer 'global)
    (erase-buffer)
    (hls--insert-lines-math)))

(defvar hls--math-source
  '((name . "Latex Math Symbols")
    (init . hls--math-init)
    (get-line . buffer-substring) ; default is `buffer-substring-no-properties'
    (candidates-in-buffer)
    (display-to-real . read)
    (action . insert)))

;;;###autoload
(defun anything-latex-snippets-math ()
  "Search latex math symbols."
  (interactive)
  (let ((hls-candidate-buffer #'anything-candidate-buffer))
    (anything-other-buffer hls--math-source
                           "*anything latex snippets math*")))

;;;###autoload
(defun helm-latex-snippets-math ()
  "Search latex math symbols."
  (interactive)
  (let ((hls-candidate-buffer #'helm-candidate-buffer))
    (helm-other-buffer hls--math-source
                       "*helm latex snippets math*")))


;;; Builder

;;;###autoload
(defun hls-build-math ()
  "Generate math symbol images."
  (interactive)
  (let ((default-directory hls--source-dir))
    (compile "python generate.py math")))

(provide 'helm-latex-snippets)

;;; helm-latex-snippets.el ends here
