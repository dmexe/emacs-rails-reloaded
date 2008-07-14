(defvar rails/tests/path
  (concat
   (file-name-directory
    (locate-library "rails-reloaded"))
   "tests/"))

(defun rails/tests/load (file)
  (load-file (concat rails/tests/path file ".el")))

(defun rails/tests/load-test (lib file)
  (find-file (concat rails/tests/path lib "/" file ".elk")))

(rails/tests/load "el-mock")
(rails/tests/load "fringe-helper")
(rails/tests/load "elk-test")

(add-to-list 'auto-mode-alist '("\\.elk\\'" . elk-test-mode))

(defmacro assert-type-of (expected actual)
  "Assert that ALTUAL is type of EXPECTED."
  `(unless (eq ,expected ',(type-of actual))
     (error "assert-type-of for <%s> failed: expected type <%s>, was <%s>"
            ',(type-of actual) ,expected ',(type-of actual))))

(defmacro assert-changed (changed-value &rest body)
  "Assert changed CHANGED-VALUE after eval BODY."
  `(let ((old-changed-value ,changed-value))
     ,@body
     (when (equal ,changed-value old-changed-value)
       (error "assert-changed for <%s> failed: <%s> equal <%s>"
              ',body ,changed-value old-changed-value))))

(provide 'rails-test-helper)
