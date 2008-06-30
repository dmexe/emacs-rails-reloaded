(require 'elunit)
(require 'files-ext)
(require 'string-ext)

(defsuite files-ext ext)

(deftest files-ext/file-special-p files-ext
  (assert (files-ext/file-special-p "."))
  (assert (files-ext/file-special-p ".."))
  (assert (files-ext/file-special-p "#foo.bar"))
  (assert (files-ext/file-special-p "~foo.bar"))
  (assert-nil (files-ext/file-special-p "foo.bar")))

(deftest files-ext/find-recursive-files files-ext
  (let ((files '("core" "files" "list" "string"))) ;; files in current directory
    (assert-equal
     files
     (files-ext/find-recursive-files
      '(lambda(it) (string-ext/cut it "-ext.test.el" :end))
      "\\-ext\\.test\\.el"
      "./"))))
