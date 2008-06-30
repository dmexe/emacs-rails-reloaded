(load "elunit")

(elunit-clear-suites)

(defsuite rails-all nil)
(defsuite ext rails-all)
(defsuite rails rails-all)


(dolist (dir '("tests.ext/" "tests.rails/"))
  (let ((default-directory (expand-file-name dir)))
    (mapcar
      #'load-file
       (directory-files "./" t "\\.test\\.el$"))))

(elunit "rails-all")