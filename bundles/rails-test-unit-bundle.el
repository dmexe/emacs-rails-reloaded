;;; ---------------------------------------------------------
;;; - Functions
;;;

(defun rails/test-unit-bundle/single-file (root rails-buffer)
  (rails/compile/run-file
   root
   rails-buffer
   rails/ruby/command
   "%s"
   "_test\\.rb$"))

(defun rails/test-unit-bundle/current-method (root rails-buffer)
  (when-bind (method (rails/ruby/current-method))
    (rails/compile/run-file
     root
     rails-buffer
     rails/ruby/command
     (concat "%s --name=" method)
     "_test\\.rb$")))

(defun rails/test-unit-bundle/run-test-task (root task args)
  (rails/compile/run root
                     (if (boundp 'rails/rake-bundle/command)
                         rails/rake-bundle/command
                       "rake")
                     (format "%s %s" task (if args args ""))))

(defun rails/test-unit-bundle/after-load ()
  (setq rails/compile/single-file-list
        (cons 'rails/test-unit-bundle/single-file
              rails/compile/single-file-list))
  (setq rails/compile/current-method-list
        (cons 'rails/test-unit-bundle/current-method
              rails/compile/current-method-list))

  (when (boundp 'rails/rake-bundle/tasks-runners-alist)
    (add-to-list 'rails/rake-bundle/tasks-runners-alist
                 '("^test" . rails/test-unit-bundle/run-test-task))))

;;; ---------------------------------------------------------
;;; - Bundle
;;;

(rails/defbundle "Test::Unit"
  (:menu
   (([method]    (cons "Run Current Mehtod" 'rails/compile/current-method))
    ([file]      (cons "Run Single File"   'rails/compile/single-file)))
   :after-load-bundles
   'rails/test-unit-bundle/after-load)

  ;;; ---------------------------------------------------------
  ;;; - Resources
  ;;;

  (rails/defresource 'unit-test "Unit Test"
                   :group 'unit-test
                   :dir "test/unit"
                   :file-suffix  "_test"
                   :file-ext  "rb"
                   :options 'pluralize
                   :test-to 'model)

  (rails/defresource 'unit-test-mailer "Unit Test Mailer"
                     :group 'unit-test
                     :dir "test/unit"
                     :file-suffix  "_test"
                     :skip-file-suffix "_mailer"
                     :file-ext  "rb"
                     :weight 2
                     :test-to 'mailer)

  (rails/defresource 'fixture "Fixture"
                     :group 'unit-test
                     :dir "test/fixtures"
                     :file-ext  "yml")

  (rails/defresource 'functional-test "Functional Test"
                     :group 'unit-test
                     :dir "test/functional"
                     :file-suffix  "_controller_test"
                     :file-ext  "rb"
                     :test-to 'controller))
