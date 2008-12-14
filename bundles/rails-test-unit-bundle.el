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

;;; ---------------------------------------------------------
;;; - Bundle
;;;

(rails/defbundle "Test::Unit"
  (:menu
   (([method]    (cons "Run Current Mehtod" 'rails/compile/current-method))
    ([file]      (cons "Run Single File"   'rails/compile/single-file))))

  ;;; ---------------------------------------------------------
  ;;; - Setup tests
  ;;;

  (setq rails/compile/single-file-list
        (cons 'rails/test-unit-bundle/single-file
              rails/compile/single-file-list))
  (setq rails/compile/current-method-list
        (cons 'rails/test-unit-bundle/current-method
              rails/compile/current-method-list))

  ;;; ---------------------------------------------------------
  ;;; - Resources
  ;;;

  (rails/defresource 'unit-test "Unit Test"
                   :menu-group 'unit-test
                   :bundle-name "Test::Unit"
                   :dir "test/unit"
                   :file-suffix  "_test"
                   :file-ext  "rb"
                   :pluralize t
                   :test-to 'model)

  (rails/defresource 'unit-test-mailer "Unit Test Mailer"
                     :menu-group 'unit-test
                     :bundle-name "Test::Unit"
                     :dir "test/unit"
                     :file-suffix  "_test"
                     :skip-file-suffix "_mailer"
                     :file-ext  "rb"
                     :weight 2
                     :test-to 'mailer)

  (rails/defresource 'fixture "Fixture"
                     :menu-group 'unit-test
                     :bundle-name "Test::Unit"
                     :dir "test/fixtures"
                     :file-ext  "yml"
                     :link-to '(unit-test unit-test-mailer))

  (rails/defresource 'functional-test "Functional Test"
                     :menu-group 'unit-test
                     :bundle-name "Test::Unit"
                     :dir "test/functional"
                     :file-suffix  "_controller_test"
                     :file-ext  "rb"
                     :test-to 'controller))
