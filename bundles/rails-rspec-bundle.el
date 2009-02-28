;;; ---------------------------------------------------------
;;; - Variables
;;;

(defvar rails/rspec-bundle/command "spec")
(defvar rails/rspec-bundle/spec-options "-O spec/spec.opts")

;;; ---------------------------------------------------------
;;; - Functions
;;;

(defun rails/rspec-bundle/single-file (root rails-buffer)
  (rails/compile/run-file
   root
   rails-buffer
   rails/rspec-bundle/command
   (concat "%s" (format " %s" rails/rspec-bundle/spec-options))
   "_spec\\.rb$"))

(defun rails/rspec-bundle/current-method (root rails-buffer)
  (when-bind (line (line-number-at-pos))
    (rails/compile/run-file
     root
     rails-buffer
     rails/rspec-bundle/command
     (concat "%s" (format " %s -l %s" rails/rspec-bundle/spec-options line))
     "_spec\\.rb$")))

;;; ---------------------------------------------------------
;;; - Bundle
;;;

(rails/defbundle "RSpec"
  (:menu
   (([method]    (cons "Run Current Mehtod" 'rails/compile/current-method))
    ([file]      (cons "Run Single File"   'rails/compile/single-file))))

  ;;; ---------------------------------------------------------
  ;;; - Setup tests
  ;;;

  (setq rails/compile/single-file-list
      (cons 'rails/rspec-bundle/single-file
            rails/compile/single-file-list))
  (setq rails/compile/current-method-list
      (cons 'rails/rspec-bundle/current-method
            rails/compile/current-method-list))

  ;;; ---------------------------------------------------------
  ;;; - Resources
  ;;;

  (rails/defresource 'model-spec "Model RSpec"
                     :group 'spec
                     :dir "spec/models"
                     :file-suffix  "_spec"
                     :file-ext  "rb"
                     :pluralize t
                     :test-to 'model)

  (rails/defresource 'controller-spec "Controller RSpec"
                     :group 'spec
                     :dir "spec/controllers"
                     :file-suffix  "_controller_spec"
                     :file-ext  "rb"
                     :test-to 'controller)

  (rails/defresource 'helper-spec "Helper RSpec"
                     :group 'spec
                     :dir "spec/helpers"
                     :file-suffix  "_helper_spec"
                     :file-ext  "rb"
                     :test-to 'helper)

  (rails/defresource 'fixture-spec "Fixture"
                     :group 'spec
                     :dir "spec/fixtures"
                     :file-ext "yml"
                     :link-to 'model-spec))
