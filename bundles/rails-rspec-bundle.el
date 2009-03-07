;;; ---------------------------------------------------------
;;; - Variables
;;;

(defvar rails/rspec-bundle/command rails/ruby/command)
(defvar rails/rspec-bundle/spec-options "-O spec/spec.opts")

;;; ---------------------------------------------------------
;;; - Functions
;;;

(defun rails/rspec-bundle/single-file (root rails-buffer)
  (rails/compile/run-file
   root
   rails-buffer
   rails/rspec-bundle/command
   (concat "script/spec %s" (format " %s" rails/rspec-bundle/spec-options))
   "_spec\\.rb$"))

(defun rails/rspec-bundle/current-method (root rails-buffer)
  (when-bind (line (line-number-at-pos))
    (rails/compile/run-file
     root
     rails-buffer
     rails/rspec-bundle/command
     (concat "script/spec %s" (format " %s -l %s" rails/rspec-bundle/spec-options line))
     "_spec\\.rb$")))

(defun rails/rspec-bundle/run-spec-task (root task args)
  (rails/compile/run root
                     (if (boundp 'rails/rake-bundle/command)
                         rails/rake-bundle/command
                       "rake")
                     (format "%s %s" task (if args args ""))))

(defun rails/rspec-bundle/after-load ()
  (when (boundp 'rails/rake-bundle/tasks-runners-alist)
    (add-to-list 'rails/rake-bundle/tasks-runners-alist
                 '("^spec" . rails/rspec-bundle/run-spec-task)))
  (setq rails/compile/single-file-list
        (cons 'rails/rspec-bundle/single-file
              rails/compile/single-file-list))
  (setq rails/compile/current-method-list
        (cons 'rails/rspec-bundle/current-method
              rails/compile/current-method-list)))

;;; ---------------------------------------------------------
;;; - Bundle
;;;
(rails/defbundle "RSpec"
  (:menu
   (([method]    (cons "Run Current Mehtod" 'rails/compile/current-method))
    ([file]      (cons "Run Single File"   'rails/compile/single-file)))
   :after-load-bundles
   'rails/rspec-bundle/after-load)

  ;;; ---------------------------------------------------------
  ;;; - Resources
  ;;;

  (rails/defresource 'model-spec "RSpec Model"
                     :group 'spec
                     :dir "spec/models"
                     :file-suffix  "_spec"
                     :file-ext  "rb"
                     :options 'pluralize
                     :test-to 'model)

  (rails/defresource 'controller-spec "RSpec Controller"
                     :group 'spec
                     :dir "spec/controllers"
                     :file-suffix  "_controller_spec"
                     :file-ext  "rb"
                     :test-to 'controller)

  (rails/defresource 'helper-spec "RSpec Helper"
                     :group 'spec
                     :dir "spec/helpers"
                     :file-suffix  "_helper_spec"
                     :file-ext  "rb"
                     :test-to 'helper)

  (rails/defresource 'fixture-spec "RSpec Fixture"
                     :group 'spec
                     :dir "spec/fixtures"
                     :file-ext "yml")

  (rails/defresource 'factory-spec "RSpec Factory"
                     :dir "spec/factories"
                     :file-ext  "rb"
                     :options  'pluralize)

  (rails/defresource 'view-spec "RSpec View"
                     :group 'views
                     :dir "spec/views"
                     :file-suffix  "_spec"
                     :file-pattern "{name}/.*"
                     :file-ext  "rb"
                     :options 'expand-in-menu
                     :test-to 'view ))