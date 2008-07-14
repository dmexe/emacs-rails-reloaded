(require 'cl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Constants
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Functions
;;

(defun rails/test-unit/single-file (root rails-buffer)
  (rails/compile/run-file
   root
   rails-buffer
   "Test::Unit"
   rails/ruby/command
   "%s"))

(defun rails/test-unit/current-method (root rails-buffer)
  (when (and (member (rails/buffer-type rails-buffer) (rails/bundle-group-members "Test::Unit"))
             (eq major-mode 'ruby-mode))
    (when-bind (method (rails/ruby/current-method))
      (rails/compile/run-file
       root
       rails-buffer
       "Test::Unit"
       rails/ruby/command
       (concat "%s --name=" method)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Callbacks
;;

(defun rails/test-unit/load ()
  (rails/add-to-bundles-group "Test::Unit" :test-unit)
  (setq rails/compile/single-file-list
        (cons 'rails/test-unit/single-file
              rails/compile/single-file-list))
  (setq rails/compile/current-method-list
        (cons 'rails/test-unit/current-method
              rails/compile/current-method-list))
  (let ((map (make-sparse-keymap)))
    (define-keys map
      ([method]    (cons "Run Current Mehtod" 'rails/compile/current-method))
      ([file]      (cons "Run Single File"   'rails/compile/single-file)))
    (rails/add-to-bundles-menu "Test::Unit" map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Interactives
;;

(provide 'rails-test-unit-bundle)