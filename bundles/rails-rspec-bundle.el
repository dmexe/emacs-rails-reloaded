(require 'cl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Constants
;;
(defvar rails/rspec/command "spec")
(defvar rails/rspec/spec-options "-O spec/spec.opts")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Functions
;;

(defun rails/rspec/single-file (root rails-buffer)
  (rails/compile/run-file
   root
   rails-buffer
   "RSpec"
   rails/rspec/command
   (concat "%s" (format " %s" rails/rspec/spec-options))))

(defun rails/rspec/current-method (root rails-buffer)
  (when (and (member (rails/buffer-type rails-buffer) (rails/bundle-group-members "RSpec"))
             (eq major-mode 'ruby-mode))
    (when-bind (line (line-number-at-pos))
      (rails/compile/run-file
       root
       rails-buffer
       "RSpec"
       rails/rspec/command
       (concat "%s" (format " %s -l %s" rails/rspec/spec-options line))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Callbacks
;;

(defun rails/rspec/load ()
  (rails/define-bundle
   :rspec nil "RSpec"
  (setq rails/compile/single-file-list
        (cons 'rails/rspec/single-file
              rails/compile/single-file-list))
  (setq rails/compile/current-method-list
        (cons 'rails/rspec/current-method
              rails/compile/current-method-list))
  (rails/define-key "<return>" 'rails/rspec/run-current-file-as-spec)
  (let ((map (make-sparse-keymap)))
    (define-keys map
      ([spec]      (cons "Run Current File as Spec" 'rails/rspec/run-current-file-as-spec))
      ([method]    (cons "Run Current Mehtod" 'rails/compile/current-method))
      ([file]      (cons "Run Single File"   'rails/compile/single-file)))
    (rails/add-to-bundles-menu "RSpec" map))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Interactives
;;

(defun rails/rspec/run-current-file-as-spec ()
  (interactive)
  (when-bind(root (rails/root))
    (let ((file (rails/cut-root (buffer-file-name))))
      (when file
        (rails/compile/run root
                           rails/rspec/command
                           (format "%s %s" file rails/rspec/spec-options))))))

(provide 'rails-rspec-bundle)