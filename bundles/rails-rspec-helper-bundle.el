;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Constants
;;

(defconst rails/rspec-helper/dir "spec/helpers/")
(defconst rails/rspec-helper/file-suffix "_helper_spec")
(defconst rails/rspec-helper/buffer-type :rspec-helper)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Functions
;;

(defun rails/rspec-helper/canonical-name (file)
  (let* ((name (file-name-sans-extension file))
         (name (string-ext/cut name rails/rspec-helper/dir :begin))
         (name (string-ext/cut name rails/rspec-helper/file-suffix :end)))
    name))

(defun rails/rspec-helper/exist-p (root resource-name)
  (when resource-name
    (let ((file (concat rails/rspec-helper/dir
                        resource-name
                        rails/rspec-helper/file-suffix
                        rails/ruby/file-suffix)))
      (when (rails/file-exist-p root file)
        file))))

(defun rails/rspec-helper/rspec-helper-p (file)
  (rails/with-root file
    (string-ext/start-p (rails/cut-root file) rails/rspec-helper/dir)))

(defun rails/rspec-helper/resource-true-name (root resource)
  (let ((plural (pluralize-string resource)))
    (if (rails/rspec-helper/exist-p root plural)
        plural
      (singularize-string resource))))

(defun rails/rspec-helper/type-true-name (root resource)
  rails/rspec-helper/buffer-type)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Callbacks
;;
(defun rails/rspec-helper/determine-type-of-file (rails-root file)
  (when (rails/rspec-helper/rspec-helper-p (concat rails-root file))
    (let ((name (rails/rspec-helper/canonical-name file)))
      (make-rails/buffer :type   (rails/rspec-helper/type-true-name rails-root name)
                         :name   name
                         :resource-name (rails/rspec-helper/resource-true-name rails-root name)))))

(defun rails/rspec-helper/goto-item-from-file (root file rails-current-buffer)
  (when (rails/resource-type-of-buffer rails-current-buffer)
    (when-bind (file-name
                (rails/rspec-helper/exist-p root (rails/buffer-resource-name rails-current-buffer)))
      (make-rails/goto-item :group :rspec
                            :name "RSpec Helper"
                            :file file-name))))

(defalias 'rails/rspec-helper/goto-item-from-rails-buffer
          'rails/rspec-helper/goto-item-from-file)

(defun rails/rspec-helper/load ()
  (rails/define-bundle
   rails/rspec-helper/buffer-type rails/rspec-helper/buffer-type "RSpec"
   (rails/add-type-link :tests :helper rails/rspec-helper/buffer-type)
   (rails/define-goto-key "H" 'rails/rspec-helper/goto-from-list)
   (rails/define-goto-menu  "RSpec Helper" 'rails/rspec-helper/goto-from-list)
   (rails/define-toggle-key "H" 'rails/rspec-helper/goto-current)
   (rails/define-toggle-menu  "RSpec Helper" 'rails/rspec-helper/goto-current)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Interactives
;;

(defun rails/rspec-helper/goto-from-list ()
  (interactive)
  (when-bind (root (rails/root))
   (rails/directory-to-goto-menu root
                                 rails/rspec-helper/dir
                                 "Select a RSpec Helper"
                                 :filter-by 'rails/rspec-helper/rspec-helper-p
                                 :name-by (funcs-chain file-name-sans-extension string-ext/decamelize))))

(defun rails/rspec-helper/goto-current ()
  (interactive)
  (rails/with-current-buffer
   (when-bind (goto-item
               (rails/rspec-helper/goto-item-from-rails-buffer (rails/root)
                                                            (rails/cut-root (buffer-file-name))
                                                            rails/current-buffer))
     (rails/toggle-file-by-goto-item (rails/root) goto-item))))

(provide 'rails-rspec-helper-bundle)