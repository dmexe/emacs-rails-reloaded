;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Constants
;;

(defconst rails/rspec-model/dir "spec/models/")
(defconst rails/rspec-model/file-suffix "_spec")
(defconst rails/rspec-model/buffer-type :rspec-model)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Functions
;;

(defun rails/rspec-model/canonical-name (file)
  (let* ((name (file-name-sans-extension file))
         (name (string-ext/cut name rails/rspec-model/dir :begin))
         (name (string-ext/cut name rails/rspec-model/file-suffix :end)))
    name))

(defun rails/rspec-model/exist-p (root resource-name)
  (when resource-name
    (let ((file (concat rails/rspec-model/dir
                        (singularize-string resource-name)
                        rails/rspec-model/file-suffix
                        rails/ruby/file-suffix)))
      (when (rails/file-exist-p root file)
        file))))

(defun rails/rspec-model/rspec-model-p (file)
  (rails/with-root file
    (string-ext/start-p (rails/cut-root file) rails/rspec-model/dir)))

(defun rails/rspec-model/resource-true-name (root resource)
  (pluralize-string resource))

(defun rails/rspec-model/type-true-name (root resource)
  rails/rspec-model/buffer-type)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Callbacks
;;

(defun rails/rspec-model/determine-type-of-file (rails-root file)
  (when (rails/rspec-model/rspec-model-p (concat rails-root file))
    (let ((name (rails/rspec-model/canonical-name file)))
      (make-rails/buffer :type   (rails/rspec-model/type-true-name rails-root name)
                         :name   name
                         :resource-name (rails/rspec-model/resource-true-name rails-root name)))))

(defun rails/rspec-model/goto-item-from-file (root file rails-current-buffer)
  (when (rails/resource-type-of-buffer rails-current-buffer)
    (when-bind (file-name
                (rails/rspec-model/exist-p root (rails/buffer-resource-name rails-current-buffer)))
      (make-rails/goto-item :group :rspec
                            :name "RSpec Model"
                            :file file-name))))

(defalias 'rails/rspec-model/goto-item-from-rails-buffer
          'rails/rspec-model/goto-item-from-file)

(defun rails/rspec-model/load ()
  (rails/define-bundle
   rails/rspec-model/buffer-type rails/rspec-model/buffer-type "RSpec"
   (rails/add-type-link :tests :model rails/rspec-model/buffer-type)
   (rails/define-goto-key "u" 'rails/rspec-model/goto-from-list)
   (rails/define-goto-menu  "RSpec Model" 'rails/rspec-model/goto-from-list)
   (rails/define-toggle-key "u" 'rails/rspec-model/goto-current)
   (rails/define-toggle-menu  "RSpec Model" 'rails/rspec-model/goto-current)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Interactives
;;

(defun rails/rspec-model/goto-from-list ()
  (interactive)
  (when-bind (root (rails/root))
   (rails/directory-to-goto-menu root
                                 rails/rspec-model/dir
                                 "Select a RSpec Model"
                                 :filter-by 'rails/rspec-model/rspec-model-p
                                 :name-by (funcs-chain file-name-sans-extension string-ext/decamelize))))

(defun rails/rspec-model/goto-current ()
  (interactive)
  (rails/with-current-buffer
   (when-bind (goto-item
               (rails/rspec-model/goto-item-from-rails-buffer (rails/root)
                                                            (rails/cut-root (buffer-file-name))
                                                            rails/current-buffer))
     (rails/toggle-file-by-goto-item (rails/root) goto-item))))

(provide 'rails-rspec-model-bundle)