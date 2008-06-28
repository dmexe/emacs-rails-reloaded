(defconst rails/model/directory "app/models/")
(defconst rails/model/buffer-weight 1)
(defconst rails/model/buffer-type :model)

(defun rails/model/goto-item-from-file (root file rails-buffer)
  (when-bind (type (rails/associated-type-p rails-buffer rails/model/buffer-type))
     (when-bind (file-name
                 (rails/model/exist-p root (rails/buffer-name rails-buffer)))
       (make-rails/goto-item :group :default
                             :name "Model"
                             :file file-name))))

(defun rails/model/determine-type-of-file (rails-root file)
  (when (string-ext/start-p file rails/model/directory)
    (make-rails/buffer :type   rails/model/buffer-type
                       :weight rails/model/buffer-weight
                       :name   (rails/model/canonical-name file))))

(defun rails/model/canonical-name (file)
  (let* ((name (file-name-sans-extension file))
         (name (string-ext/cut name rails/model/directory :begin)))
    name))

(defun rails/model/exist-p (root canonical-name)
  (let ((file (concat rails/model/directory
                      (singularize-string canonical-name)
                      rails/ruby/file-suffix)))
    (when (rails/file-exist-p root file)
      file)))

(defun rails/model/initialize (root file)
)

(defun rails/model/load ()
  (rails/add-to-associated-types-list rails/model/buffer-type))

(provide 'rails-model-bundle)