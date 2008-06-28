(defconst rails/helper/directory "app/helpers/")
(defconst rails/helper/file-suffix "_helper")
(defconst rails/helper/buffer-weight 1)
(defconst rails/helper/buffer-type :helper)

(defun rails/helper/goto-item-from-file (root file rails-buffer)
  (when-bind (type (rails/associated-type-p rails-buffer rails/helper/buffer-type))
     (when-bind (file-name
                 (rails/helper/exist-p root (rails/buffer-name rails-buffer)))
       (make-rails/goto-item :group :default
                             :name "Helper"
                             :file file-name))))

(defun rails/helper/determine-type-of-file (rails-root file)
  (when (string-ext/start-p file rails/helper/directory)
    (make-rails/buffer :type   rails/helper/buffer-type
                       :weight rails/helper/buffer-weight
                       :name   (rails/helper/canonical-name file))))

(defun rails/helper/canonical-name (file)
  (let* ((name (file-name-sans-extension file))
         (name (string-ext/cut name rails/helper/directory :begin))
         (name (string-ext/cut name rails/helper/file-suffix :end)))
    name))

(defun rails/helper/exist-p (root canonical-name)
  (let ((file (concat rails/helper/directory
                      (pluralize-string canonical-name)
                      rails/helper/file-suffix
                      rails/ruby/file-suffix)))
    (when (rails/file-exist-p root file)
      file)))

(defun rails/helper/initialize (root file)
  )

(defun rails/helper/load ()
  (rails/add-to-associated-types-list rails/helper/buffer-type))

(provide 'rails-helper-bundle)