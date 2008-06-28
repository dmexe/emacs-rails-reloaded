(defconst rails/controller/directory "app/controllers/")
(defconst rails/controller/file-suffix "_controller")
(defconst rails/controller/buffer-weight 1)
(defconst rails/controller/buffer-type :controller)

(defun rails/controller/goto-item-from-file (root file rails-buffer)
  (when-bind (type (rails/associated-type-p rails-buffer rails/controller/buffer-type))
     (when-bind (file-name
                 (rails/controller/exist-p root (rails/buffer-name rails-buffer)))
       (make-rails/goto-item :group :default
                             :name "Controller"
                             :file file-name))))

(defun rails/controller/determine-type-of-file (rails-root file)
  (when (string-ext/start-p file rails/controller/directory)
    (make-rails/buffer :type   rails/controller/buffer-type
                       :weight rails/controller/buffer-weight
                       :name   (rails/controller/canonical-name file))))

(defun rails/controller/canonical-name (file)
  (let* ((name (file-name-sans-extension file))
         (name (string-ext/cut name rails/controller/directory :begin))
         (name (string-ext/cut name rails/controller/file-suffix :end)))
    name))

(defun rails/controller/exist-p (root canonical-name)
  (let ((file (concat rails/controller/directory
                      (pluralize-string canonical-name)
                      rails/controller/file-suffix
                      rails/ruby/file-suffix)))
    (when (rails/file-exist-p root file)
      file)))

(defun rails/controller/initialize (root file rails-current-buffer)
)

(defun rails/controller/load ()
  (rails/add-to-associated-types-list rails/controller/buffer-type))


(provide 'rails-controller-bundle)