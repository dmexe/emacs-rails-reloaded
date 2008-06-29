(defconst rails/model/dir "app/models/")
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
  (when (string-ext/start-p file rails/model/dir)
    (make-rails/buffer :type   rails/model/buffer-type
                       :weight rails/model/buffer-weight
                       :name   (rails/model/canonical-name file))))

(defun rails/model/canonical-name (file)
  (let* ((name (file-name-sans-extension file))
         (name (string-ext/cut name rails/model/dir :begin)))
    name))

(defun rails/model/exist-p (root canonical-name)
  (let ((file (concat rails/model/dir
                      (singularize-string canonical-name)
                      rails/ruby/file-suffix)))
    (when (rails/file-exist-p root file)
      file)))

(defun rails/model/initialize (root file)
)

(defun rails/model/load ()
  (rails/add-to-associated-types-list rails/model/buffer-type)
  (rails/define-goto-key "m" 'rails/model/goto-from-list)
  (rails/define-fast-goto-key "m" 'rails/model/goto-associated))

(defun rails/model/goto-from-list ()
  (interactive)
  (let ((file (buffer-file-name)))
    (rails/with-root file
      (rails/directory-to-goto-menu (rails/root)
                                    rails/model/dir
                                    "Select a Model"
                                    'rails/model/model-p))))

(defun rails/model/model-p (root file)
  (when-bind (buf (rails/determine-type-of-file root (concat rails/model/dir file)))
    (when (eq rails/model/buffer-type (rails/buffer-type buf))
      buf)))

(defun rails/model/goto-associated ()
  (interactive)
  (let ((file (buffer-file-name))
        (rails-buffer rails/current-buffer))
    (rails/with-root file
      (when-bind
       (goto-item
        (rails/model/goto-item-from-file (rails/root)
                                         (rails/cut-root file)
                                         rails-buffer))
       (rails/fast-find-file-by-goto-item (rails/root) goto-item)))))

(provide 'rails-model-bundle)