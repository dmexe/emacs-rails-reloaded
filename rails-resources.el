(require 'core-ext)
(require 'inflections)
(require 'rails-lib)

(defstruct rails/resource type name bundle-group
                          dir file-ext file-suffix skip-file-suffix
                          pluralize resource-name-func resource-file-func
                          link-to test-to
                          get-action-func
                          set-action-func
                          (weight 1))

(defstruct rails/buffer type
                        name
                        resource
                        get-action-func
                        set-action-func
                        weight)

(defvar rails/resources/list-defined nil)

(defun* rails/defresource (type name &key bundle-group
                                          dir file-suffix file-ext
                                          skip-file-suffix
                                          pluralize resource-name-func resource-file-func
                                          link-to test-to
                                          get-action-func set-action-func
                                          weight)
  (when (rails/resources/find type)
    (error (format "rails/resource %s already defined" type)))
  (let (res)
    (setq res (make-rails/resource :type type
                                   :bundle-group bundle-group
                                   :name name
                                   :dir (if (string-ext/end-p dir "/") dir (concat dir "/"))
                                   :file-suffix file-suffix
                                   :skip-file-suffix skip-file-suffix
                                   :file-ext  (if (string-ext/start-p file-ext ".")
                                                  file-ext
                                                (when file-ext
                                                  (concat "." file-ext)))
                                   :link-to link-to
                                   :test-to test-to
                                   :pluralize pluralize
                                   :resource-name-func resource-name-func
                                   :resource-file-func resource-file-func
                                   :get-action-func get-action-func
                                   :set-action-func set-action-func
                                   :weight (if (not weight) 1 weight)))
    (add-to-list 'rails/resources/list-defined res t)))

(defun rails/resources/find (resource-type)
  (find resource-type rails/resources/list-defined :key 'rails/resource-type))

(defun rails/resources/delete (resource-type)
  (setq
   rails/resources/list-defined
   (remove* resource-type rails/resources/list-defined :key 'rails/resource-type)))

(defun rails/resources/clear ()
  (setq rails/resources/list-defined nil))

(defun rails/resources/resource-file-p (resource file-name)
  (let ((file file-name))
    (catch 'result
      ;; dir
      (when-bind (dir (rails/resource-dir resource))
        (unless (string-ext/start-p file dir) (throw 'result nil))
        (setq file (string-ext/cut file dir :begin)))
      ;; file-ext
      (when-bind (file-ext (rails/resource-file-ext resource))
        (unless (string-ext/end-p file file-ext) (throw 'result nil))
        (setq file (string-ext/cut file file-ext :end)))
      ;; file-suffix
      (when-bind (file-suffix (rails/resource-file-suffix resource))
        (unless (string-ext/end-p file file-suffix) (throw 'result nil))
        (setq file (string-ext/cut file file-suffix :end)))
      ;; skip-file-suffix
      (when-bind (skip-file-suffix (rails/resource-skip-file-suffix resource))
        (unless (string-ext/end-p file skip-file-suffix) (throw 'result nil)))
      ;; resource-name-func
      (when-bind (resource-name-func (rails/resource-resource-name-func resource))
        (let ((res (apply resource-name-func (list file))))
          (unless res (throw 'result nil))
          (setq file res)))
      ;; pluralize
      (when (rails/resource-pluralize resource)
        (setq file (pluralize-string file)))
      ;; make resource
      (make-rails/buffer :type (rails/resource-type resource)
                         :name file
                         :weight (rails/resource-weight resource)
                         :get-action-func (rails/resource-get-action-func resource)
                         :set-action-func (rails/resource-set-action-func resource)
                         :resource resource))))

(defun rails/resources/buffer-for-file(root file)
  (let (resources)
    (setq resources
          (loop for res in rails/resources/list-defined
                for allow = (rails/resources/resource-file-p res file)
                when allow
                collect allow))
    (when (listp resources)
      (car (sort* resources '> :key 'rails/buffer-weight)))))

(defun rails/resources/associated-buffer-p (root buffer resource)
  (let ((file (rails/buffer-name buffer))
        (file-func (rails/resource-resource-file-func resource))
        result)
    (setq result
          (if file-func
              ;; resource-file-func
              (apply file-func (list root file buffer resource))
            (progn
              ;; singularize
              (when-bind (pluralize (rails/resource-pluralize resource))
                (setq file (singularize-string file)))
              ;; dir
              (when-bind (dir (rails/resource-dir resource))
                (setq file (concat dir file)))
              ;; file-suffix
              (when-bind (file-suffix (rails/resource-file-suffix resource))
                (setq file (concat file file-suffix)))
              ;; file-ext
              (when-bind (file-ext (rails/resource-file-ext resource))
                (setq file (concat file file-ext)))
              (when (rails/file-exist-p root file)
                (list (rails/resource-name resource) file)))))))

(defun rails/resources/associated-buffers(root buffer)
  (let (resources)
    (setq resources
          (loop for res in rails/resources/list-defined
                for allow = (rails/resources/associated-buffer-p root buffer res)
                when allow
                collect allow))))

(defun rails/resources/link-of(resource)
  (let ((type (rails/resource-type resource))
        (link-to (rails/resource-link-to resource)))
    (if link-to
        (rails/resources/find link-to)
      resource)))

(defun rails/resources/links-to-of(resource)
  (let ((type (rails/resource-type resource)))
    (loop for res in rails/resources/list-defined
          for allow = (eq (rails/resource-link-to res) type)
          when allow
          collect res)))


;; (rails/resources/delete 'view)
;; (rails/resources/clear)

(rails/defresource 'view "View"
                   :dir "app/views"
                   :resource-name-func '(lambda(file) (string-ext/cut (file-name-directory file) "/" :end))
                   :resource-file-func '(lambda(root name buffer resource)
                                          (rails/directory-files root (format "app/views/%s" name)))
                   :get-action-func '(lambda() (file-name-nondirectory (file-name-sans-extension (buffer-file-name)))))

(rails/defresource 'migration "Migration"
                   :dir "db/migrate"
                   :file-ext  "rb"
                   :resource-name-func '(lambda(file) (string-ext/string=~ "^[0-9]+_create_\\(.*\\)" file $1))
                   :resource-file-func '(lambda(root name buffer resource)
                                          (rails/directory-files root "db/migrate" nil (format "^[0-9]+_create_%s\\.rb$" name)))
                   :link-to 'model)

(rails/defresource 'model "Model"
                   :dir "app/models"
                   :file-ext  "rb"
                   :pluralize t)

(rails/defresource 'mailer "Mailer"
                   :dir "app/models"
                   :file-ext  "rb"
                   :skip-file-suffix "_mailer"
                   :weight 2)

(rails/defresource 'model-spec "RSpec Model"
                   :dir "spec/models"
                   :file-suffix  "_spec"
                   :file-ext  "rb"
                   :test-to '(model mailer))

(provide 'rails-resources)

;; (setq mig (rails/resources/find 'migration))
;; (setq mod (rails/resources/find 'model))
;; (setq vi (rails/resources/find 'view))
;; ;; (setq a (rails/resources/links-to-of mod))

;; (setq buf (rails/resources/buffer-for-file "" "app/views/commercials/index.html"))
;; (setq ff (rails/resources/associated-buffer-p "z:/apps/admon/" buf vi))


;; (rails/directory-files "z:/apps/admon/" "app/views/permissions")