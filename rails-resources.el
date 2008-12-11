(require 'core-ext)
(require 'inflections)
(require 'rails-lib)

(defstruct rails/resource type display-name
                          menu-group
                          dir file-ext file-suffix skip-file-suffix
                          pluralize resource-name-func resource-files-func
                          link-to test-to
                          get-action-func
                          set-action-func
                          (weight 1))

(defstruct rails/resource-buffer type
                                 resource-name
                                 get-action-func
                                 set-action-func
                                 weight)

(defstruct rails/resource-item file
                               display-name
                               resource-type
                               resource-menu-group
                               resource-display-name)

(defvar rails/resources/list-defined nil)

(defun* rails/defresource (type display-name &key menu-group
                                          dir file-suffix file-ext
                                          skip-file-suffix
                                          pluralize resource-name-func resource-files-func
                                          link-to test-to
                                          get-action-func set-action-func
                                          weight)
  (when (rails/resources/find type)
    (error (format "Resource %s already defined" type)))
  (let (res)
    (setq res (make-rails/resource :type type
                                   :menu-group (if menu-group (symbol-name menu-group) "acore")
                                   :display-name display-name
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
                                   :resource-files-func resource-files-func
                                   :get-action-func get-action-func
                                   :set-action-func set-action-func
                                   :weight (if (not weight) 1 weight)))
    (add-to-list 'rails/resources/list-defined res t)))

(defun rails/resources/find (resource-type)
  (when (stringp resource-type)
    (setq resource-type (intern resource-type)))
  (find resource-type rails/resources/list-defined :key 'rails/resource-type))

(defun rails/resources/delete (resource-type)
  (setq
   rails/resources/list-defined
   (remove* resource-type rails/resources/list-defined :key 'rails/resource-type)))

(defun rails/resources/clear ()
  (setq rails/resources/list-defined nil))

(defun rails/resources/get-buffer-by-resource-for-file (resource file-name)
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
      (make-rails/resource-buffer :type (rails/resource-type resource)
                                  :resource-name file
                                  :weight (rails/resource-weight resource)
                                  :get-action-func (rails/resource-get-action-func resource)
                                  :set-action-func (rails/resource-set-action-func resource)))))

(defun rails/resources/get-buffer-for-file(root file)
  (let (resources)
    (setq resources
          (loop for res in rails/resources/list-defined
                for allow = (rails/resources/get-buffer-by-resource-for-file res file)
                when allow
                collect allow))
    (when (listp resources)
      (car (sort* resources '> :key 'rails/resource-buffer-weight)))))

(defun rails/resources/files-to-items (resource files)
  (unless (listp files)
    (setq files (list files)))
  (mapcar '(lambda(it)
             (let ((name it)
                   (file it)
                   (dir (rails/resource-dir resource)))
               (when (consp it)
                 (setq name (car it)
                       file (cdr it)))
               (unless (string-ext/start-p file dir)
                 (setq file (concat dir file)))
               (make-rails/resource-item :display-name name
                                         :file file
                                         :resource-type (rails/resource-type resource)
                                         :resource-menu-group (rails/resource-menu-group resource)
                                         :resource-display-name (rails/resource-display-name resource))))
          files))

(defun rails/resources/get-associated-items-by-resource (root buffer resource)
  (let ((file (rails/resource-buffer-resource-name buffer))
        (file-func (rails/resource-resource-files-func resource))
        result name)
    (setq result
          (if file-func
              ;; resource-files-func
              (rails/resources/files-to-items
               resource
               (apply file-func (list root file buffer resource)))
            (progn
              ;; singularize
              (when-bind (pluralize (rails/resource-pluralize resource))
                (setq file (singularize-string file)))
              (setq name file)
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
                (rails/resources/files-to-items resource (list (cons name file)))))))))

(defun rails/resources/get-associated-items(root rails-buffer)
  (let (resources)
    (setq resources
          (loop for res in rails/resources/list-defined
                for allow = (rails/resources/get-associated-items-by-resource root rails-buffer res)
                when allow
                collect allow))))

(defun rails/resources/get-associated-resources(root rails-buffer)
  (let (resources)
    (setq resources
          (loop for res in rails/resources/list-defined
                for allow = (rails/resources/get-associated-items-by-resource root rails-buffer res)
                when allow
                collect res))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rails/resources/goto-associated (&optional force-ido)
  (interactive)
  (let* ((root (rails/root))
         (rails-buffer (rails/resources/get-buffer-for-file root (rails/cut-root (buffer-file-name)))))
    (if force-ido
        (rails/resources/goto-associated-using-ido root rails-buffer)
      (rails/resources/goto-associated-using-menu root rails-buffer))))


(defun rails/resources/goto-associated-using-menu (root rails-buffer)
  (let (items menu last-p file resource)
    (setq items
          (rails/resources/get-associated-items root rails-buffer))
    (setq items
          (list-ext/group-by items
                             '(lambda(i) (rails/resource-item-resource-menu-group (car i)))
                             'string<))
    (dolist (group items)
      (let ((group-name (car group))
            (group-items (cadr group)))
        (when last-p
          (add-to-list 'menu (list "--" "--") t))
        (if (and (= 1 (length group-items))
                 (< 1 (length (car group-items))))
            (dolist (i (car group-items))
              (add-to-list 'menu (cons (rails/resource-item-display-name i) i) t))
          (dolist (i group-items)
            (add-to-list 'menu (cons (rails/resource-item-resource-display-name (car i)) (car i)) t))))
        (unless last-p
          (setq last-p t)))
    (setq resource  (rails/resources/find (rails/resource-buffer-type rails-buffer)))
    (setq file (rails/display-menu-using-popup
                (format "Go to from %s to" (rails/resource-display-name resource))
                menu))
    (rails/resources/find-file-by-item root file)
    file))

(defun rails/resources/goto-associated-using-ido (root rails-buffer)
  (let (items resource file)
    (setq resource
          (rails/display-menu-using-ido "Go to"
                                        (mapcar '(lambda(i) (cons (symbol-name (rails/resource-type i)) i))
                                                (rails/resources/get-associated-resources root rails-buffer))))
    (setq items
          (rails/resources/get-associated-items-by-resource root rails-buffer resource))
    (if (< 1 (length items))
        (setq file
              (rails/display-menu-using-ido (format "%s" (rails/resource-display-name resource))
                                            (mapcar '(lambda(i) (cons (rails/resource-item-display-name i) i)) items)))
      (setq file (car items)))
    (rails/resources/find-file-by-item root file)
    file))

(defun rails/resources/find-file-by-item (root item)
  (when (rails/resource-item-p item)
    (rails/find-file root (rails/resource-item-file item))))

;; (with-current-buffer "asset.rb"
;;   (setq test2 (rails/resources/goto-associated)))

;; test

;;;   (let (resource)
;;;     (if resource-type
;;;         (setq resource (rails/resources/find resource-type))
;;;       (let ((resource-types
;;;              (mapcar '(lambda(res) (cons (rails/resource-display-name res) res))
;;;                      (rails/resources/get-associated-resources root rails-buffer))))
;;;         (setq resource (rails/display-menu-using-popup "Select?: " resource-types))))

;;;     (let ((items (rails/resources/get-associated-items-by-resource root
;;;                                                                    rails-buffer
;;;                                                                    resource)))
;;;       (unless (= 1 (length items))
;;;         (setq items (rails/display-menu-using-popup
;;;                      (format "Select a %s: " (rails/resource-display-name resource))
;;;                      (mapcar '(lambda(i) (cons (rails/resource-item-display-name i) i))
;;;                              items))))
;;;       (message "%S" items))))

;; (rails/resources/delete 'view)
(rails/resources/clear)

(rails/defresource 'view "View"
                   :dir "app/views"
                   :menu-group 'view
                   :resource-name-func '(lambda(file) (string-ext/cut (file-name-directory file) "/" :end))
                   :resource-files-func '(lambda(root name buffer resource)
                                          (mapcar '(lambda(file)
                                                     (cons file (concat name "/" file)))
                                                  (rails/directory-files root (format "app/views/%s" name))))
                   :get-action-func '(lambda() (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))
                   :link-to '(mailer controller))

(rails/defresource 'migration "Migration"
                   :dir "db/migrate"
                   :file-ext  "rb"
                   :resource-name-func '(lambda(file) (string-ext/string=~ "^[0-9]+_create_\\(.*\\)" file $1))
                   :resource-files-func '(lambda(root name buffer resource)
                                          (rails/directory-files root
                                                                 "db/migrate" nil
                                                                 (format "^[0-9]+_create_%s\\.rb$" name)))
                   :link-to 'model)

(rails/defresource 'model "Model"
                   :dir "app/models"
                   :file-ext  "rb"
                   :pluralize t)

(rails/defresource 'controller "Controller"
                   :dir "app/controllers"
                   :file-ext  "rb"
                   :resource-name-func '(lambda(file) (string-ext/string=~
                                                  "^\\(application\\|\\(.*\\)_controller\\)$" file (or $2 $1)))
                   :resource-files-func '(lambda(root name buffer resource)
                                          (rails/directory-files root
                                                                 "app/controllers" nil
                                                                 (format "^%s_controller\\.rb$" name))))

(rails/defresource 'helper "Helper"
                   :dir "app/helpers"
                   :file-suffix "_helper"
                   :file-ext  "rb"
                   :link-to 'controller)

(rails/defresource 'mailer "Mailer"
                   :dir "app/models"
                   :file-ext  "rb"
                   :skip-file-suffix "_mailer"
                   :weight 2)

(rails/defresource 'model-spec "RSpec Model"
                   :menu-group 'spec
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

