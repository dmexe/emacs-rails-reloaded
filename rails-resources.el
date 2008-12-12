(require 'core-ext)
(require 'inflections)
(require 'rails-lib)

(defstruct rails/resource type ;; symbol, required
                          display-name ;; string, required
                          menu-group ;; string required
                          boundle-name ;; string
                          dir file-ext file-suffix skip-file-suffix ;; string
                          pluralize ;; boolean
                          resource-name-func resource-files-func ;; commandp
                          link-to test-to ;; list
                          get-action-func set-action-func ;; commandp
                          (weight 1)) ;; integer required

(defstruct rails/resource-buffer type
                                 resource-name
                                 file-name
                                 get-action-func
                                 set-action-func
                                 (weight 1)) ;; must be integer

(defstruct rails/resource-item file
                               display-name
                               resource-type
                               resource-menu-group
                               resource-display-name)

(defvar rails/resources/list-defined nil)

(defun* rails/defresource (type display-name &key menu-group bundle-name
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
                                   :link-to (if (listp link-to) link-to (list link-to))
                                   :test-to (if (listp test-to) test-to (list test-to))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Lookup resource-buffer for file
;;

(defun rails/resources/get-buffer-by-resource-for-file (resource file-name)
  (let ((file file-name))
    (catch 'result
      ;; dir
      (when-bind (dir (rails/resource-dir resource))
        (unless (string-ext/start-p file dir) (throw 'result nil))
        (setq file (string-ext/cut file dir :begin))
        (setq file-in-dir file))
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
                                  :file-name file-name
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Lookup associated resources by resource-buffer
;;

(defun rails/resources/resource-files-to-items (resource files)
  (unless (listp files)
    (setq files (list files)))
  (loop for it in files collect
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
                                    :resource-display-name (rails/resource-display-name resource)))))

(defun rails/resources/filter-buffer-in-items (rails-buffer items)
  (let ((compared-file (rails/resource-buffer-file-name rails-buffer)))
    (loop for item in items
          for allow = (not (string= compared-file (rails/resource-item-file item)))
          when allow
          collect item)))

(defun rails/resources/filter-dublicated-files-in-items (items)
  (let (result deleted)
    (dolist (res-items items)
      (dolist (it res-items)
        (let* ((file (rails/resource-item-file it))
               (exist (find file result :test 'string= :key 'car)))
          (if exist
              (let* ((it-exist (cdr exist))
                     (res  (rails/resources/find (rails/resource-item-resource-type it)))
                     (eres (rails/resources/find (rails/resource-item-resource-type it-exist)))
                     (w    (rails/resource-weight res))
                     (ew   (rails/resource-weight eres)))
                (if (> w ew) ;; if weight if geater that exist weight
                    (progn
                      (delete* file result :test 'string= :key 'car) ;; remove from alist
                      (add-to-list 'deleted it-exist)                ;; place item to deleted
                      (add-to-list 'result (cons file it)))          ;; append a new item
                  (add-to-list 'deleted it)))
            (add-to-list 'result (cons file it)))))) ;; save files alist
    (setq result nil)
    (dolist (res-items items)
      (let ((its res-items))
        (dolist (del deleted)
          (setq its (remove* del its :test 'equal)))
        (when its
          (add-to-list 'result its t))))
    result))

(defun rails/resources/get-associated-items-by-resource (root rails-buffer resource)
  (let ((file (rails/resource-buffer-resource-name rails-buffer))
        (file-func (rails/resource-resource-files-func resource))
        result name)
    (setq result
          (if file-func
              ;; resource-files-func
              (apply file-func (list root file rails-buffer resource))
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
                (list (cons name file))))))
    (rails/resources/filter-buffer-in-items
     rails-buffer
     (rails/resources/resource-files-to-items resource result))))

(defun rails/resources/get-associated-items(root rails-buffer)
  (rails/resources/filter-dublicated-files-in-items
   (loop for res in rails/resources/list-defined
         for items = (rails/resources/get-associated-items-by-resource root rails-buffer res)
         when items
         collect items)))

(defun rails/resources/get-associated-resources(root rails-buffer)
  (loop for res in rails/resources/list-defined
        for items = (rails/resources/get-associated-items-by-resource root rails-buffer res)
        when items
        collect res))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Menu functions
;;

(defun rails/resources/items-to-menu (menu items &optional display-resource-name)
  (let ((menu menu)
        func)
    (setq func
          (if display-resource-name
              'rails/resource-item-resource-display-name
            'rails/resource-item-display-name))
    (dolist (it items)
      (add-to-list 'menu (cons (apply func (list it)) it) t))
    menu))

(defun rails/resources/find-file-by-item (root item)
  (when (rails/resource-item-p item)
    (rails/find-file root (rails/resource-item-file item))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Lookup resource by link
;;

(defun rails/resources/linked-to-items-of-buffer(root rails-buffer)
  (let* ((type (rails/resource-buffer-type rails-buffer))
         (resource (rails/resources/find type))
         (link-to (rails/resource-link-to resource)))
    (loop for name in link-to
          with max-w   = 0
          with max-its = nil
          for res = (rails/resources/find name)
          for w   = (rails/resource-weight res)
          do (when (> w max-w)
               (when-bind (its (rails/resources/get-associated-items-by-resource root rails-buffer res))
                 (setf max-its its
                       max-w w)))
          finally (return max-its))))

(defun rails/resources/linked-items-of-buffer(root rails-buffer)
  (let* ((type (rails/resource-buffer-type rails-buffer))
         (resource (rails/resources/find type)))
    (loop for res in rails/resources/list-defined
          with max-w   = 0
          with max-its = nil
          for defined = (memq type (rails/resource-link-to res))
          for w       = (rails/resource-weight res)
          do (when (and defined (> w max-w))
               (when-bind (its (rails/resources/get-associated-items-by-resource root rails-buffer res))
                 (setf max-its its
                       max-w w)))
          finally (return max-its))))

(defun rails/resources/toggle-by-link (&optional force-ido)
  (interactive)
  (let* ((root (rails/root))
         (rails-buffer (rails/resources/get-buffer-for-file root (rails/cut-root (buffer-file-name))))
         (resource (rails/resources/find (rails/resource-buffer-type rails-buffer)))
         items menu file)
    (setq items
          (if (rails/resource-link-to resource)
              (rails/resources/linked-to-items-of-buffer root rails-buffer)
            (rails/resources/linked-items-of-buffer root rails-buffer)))
    (setq menu (rails/resources/items-to-menu menu items))
    (setq file (if (< 1 (length menu))
                   (rails/display-menu "Select" menu force-ido)
                 (cdr (car menu))))
    (rails/resources/find-file-by-item root file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Menu for associated resources
;;

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
            (setq menu (rails/resources/items-to-menu menu (car group-items)))
          (dolist (i group-items)
            (add-to-list 'menu (cons (rails/resource-item-resource-display-name (car i)) (car i)) t))))
        (unless last-p
          (setq last-p t)))
    (setq resource  (rails/resources/find (rails/resource-buffer-type rails-buffer)))
    (setq file (rails/display-menu-using-popup
                (format "Go to from %s to:" (rails/resource-display-name resource))
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
                                            (rails/resources/items-to-menu menu items)))
      (setq file (car items)))
    (rails/resources/find-file-by-item root file)
    file))

(defun rails/resources/goto-associated (&optional force-ido)
  (interactive)
  (let* ((root (rails/root))
         (rails-buffer (rails/resources/get-buffer-for-file root (rails/cut-root (buffer-file-name)))))
    (if force-ido
        (rails/resources/goto-associated-using-ido root rails-buffer)
      (rails/resources/goto-associated-using-menu root rails-buffer))))


;; (rails/resources/delete 'view)
(rails/resources/clear)

(rails/defresource 'controller "Controller"
                   :dir "app/controllers"
                   :file-ext  "rb"
                   :resource-name-func '(lambda(file) (string-ext/string=~
                                                  "^\\(application\\|\\(.*\\)_controller\\)$" file (or $2 $1)))
                   :resource-files-func '(lambda(root name buffer resource)
                                          (rails/directory-files root
                                                                 "app/controllers" nil
                                                                 (format "^%s_controller\\.rb$" name))))
(rails/defresource 'mailer "Mailer"
                   :dir "app/models"
                   :file-ext  "rb"
                   :skip-file-suffix "_mailer"
                   :weight 2)

(rails/defresource 'helper "Helper"
                   :dir "app/helpers"
                   :file-suffix "_helper"
                   :file-ext  "rb"
                   :link-to 'controller)

(rails/defresource 'view "View"
                   :dir "app/views"
                   :menu-group 'view
                   :resource-name-func '(lambda(file) (string-ext/cut (file-name-directory file) "/" :end))
                   :resource-files-func '(lambda(root name buffer resource)
                                          (mapcar '(lambda(file)
                                                     (cons file (concat name "/" file)))
                                                  (rails/directory-files root (format "app/views/%s" name))))
                   :get-action-func '(lambda() (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))
                   :link-to '(mailer controller)
                   :weight 2)

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

