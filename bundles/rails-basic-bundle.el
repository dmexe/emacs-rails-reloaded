(defun rails/basic-bundle/toggle-from-view (root rails-buffer)
  (when (eq 'view (rails/resource-buffer-type rails-buffer))
    (let ((controllers
           (rails/resources/get-associated-items-by-resource
            root
            rails-buffer
            (rails/resources/find 'controller)))
          (mailers
           (rails/resources/get-associated-items-by-resource
            root
            rails-buffer
            (rails/resources/find 'controller)))
          (method (file-name-sans-extension
                   (file-name-sans-extension
                    (file-name-nondirectory
                     (rails/resource-buffer-file rails-buffer)))))
          item)
      (setq item
            (or (car controllers)
                (car mailers)))
      (when item
        (rails/resources/find-file-by-item root item)
        (rails/ruby/goto-method-in-current-buffer method)
        (rails/resources/notify-item item)
        t))))

(defun rails/basic-bundle/toggle-to-view (root rails-buffer)
  (when (memq (rails/resource-buffer-type rails-buffer)
              '(controller mailer))
    (let* ((method (rails/ruby/current-method))
           (res-name (rails/resource-buffer-title rails-buffer))
           (view (rails/resources/find 'view))
           (dir (concat root (rails/resource-dir view) res-name "/"))
           (regexp (if method
                       (concat "^" method "\..*")
                     ".*"))
           items
           file)
      (setq items
            (files-ext/directory-files-recursive dir regexp))
      (if (= 1 (length items))
          (setq file (car items))
        (setq file
              (ido-completing-read "Select view: "
                                   items)))
      (find-file (concat dir file))
      (rails/resources/notify-item
       nil
       "View"
       (concat res-name "/" file)))
    t))

(rails/defbundle "Basic"
  ()

  (rails/defresource 'controller "Controller"
                     :dir "app/controllers"
                     :file-ext  "rb"
                     :file-suffix "_controller")

  (rails/defresource 'mailer "Mailer"
                     :dir "app/models"
                     :file-ext  "rb"
                     :skip-file-suffix "_mailer"
                     :weight 2)

  (rails/defresource 'helper "Helper"
                     :dir "app/helpers"
                     :file-suffix "_helper"
                     :file-ext  "rb")

  (rails/defresource 'view "View"
                     :dir "app/views"
                     :file-pattern "{name}/.*"
                     :toggle '(rails/basic-bundle/toggle-to-view . rails/basic-bundle/toggle-from-view)
                     :group 'viewa
                     :options 'expand-in-menu
                     :weight 2)

  (rails/defresource 'migration "Migration"
                     :dir "db/migrate"
                     :file-ext  "rb"
                     :file-pattern  "[0-9]+_create_{name}")

  (rails/defresource 'model "Model"
                     :dir "app/models"
                     :file-ext  "rb"
                     :options 'pluralize)

  (rails/defresource 'observer "Observer"
                     :dir "app/models"
                     :file-ext  "rb"
                     :file-suffix "_observer"
                     :weight 2
                     :options 'pluralize)

  (rails/defresource 'stylesheet "Stylesheet"
                     :dir "public/stylesheets"
                     :file-ext  "css")

  (rails/defresource 'javascript "Javascript"
                     :dir "public/javascripts"
                     :file-ext  "js")
 )
