(rails/defbundle "Basic"
  (:menu
   (([new] (cons "Create a new view for current file" 'identity))
    ([new2] (cons "Create a new view for current file" 'identity))))

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
                     :expand-in-menu t
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

  (rails/defresource 'observer "Observer"
                     :dir "app/models"
                     :file-ext  "rb"
                     :file-suffix "_observer"
                     :weight 2
                     :link-to 'model
                     :pluralize t)

  (rails/defresource 'stylesheet "Stylesheet"
                     :dir "public/stylesheets"
                     :file-ext  "css")

  (rails/defresource 'javascript "Javascript"
                     :dir "public/javascripts"
                     :file-ext  "css")
 )
