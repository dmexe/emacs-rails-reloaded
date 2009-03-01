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
                     :group 'view
                     :file-pattern "{name}/.*"
                     :get-action-func '(lambda() (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))
                     :options 'expand-in-menu
                     :link-to '(mailer controller)
                     :weight 2)

  (rails/defresource 'migration "Migration"
                     :dir "db/migrate"
                     :file-ext  "rb"
                     :file-pattern  "[0-9]+_create_{name}"
                     :link-to 'model)

  (rails/defresource 'model "Model"
                     :dir "app/models"
                     :file-ext  "rb"
                     :options 'pluralize)

  (rails/defresource 'observer "Observer"
                     :dir "app/models"
                     :file-ext  "rb"
                     :file-suffix "_observer"
                     :weight 2
                     :link-to 'model
                     :options 'pluralize)

  (rails/defresource 'stylesheet "Stylesheet"
                     :dir "public/stylesheets"
                     :file-ext  "css")

  (rails/defresource 'javascript "Javascript"
                     :dir "public/javascripts"
                     :file-ext  "js")
 )
