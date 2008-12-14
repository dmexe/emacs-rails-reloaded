(require 'cl)

(defvar rails/bundles/disabled-list nil)
(defvar rails/bundles/loaded-list nil)
(defvar rails/bundles/loaded-p nil)

(defconst rails/bundles/file-regexp "^rails-\\(.*\\)-bundle\.el$")
(defconst rails/bundles/name-fmt "rails/%s-bundle/name")

(defun rails/bundles/enabled-p (title)
  (not (memq (string-ext/safe-symbol title) rails/bundles/disabled-list)))

(defun rails/bundles/toggle-enabled (title)
  (let ((sym (string-ext/safe-symbol title)))
    (if (rails/bundles/enabled-p title)
        (progn
          (add-to-list 'rails/bundles/disabled-list sym nil 'eq)
          nil)
      (setq rails/bundles/disabled-list (delete sym rails/bundles/disabled-list))
      t)))

(defun rails/bundles/load ()
  (unless rails/bundles/loaded-p
    (let* ((bdir (concat
                  (file-name-directory (locate-library "rails-reloaded"))
                  "newbundles"))
           (files (directory-files bdir nil rails/bundles/file-regexp)))
      (dolist (file files)
        (load (concat bdir "/" (file-name-sans-extension file)))
        (let* ((sym (intern
                     (string-ext/string=~ rails/bundles/file-regexp file $1)))
               (title (symbol-value
                       (intern (format rails/bundles/name-fmt sym)))))
          (add-to-list 'rails/bundles/loaded-list (cons sym title))
          (rails/bundles/add-to-loaded-menu title))))
    (setq rails/bundles/loaded-p t)))

(defun rails/bundles/reload ()
  (interactive)
  (setq rails/bundles/loaded-list nil)
  (setq rails/bundles/loaded-p nil)
  (rails/resources/clear)
  (rails-minor-mode-reset-keymap)
  (rails/bundles/load))

(defun rails/bundles/add-to-loaded-menu (title)
  (unless (lookup-key rails-minor-mode-map
                      [menu-bar rails bundles-loaded])
    (define-key-after rails-minor-mode-map
      [menu-bar rails bundles-loaded]
      (cons "Loaded Bundles" (make-sparse-keymap))
      'bundles-title))
  (define-key rails-minor-mode-map
    (merge 'vector [menu-bar rails bundles-loaded]
           (list (string-ext/safe-symbol title))
           'eq)
    (list 'menu-item title
          `(lambda()
             (interactive)
             (rails/bundles/toggle-enabled ,title)
             (rails/bundles/reload))
          :button (cons :toggle (rails/bundles/enabled-p title)))))

(defun rails/bundles/add-to-bundles-menu (title menumap)
  (define-key-after rails-minor-mode-map
    (merge 'vector [menu-bar rails] (list (string-ext/safe-symbol title)) 'eq)
    (cons (concat title " Bundle") menumap)
    'bundles-title))

(defmacro* rails/defbundle (name (&key menu) &body body)
  `(progn
     (defconst
       ,(intern (format rails/bundles/name-fmt (string-ext/safe-symbol name)))
       ,name)
     (when (rails/bundles/enabled-p ,name)
       (when ,(not (not menu))
         (rails/bundles/add-to-bundles-menu
          ,name
          (let ((map (make-sparse-keymap)))
            (define-keys map
              ,@menu))))
       ,@body)))

(provide 'rails-bundles)