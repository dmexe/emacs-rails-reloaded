;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Constants
;;

(defconst rails/view/dir "app/views/")
(defconst rails/view/excluded-dir '("layouts"))
(defconst rails/view/fast-goto-item-weight 1)
(defconst rails/view/buffer-weight 1)
(defconst rails/view/buffer-type :view)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Variables
;;

(defvar rails/view/templates-list
  '("html.erb" "js.rjs" "xml.builder" "erb" "builder" "haml" "liquid" "mab"))

(defvar rails/view/templates-history-list nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Functions
;;

(defun rails/view/resource-name (file)
  (let* ((name (string-ext/cut file rails/view/dir :begin))
         (name (file-name-directory name))
         (name (string-ext/cut name "/" :end)))
    name))

(defun rails/view/exist-p (root views-name)
  (let ((file (concat rails/view/dir
                      (pluralize-string views-name))))
    (when (rails/file-exist-p root file)
      file)))

(defun rails/view/files-for-action (root views-name action-name)
  (when-bind (files (rails/view/files root views-name))
    (let ((mask (concat action-name "."))
          res)
      (dolist (file files)
        (when (string-ext/start-p (file-name-nondirectory (rails/goto-item-file file)) mask)
          (add-to-list 'res file t)))
      res)))

(defun rails/view/excluded-dir-p (file)
  (let (res)
    (dolist (dir rails/view/excluded-dir)
      (when (string-ext/start-p
             file
             (concat rails/view/dir dir "/"))
        (setq res t)))
    res))

(defun rails/view/files (root views-name)
  (let* ((path (concat rails/view/dir (pluralize-string views-name) "/"))
         (rpath (concat root path))
         (files (directory-files rpath))
         res)
    (dolist (file files)
      (when (not (files-ext/file-special-p file))
        (add-to-list 'res (make-rails/goto-item :group :view
                                                :name (rails/view/decorate-file-name file)
                                                :file (concat path file))
                     t)))
    res))

(defun rails/view/file-to-action-name (file)
  (let ((file (file-name-nondirectory file)))
    (when-bind (action-name (car (split-string file "\\.")))
      (unless (string-ext/start-p action-name "_")
        action-name))))

(defun rails/view/decorate-file-name (file)
  (let ((file (file-name-nondirectory file)))
    (if (string-ext/start-p file "_" )
        (format "Partial: %s" (string-ext/cut file "_" :begin))
      (format "View: %s" file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Callbacks
;;

(defun rails/view/goto-item-from-file (root file rails-current-buffer)
  (when-bind (type (rails/resource-type-p rails-current-buffer nil))
     (when-bind (file-name
                 (rails/view/exist-p
                  root (rails/buffer-views-name rails-current-buffer)))
       (let ((files
              (rails/view/files
               root (rails/buffer-views-name rails-current-buffer))))
         files))))

(defun rails/view/determine-type-of-file (rails-root file)
  (when (and (string-ext/start-p file rails/view/dir)
             (not (rails/view/excluded-dir-p file)))
    (let ((name (rails/view/resource-name file)))
      (make-rails/buffer :type   rails/view/buffer-type
                         :weight rails/view/buffer-weight
                         :name   (format "%s#%s" name (file-name-nondirectory file))
                         :resource-name name))))

(defun rails/view/fast-goto-item-from-file (root file rails-current-buffer)
  (when (rails/resource-type-p rails-current-buffer rails/view/buffer-type)
    (when-bind (action-name (rails/current-buffer-action-name))
      (when-bind (views-name (rails/buffer-views-name rails-current-buffer))
        (when (rails/view/exist-p root views-name)
          (let ((items (rails/view/files-for-action root views-name action-name)))
            (case (length items)
              (1
               (rails/find-file-by-goto-item root (car items)))
              (0
               (rails/view/create-view-for-current-buffer))
              (t
               (let ((create-item (make-rails/goto-item :group :new-view
                                                        :name "Create a new view"
                                                        :func 'rails/view/create-view-for-current-buffer)))
                 (rails/menu-from-goto-item-alist root
                                                  "Select file..."
                                                  (list (list :view items)
                                                        (list :new-view (list create-item)))))))))))))

(defun rails/view/current-buffer-action-name ()
  (when (and (rails/buffer-p rails/current-buffer)
             (rails/buffer-file rails/current-buffer)
             (eq (rails/buffer-type rails/current-buffer) rails/view/buffer-type))
    (rails/view/file-to-action-name (rails/buffer-file rails/current-buffer))))

(defun rails/view/load ()
  (rails/add-to-resource-types-list rails/view/buffer-type)
  (rails/add-to-layouts-list :controller rails/view/buffer-type)

  (let ((map (make-sparse-keymap)))
    (define-keys map
      ([new] (cons "Create a new view for current file" 'rails/view/create-view-for-current-buffer)))
    (rails/add-to-bundles-menu "View" map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Interactives
;;

(defun rails/view/create-view-for-current-buffer (&optional goto-item)
  (interactive "p")
  (when (and (rails/buffer-p rails/current-buffer)
             (rails/resource-type-p rails/current-buffer)
             (rails/buffer-views-name rails/current-buffer))
    (let* ((action-name (rails/current-buffer-action-name))
           (views-name (pluralize-string (rails/buffer-views-name rails/current-buffer)))
           (path (concat (rails/root) rails/view/dir views-name "/")))
      (when (rails/view/exist-p (rails/root) views-name)
        (if (or (not action-name)
                (integerp goto-item)) ; called interactive
            (let ((name (completing-read
                         (format "Create %s view: " (string-ext/decamelize views-name))
                         nil)))
              (unless (string-ext/empty-p name)
                (find-file (format "%s/%s" path name))))
          (let ((name (completing-read
                       (format "Create view %s#%s."
                               (string-ext/decamelize views-name) action-name)
                       rails/view/templates-list nil nil
                       (or (car rails/view/templates-history-list)
                           (car rails/view/templates-list))
                       'rails/view/templates-history-list
                       (car rails/view/templates-list))))
            (unless (string-ext/empty-p name)
              (find-file (format "%s%s.%s" path action-name name)))))))))

(provide 'rails-view-bundle)