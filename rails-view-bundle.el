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

(defun rails/view/association-name (file)
  (let* ((name (string-ext/cut file rails/view/dir :begin))
         (name (files-ext/directory-of-file name))
         (name (string-ext/cut name "/" :end)))
    name))

(defun rails/view/exist-p (root association-name)
  (let ((file (concat rails/view/dir
                      (pluralize-string association-name))))
    (when (rails/file-exist-p root file)
      file)))

(defun rails/view/files-for-action (root association-name action-name)
  (when-bind (files (rails/view/files root association-name))
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

(defun rails/view/files (root association-name)
  (let* ((path (concat rails/view/dir (pluralize-string association-name) "/"))
         (rpath (concat root path))
         (files (directory-files rpath))
         res)
    (dolist (file files)
      (when (not (files-ext/file-special-p file))
        (add-to-list 'res (make-rails/goto-item :group :view
                                                :name file
                                                :file (concat path file))
                     t)))
    res))

(defun rails/view/file-to-action-name (file)
  (let ((file (file-name-nondirectory file)))
    (when-bind (action-name (car (split-string file "\\.")))
      (unless (string-ext/start-p action-name "_")
        action-name))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Callbacks
;;

(defun rails/view/goto-item-from-file (root file rails-current-buffer)
  (when-bind (type (rails/associated-type-p rails-current-buffer nil))
     (when-bind (file-name
                 (rails/view/exist-p root (rails/buffer-association-name rails-current-buffer)))
       (rails/view/files root (rails/buffer-association-name rails-current-buffer)))))

(defun rails/view/determine-type-of-file (rails-root file)
  (when (and (string-ext/start-p file rails/view/dir)
             (not (rails/view/excluded-dir-p file)))
    (let ((name (rails/view/association-name file)))
      (make-rails/buffer :type   rails/view/buffer-type
                         :weight rails/view/buffer-weight
                         :name   (format "%s#%s" name (file-name-nondirectory file))
                         :association-name name))))

(defun rails/view/fast-goto-item-from-file (root file rails-current-buffer)
  (when (rails/associated-type-p rails-current-buffer rails/view/buffer-type)
    (when-bind (action-name (rails/current-buffer-action-name))
      (when-bind (association-name (rails/buffer-association-name rails-current-buffer))
        (when (rails/view/exist-p root association-name)
          (let ((items (rails/view/files-for-action root association-name action-name)))
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

;; (defun rails/helper/initialize (root file rails-current-buffer)
;;   )

(defun rails/view/load ()
  (rails/add-to-associated-types-list rails/view/buffer-type))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Interactives
;;

(defun rails/view/create-view-for-current-buffer (&optional goto-item)
  (interactive)
  (when (and (rails/buffer-p rails/current-buffer)
             (rails/associated-type-p rails/current-buffer nil))
    (when-bind (action-name (rails/current-buffer-action-name))
      (when-bind (association-name (rails/buffer-association-name rails/current-buffer))
        (when (rails/view/exist-p (rails/root) association-name)
          (let ((association-name (pluralize-string association-name))
                (path (concat (rails/root)
                              rails/view/dir
                              association-name "/")))
            (let ((name (completing-read
                         (format "Create view %s#%s."
                                 (string-ext/decamelize association-name) action-name)
                         rails/view/templates-list
                         nil
                         nil
                         (or (car rails/view/templates-history-list)
                             (car rails/view/templates-list))
                         'rails/view/templates-history-list
                         (car rails/view/templates-list))))
              (when (not (string-ext/empty-p name))
                (find-file (format "%s%s.%s" path action-name name))))))))))

(provide 'rails-view-bundle)