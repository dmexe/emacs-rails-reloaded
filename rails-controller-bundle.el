;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Constants
;;

(defconst rails/controller/dir "app/controllers/")
(defconst rails/controller/file-suffix "_controller")
(defconst rails/controller/buffer-type :controller)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Functions
;;

(defun rails/controller/canonical-name (file)
  (let* ((name (file-name-sans-extension file))
         (name (string-ext/cut name rails/controller/dir :begin))
         (name (string-ext/cut-safe name rails/controller/file-suffix :end)))
    name))

(defun rails/controller/exist-p (root resource-name)
  (let ((file (concat rails/controller/dir
                      (pluralize-string resource-name)
                      rails/controller/file-suffix
                      rails/ruby/file-suffix)))
    (when (rails/file-exist-p root file)
      file)))

(defun rails/controller/controller-p (file)
  (rails/with-root file
    (when-bind (buf (rails/determine-type-of-file (rails/root) (rails/cut-root file)))
      (eq rails/controller/buffer-type (rails/buffer-type buf)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Callbacks
;;

(defun rails/controller/determine-type-of-file (rails-root file)
  (when (string-ext/start-p file rails/controller/dir)
    (let ((name (rails/controller/canonical-name file)))
      (make-rails/buffer :type   rails/controller/buffer-type
                         :name   name
                         :resource-name (pluralize-string name)))))

(defun rails/controller/goto-item-from-file (root file rails-current-buffer)
  (when (rails/resource-type-of-buffer rails-current-buffer
                                       :exclude rails/controller/buffer-type)
     (when-bind (file-name
                 (rails/controller/exist-p root (rails/buffer-resource-name rails-current-buffer)))
       (make-rails/goto-item :name "Controller"
                             :file file-name))))

(defalias 'rails/controller/goto-item-from-rails-buffer
          'rails/controller/goto-item-from-file)

(defun rails/controller/current-buffer-action-name ()
  (rails/ruby/current-method))

(defun rails/controller/goto-action-in-current-buffer (action-name)
  (rails/ruby/goto-method-in-current-buffer action-name))

(defun rails/controller/load ()
  (rails/add-to-resource-types-list rails/controller/buffer-type)
  (rails/define-goto-key "c" 'rails/controller/goto-from-list)
  (rails/define-toggle-key "c" 'rails/controller/goto-current)
  (rails/define-goto-menu "Controller" 'rails/controller/goto-from-list)
  (rails/define-toggle-menu  "Controller" 'rails/controller/goto-current))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Interactives
;;

(defun rails/controller/goto-current ()
  (interactive)
  (rails/with-current-buffer
   (when-bind (goto-item
               (rails/controller/goto-item-from-rails-buffer (rails/root)
                                                             (rails/cut-root (buffer-file-name))
                                                             rails/current-buffer))
     (rails/toggle-file-by-goto-item (rails/root) goto-item))))

(defun rails/controller/goto-from-list ()
  (interactive)
  (rails/with-current-buffer
   (rails/directory-to-goto-menu (rails/root)
                                 rails/controller/dir
                                 "Select a Controller"
                                 :filter-by 'rails/controller/controller-p
                                 :name-by (funcs-chain file-name-sans-extension string-ext/decamelize))))

(provide 'rails-controller-bundle)