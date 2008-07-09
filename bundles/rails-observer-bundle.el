;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Constants
;;

(defconst rails/observer/dir "app/models/")
(defconst rails/observer/file-suffix "_observer")
(defconst rails/observer/buffer-type :observer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Functions
;;

(defun rails/observer/canonical-name (file)
  (let* ((name (file-name-sans-extension file))
         (name (string-ext/cut name rails/observer/dir :begin))
         (name (string-ext/cut name rails/observer/file-suffix :end)))
    name))

(defun rails/observer/exist-p (root resource-name)
  (when resource-name
    (let ((file (concat rails/observer/dir
                        (singularize-string resource-name)
                        rails/observer/file-suffix
                        rails/ruby/file-suffix)))
      (when (and (rails/file-exist-p root file)
                 (rails/observer/observer-p file))
        file))))

(defun rails/observer/observer-p (file)
  (rails/with-root file
    (let ((file (rails/cut-root file)))
      (and (string-ext/start-p file rails/observer/dir)
           (string-ext/end-p (file-name-sans-extension file) rails/observer/file-suffix)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Callbacks
;;

(defun rails/observer/determine-type-of-file (rails-root file)
  (when (rails/observer/observer-p file)
    (let ((name (rails/observer/canonical-name file)))
      (make-rails/buffer :type   rails/observer/buffer-type
                         :name   name
                         :resource-name (pluralize-string name)))))

(defun rails/observer/goto-item-from-file (root file rails-current-buffer)
  (when (rails/resource-type-of-buffer rails-current-buffer
                                       :exclude rails/observer/buffer-type)
    (when-bind (file-name
                (rails/observer/exist-p root (rails/buffer-resource-name rails-current-buffer)))
      (make-rails/goto-item :name "Observer" :file file-name))))

(defalias 'rails/observer/goto-item-from-rails-buffer
          'rails/observer/goto-item-from-file)

(defun rails/observer/load ()
  (rails/add-to-resource-types-list rails/observer/buffer-type)
  (rails/define-goto-key "o" 'rails/observer/goto-from-list)
  (rails/define-goto-menu  "Observer" 'rails/observer/goto-from-list)
  (rails/define-toggle-key "o" 'rails/observer/goto-current)
  (rails/define-toggle-menu  "Observer" 'rails/observer/goto-current))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Interactives
;;

(defun rails/observer/goto-from-list ()
  (interactive)
  (when-bind (root (rails/root))
   (rails/directory-to-goto-menu root
                                 rails/observer/dir
                                 "Select a Observer"
                                 :filter-by 'rails/observer/observer-p
                                 :name-by (funcs-chain file-name-sans-extension string-ext/decamelize))))

(defun rails/observer/goto-current ()
  (interactive)
  (rails/with-current-buffer
   (when-bind (goto-item
               (rails/observer/goto-item-from-rails-buffer (rails/root)
                                                        (rails/cut-root (buffer-file-name))
                                                        rails/current-buffer))
     (rails/toggle-file-by-goto-item (rails/root) goto-item))))

(provide 'rails-observer-bundle)