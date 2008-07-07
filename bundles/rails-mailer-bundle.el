;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Constants
;;

(defconst rails/mailer/dir "app/models/")
(defconst rails/mailer/file-suffix "_mailer")
(defconst rails/mailer/buffer-type :mailer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Functions
;;

(defun rails/mailer/canonical-name (file)
  (let* ((name (file-name-sans-extension file))
         (name (string-ext/cut name rails/mailer/dir :begin)))
    name))

(defun rails/mailer/exist-p (root resource-name)
  (when resource-name
    (let ((file (concat rails/mailer/dir
                        (singularize-string resource-name)
                        rails/ruby/file-suffix)))
      (when (and (rails/file-exist-p root file)
                 (rails/mailer/mailer-p file))
        file))))

(defun rails/mailer/mailer-p (file)
  (rails/with-root file
    (let ((file (rails/cut-root file)))
      (and (string-ext/start-p file rails/mailer/dir)
           (string-ext/end-p (file-name-sans-extension file) rails/mailer/file-suffix)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Callbacks
;;

(defun rails/mailer/determine-type-of-file (rails-root file)
  (when (rails/mailer/mailer-p (concat rails-root file))
    (let ((name (rails/mailer/canonical-name file)))
      (make-rails/buffer :type   rails/mailer/buffer-type
                         :name   name
                         :resource-name (singularize-string name)))))

(defun rails/mailer/goto-item-from-file (root file rails-current-buffer)
  (when (rails/resource-type-of-buffer rails-current-buffer
                                       :exclude rails/mailer/buffer-type)
    (when-bind (file-name
                (rails/mailer/exist-p root (rails/buffer-resource-name rails-current-buffer)))
      (make-rails/goto-item :name "Mailer" :file file-name))))

(defalias 'rails/mailer/goto-item-from-rails-buffer
          'rails/mailer/goto-item-from-file)

(defalias 'rails/mailer/current-buffer-action-name
          'rails/ruby/current-method)

(defalias 'rails/mailer/goto-action-in-current-buffer
          'rails/ruby/goto-method-in-current-buffer)

(defun rails/mailer/load ()
  (rails/add-to-resource-types-list rails/mailer/buffer-type)
  (rails/define-goto-key "n" 'rails/mailer/goto-from-list)
  (rails/define-goto-menu  "Mailer" 'rails/mailer/goto-from-list)
  (rails/define-toggle-key "n" 'rails/mailer/goto-current)
  (rails/define-toggle-menu  "Mailer" 'rails/mailer/goto-current))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Interactives
;;

(defun rails/mailer/goto-from-list ()
  (interactive)
  (rails/with-current-buffer
   (rails/directory-to-goto-menu (rails/root)
                                 rails/mailer/dir
                                 "Select a Mailer"
                                 :filter-by 'rails/mailer/mailer-p
                                 :name-by (funcs-chain file-name-sans-extension string-ext/decamelize))))

(defun rails/mailer/goto-current ()
  (interactive)
  (rails/with-current-buffer
   (when-bind (goto-item
               (rails/mailer/goto-item-from-rails-buffer (rails/root)
                                                        (rails/cut-root (buffer-file-name))
                                                        rails/current-buffer))
     (rails/toggle-file-by-goto-item (rails/root) goto-item))))

(provide 'rails-mailer-bundle)