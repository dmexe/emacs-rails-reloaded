;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Constants
;;

(defconst rails/javascript/dir "public/javascripts/")
(defconst rails/javascript/file-suffix ".js")
(defconst rails/javascript/buffer-type :javascript)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Functions
;;

(defun rails/javascript/canonical-name (file)
  (let* ((name (file-name-sans-extension file))
         (name (string-ext/cut name rails/javascript/dir :begin)))
    name))

(defun rails/javascript/exist-p (root resource-name)
  (let ((file (concat rails/javascript/dir
                      resource-name
                      rails/javascript/file-suffix)))
    (when (rails/file-exist-p root file)
      file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Callbacks
;;

(defun rails/javascript/determine-type-of-file (rails-root file)
  (let ((name (rails/javascript/canonical-name file)))
    (make-rails/buffer :type   rails/javascript/buffer-type
                       :name   name
                       :resource-name name)))

(defun rails/javascript/goto-item-from-file (root file rails-current-buffer)
  (when (rails/resource-type-of-buffer rails-current-buffer)
    (when-bind (file-name
                (rails/javascript/exist-p root (rails/buffer-resource-name rails-current-buffer)))
      (make-rails/goto-item :name "Javascript"
                            :file file-name))))

(defun rails/javascript/load ()
  (rails/add-to-resource-types-list rails/javascript/buffer-type)
  (rails/add-to-layouts-list :controller rails/javascript/buffer-type)
  (rails/define-goto-key "j" 'rails/javascript/goto-from-list)
  (rails/define-goto-menu  "Javascript" 'rails/javascript/goto-from-list)
  (rails/define-toggle-key "j" 'rails/javascript/goto-current)
  (rails/define-toggle-menu "Javascript" 'rails/javascript/goto-current))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Interactives
;;

(defun rails/javascript/goto-from-list ()
  (interactive)
  (when-bind (root (rails/root))
    (rails/directory-to-goto-menu root
                                  rails/javascript/dir
                                  "Select a Javascript")))

(defun rails/javascript/goto-current ()
  (interactive)
  (rails/with-current-buffer
   (when-bind (goto-item
               (rails/javascript/goto-item-from-file (rails/root)
                                                     (rails/cut-root (buffer-file-name))
                                                     rails/current-buffer))
     (rails/toggle-file-by-goto-item (rails/root) goto-item))))


(provide 'rails-javascript-bundle)