;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Constants
;;

(defconst rails/helper/dir "app/helpers/")
(defconst rails/helper/file-suffix "_helper")
(defconst rails/helper/buffer-type :helper)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Functions
;;

(defun rails/helper/canonical-name (file)
  (let* ((name (file-name-sans-extension file))
         (name (string-ext/cut name rails/helper/dir :begin))
         (name (string-ext/cut name rails/helper/file-suffix :end)))
    name))

(defun rails/helper/exist-p (root resource-name)
  (let ((file (concat rails/helper/dir
                      (pluralize-string resource-name)
                      rails/helper/file-suffix
                      rails/ruby/file-suffix)))
    (when (rails/file-exist-p root file)
      file)))

(defun rails/helper/helper-p (file)
  (rails/with-root file
    (when-bind (buf (rails/determine-type-of-file (rails/root) (rails/cut-root file)))
      (eq rails/helper/buffer-type (rails/buffer-type buf)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Callbacks
;;

(defun rails/helper/determine-type-of-file (rails-root file)
  (when (string-ext/start-p file rails/helper/dir)
    (let ((name (rails/helper/canonical-name file)))
      (make-rails/buffer :type   rails/helper/buffer-type
                         :name   name
                         :resource-name (pluralize-string name)))))

(defun rails/helper/goto-item-from-file (root file rails-current-buffer)
  (when (rails/resource-type-of-buffer rails-current-buffer
                                       :exclude rails/helper/buffer-type)
    (when-bind (file-name
                (rails/helper/exist-p root (rails/buffer-resource-name rails-current-buffer)))
      (make-rails/goto-item :name "Helper"
                            :file file-name))))

(defun rails/helper/load ()
  (rails/add-to-resource-types-list rails/helper/buffer-type)
  (rails/add-to-layouts-list :controller rails/helper/buffer-type)
  (rails/define-goto-key "h" 'rails/helper/goto-from-list)
  (rails/define-goto-menu  "Helper" 'rails/helper/goto-from-list)
  (rails/define-toggle-key "h" 'rails/helper/goto-current)
  (rails/define-toggle-menu "Helper" 'rails/helper/goto-current))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Interactives
;;

(defun rails/helper/goto-from-list ()
  (interactive)
  (rails/with-current-buffer
   (let ((test-helper (make-rails/goto-item :group :test
                                            :name "TestHelper"
                                            :file "test/test_helper.rb")))
     (rails/directory-to-goto-menu (rails/root)
                                   rails/helper/dir
                                   "Select a Helper"
                                   :filter-by 'rails/helper/helper-p
                                   :name-by (funcs-chain file-name-sans-extension string-ext/decamelize)
                                   :append (list test-helper)))))

(defun rails/helper/goto-current ()
  (interactive)
  (rails/with-current-buffer
   (when-bind (goto-item
               (rails/helper/goto-item-from-file (rails/root)
                                                 (rails/cut-root (buffer-file-name))
                                                 rails/current-buffer))
     (rails/toggle-file-by-goto-item (rails/root) goto-item))))


(provide 'rails-helper-bundle)