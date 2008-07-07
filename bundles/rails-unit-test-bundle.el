;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Constants
;;

(defconst rails/unit-test/dir "test/unit/")
(defconst rails/unit-test/file-suffix "_test")
(defconst rails/unit-test/buffer-type :unit-test)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Functions
;;

(defun rails/unit-test/canonical-name (file)
  (let* ((name (file-name-sans-extension file))
         (name (string-ext/cut name rails/unit-test/dir :begin))
         (name (string-ext/cut name rails/unit-test/file-suffix :end)))
    name))

(defun rails/unit-test/exist-p (root tests-name)
  (when tests-name
    (let ((file (concat rails/unit-test/dir
                        (singularize-string tests-name)
                        rails/unit-test/file-suffix
                        rails/ruby/file-suffix)))
      (when (rails/file-exist-p root file)
        file))))

(defun rails/unit-test/unit-test-p (file)
  (rails/with-root file
    (string-ext/start-p (rails/cut-root file) rails/unit-test/dir)))

(defun rails/unit-test/resource-true-name (root resource)
  (if (rails/resource-mailer-p root resource)
      (singularize-string resource)
    (pluralize-string resource)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Callbacks
;;

(defun rails/unit-test/determine-type-of-file (rails-root file)
  (when (rails/unit-test/unit-test-p (concat rails-root file))
    (let ((name (rails/unit-test/canonical-name file)))
      (make-rails/buffer :type   rails/unit-test/buffer-type
                         :name   name
                         :resource-name (rails/unit-test/resource-true-name rails-root name)))))

(defun rails/unit-test/goto-item-from-file (root file rails-current-buffer)
  (when (rails/resource-type-of-buffer rails-current-buffer
                                       :exclude rails/unit-test/buffer-type)
    (when-bind (file-name
                (rails/unit-test/exist-p root (rails/buffer-tests-name rails-current-buffer)))
      (make-rails/goto-item :group :test
                            :name "Unit Test"
                            :file file-name))))

(defalias 'rails/unit-test/goto-item-from-rails-buffer
          'rails/unit-test/goto-item-from-file)

(defun rails/unit-test/load ()
  (rails/add-type-link :tests :model rails/unit-test/buffer-type)
  (rails/add-type-link :tests :mailer rails/unit-test/buffer-type)
  (rails/add-to-bundles-group "Test::Unit" rails/unit-test/buffer-type)
  (rails/add-to-resource-types-list rails/unit-test/buffer-type)
  (rails/define-goto-key "u" 'rails/unit-test/goto-from-list)
  (rails/define-goto-menu  "Unit Test" 'rails/unit-test/goto-from-list)
  (rails/define-toggle-key "u" 'rails/unit-test/goto-current)
  (rails/define-toggle-menu  "Unit Test" 'rails/unit-test/goto-current))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Interactives
;;

(defun rails/unit-test/goto-from-list ()
  (interactive)
  (rails/with-current-buffer
   (rails/directory-to-goto-menu (rails/root)
                                 rails/unit-test/dir
                                 "Select a Unit Test"
                                 :filter-by 'rails/unit-test/unit-test-p
                                 :name-by (funcs-chain file-name-sans-extension string-ext/decamelize))))

(defun rails/unit-test/goto-current ()
  (interactive)
  (rails/with-current-buffer
   (when-bind (goto-item
               (rails/unit-test/goto-item-from-rails-buffer (rails/root)
                                                            (rails/cut-root (buffer-file-name))
                                                            rails/current-buffer))
     (rails/toggle-file-by-goto-item (rails/root) goto-item))))

(provide 'rails-unit-test-bundle)