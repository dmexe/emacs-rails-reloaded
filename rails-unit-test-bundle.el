;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Constants
;;

(defconst rails/unit-test/dir "test/unit/")
(defconst rails/unit-test/file-suffix "_test")
(defconst rails/unit-test/buffer-weight 1)
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
    (when-bind (buf (rails/determine-type-of-file (rails/root) (rails/cut-root file)))
      (eq rails/unit-test/buffer-type (rails/buffer-type buf)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Callbacks
;;

(defun rails/unit-test/goto-item-from-file (root file rails-current-buffer)
  (when-bind (type (rails/resource-type-p rails-current-buffer rails/unit-test/buffer-type))
     (when-bind (file-name
                 (rails/unit-test/exist-p root (rails/buffer-tests-name rails-current-buffer)))
       (make-rails/goto-item :group :test
                             :name "Unit Test"
                             :file file-name))))

(defun rails/unit-test/determine-type-of-file (rails-root file)
  (when (string-ext/start-p file rails/unit-test/dir)
    (let ((name (rails/unit-test/canonical-name file)))
      (make-rails/buffer :type   rails/unit-test/buffer-type
                         :weight rails/unit-test/buffer-weight
                         :name   name
                         :resource-name (pluralize-string name)))))

;; (defun rails/model/initialize (root file rails-current-buffer)
;; )

(defun rails/unit-test/load ()
  (rails/add-to-resource-types-list rails/unit-test/buffer-type)
  (rails/add-to-layouts-alist :model rails/unit-test/buffer-type)
  (rails/define-goto-key "u" 'rails/unit-test/goto-from-list)
  (rails/define-goto-menu [unit-test] 'rails/unit-test/goto-from-list "Unit Test")
  (rails/define-fast-goto-key "u" 'rails/unit-test/goto-current)
  (rails/define-fast-goto-menu [unit-test] 'rails/unit-test/goto-current "Unit Test"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Interactives
;;

(defun rails/unit-test/goto-from-list ()
  (interactive)
  (let ((file (buffer-file-name)))
    (rails/with-root file
      (rails/directory-to-goto-menu (rails/root)
                                    rails/unit-test/dir
                                    "Select a Unit Test"
                                    :filter-by 'rails/unit-test/unit-test-p
                                    :name-by (funcs-chain file-name-sans-extension string-ext/decamelize)))))

(defun rails/unit-test/goto-current ()
  (interactive)
  (let ((file (buffer-file-name))
        (rails-buffer rails/current-buffer))
    (rails/with-root file
      (when-bind
       (goto-item
        (rails/unit-test/goto-item-from-file (rails/root)
                                             (rails/cut-root file)
                                             rails-buffer))
       (rails/fast-find-file-by-goto-item (rails/root) goto-item)))))

(provide 'rails-unit-test-bundle)