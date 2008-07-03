;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Constants
;;

(defconst rails/functional-test/dir "test/functional/")
(defconst rails/functional-test/file-suffix "_controller_test")
(defconst rails/functional-test/buffer-weight 1)
(defconst rails/functional-test/buffer-type :functional-test)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Functions
;;

(defun rails/functional-test/canonical-name (file)
  (let* ((name (file-name-sans-extension file))
         (name (string-ext/cut name rails/functional-test/dir :begin))
         (name (string-ext/cut name rails/functional-test/file-suffix :end)))
    name))

(defun rails/functional-test/exist-p (root tests-name)
  (when tests-name
    (let ((file (concat rails/functional-test/dir
                        (pluralize-string tests-name)
                        rails/functional-test/file-suffix
                        rails/ruby/file-suffix)))
      (when (rails/file-exist-p root file)
        file))))

(defun rails/functional-test/functional-test-p (file)
  (rails/with-root file
    (when-bind (buf (rails/determine-type-of-file (rails/root) (rails/cut-root file)))
      (eq rails/functional-test/buffer-type (rails/buffer-type buf)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Callbacks
;;

(defun rails/functional-test/goto-item-from-file (root file rails-current-buffer)
  (when-bind (type (rails/resource-type-p rails-current-buffer rails/functional-test/buffer-type))
     (when-bind (file-name
                 (rails/functional-test/exist-p root (rails/buffer-tests-name rails-current-buffer)))
       (make-rails/goto-item :group :test
                             :name "Functional Test"
                             :file file-name))))

(defun rails/functional-test/determine-type-of-file (rails-root file)
  (when (string-ext/start-p file rails/functional-test/dir)
    (let ((name (rails/functional-test/canonical-name file)))
      (make-rails/buffer :type   rails/functional-test/buffer-type
                         :weight rails/functional-test/buffer-weight
                         :name   name
                         :resource-name (pluralize-string name)))))

(defun rails/functional-test/load ()
  (rails/add-to-resource-types-list rails/functional-test/buffer-type)
  (rails/define-goto-key "f" 'rails/functional-test/goto-from-list)
  (rails/define-goto-menu [functional-test] 'rails/functional-test/goto-from-list "Functional Test")
  (rails/define-fast-goto-key "f" 'rails/functional-test/goto-current)
  (rails/define-fast-goto-menu [functional-test] 'rails/functional-test/goto-current "Functional Test"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Interactives
;;

(defun rails/functional-test/goto-from-list ()
  (interactive)
  (let ((file (buffer-file-name)))
    (rails/with-root file
      (rails/directory-to-goto-menu (rails/root)
                                    rails/functional-test/dir
                                    "Select a Functional Test"
                                    :name-by (funcs-chain file-name-sans-extension string-ext/decamelize)))))

(defun rails/functional-test/goto-current ()
  (interactive)
  (let ((file (buffer-file-name))
        (rails-buffer rails/current-buffer))
    (rails/with-root file
      (when-bind
       (goto-item
        (rails/functional-test/goto-item-from-file (rails/root)
                                                   (rails/cut-root file)
                                                   rails-buffer))
       (rails/fast-find-file-by-goto-item (rails/root) goto-item)))))

(provide 'rails-functional-test-bundle)