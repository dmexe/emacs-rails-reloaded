;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Constants
;;

(defconst rails/fixture/dir "test/fixtures/")
(defconst rails/fixture/buffer-type :fixture)
(defconst rails/fixture/file-suffix ".yml")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Functions
;;

(defun rails/fixture/resource-true-name (resource-name)
  (replace-regexp-in-string "\/" "_" resource-name))

(defun rails/fixture/canonical-name (file)
  (let* ((name (file-name-sans-extension file))
         (name (string-ext/cut name rails/fixture/dir :begin)))
    name))

(defun rails/fixture/exist-p (root resource-name)
  (when resource-name
    (let ((file (concat rails/fixture/dir
                        (rails/fixture/resource-true-name resource-name)
                        rails/fixture/file-suffix)))
      (when (rails/file-exist-p root file)
        file))))

(defun rails/fixture/fixture-p (file)
  (rails/with-root file
    (string-ext/start-p (rails/cut-root file) rails/fixture/dir)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Callbacks
;;

(defun rails/fixture/determine-type-of-file (rails-root file)
  (when (rails/fixture/fixture-p (concat rails-root file))
    (let ((name (rails/fixture/canonical-name file)))
      (make-rails/buffer :type   rails/fixture/buffer-type
                         :name   name
                         :resource-name (pluralize-string name)))))

(defun rails/fixture/goto-item-from-file (root file rails-current-buffer)
  (when (rails/resource-type-of-buffer rails-current-buffer)
    (when-bind (file-name
                (rails/fixture/exist-p root (rails/buffer-resource-name rails-current-buffer)))
      (make-rails/goto-item :group :test
                            :name "Fixture"
                            :file file-name))))

(defalias 'rails/fixture/goto-item-from-rails-buffer
          'rails/fixture/goto-item-from-file)

(defun rails/fixture/load ()
  (rails/add-to-bundles-group "Test::Unit" rails/fixture/buffer-type)
  (rails/add-to-resource-types-list rails/fixture/buffer-type)
  (rails/add-to-layouts-list :unit-test rails/fixture/buffer-type)
  (rails/define-goto-key "x" 'rails/fixture/goto-from-list)
  (rails/define-goto-menu "Fixture" 'rails/fixture/goto-from-list)
  (rails/define-toggle-key "x" 'rails/fixture/goto-current)
  (rails/define-toggle-menu "Fixture" 'rails/fixture/goto-current))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Interactives
;;

(defun rails/fixture/goto-from-list ()
  (interactive)
  (when-bind (root (rails/root))
   (rails/directory-to-goto-menu root
                                 rails/fixture/dir
                                 "Select a Fixture"
                                 :name-by 'file-name-sans-extension)))

(defun rails/fixture/goto-current ()
  (interactive)
  (rails/with-current-buffer
   (when-bind (goto-item
               (rails/fixture/goto-item-from-file (rails/root)
                                                  (rails/cut-root (buffer-file-name))
                                                  rails/current-buffer))
     (rails/toggle-file-by-goto-item (rails/root) goto-item))))

(provide 'rails-fixture-bundle)