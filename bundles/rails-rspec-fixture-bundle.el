;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Constants
;;

(defconst rails/rspec-fixture/dir "spec/fixtures/")
(defconst rails/rspec-fixture/buffer-type :rspec-fixture)
(defconst rails/rspec-fixture/file-suffix ".yml")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Functions
;;

(defun rails/rspec-fixture/resource-true-name (resource-name)
  (replace-regexp-in-string "\/" "_" resource-name))

(defun rails/rspec-fixture/canonical-name (file)
  (let* ((name (file-name-sans-extension file))
         (name (string-ext/cut name rails/rspec-fixture/dir :begin)))
    name))

(defun rails/rspec-fixture/exist-p (root resource-name)
  (when resource-name
    (let ((file (concat rails/rspec-fixture/dir
                        (rails/rspec-fixture/resource-true-name resource-name)
                        rails/rspec-fixture/file-suffix)))
      (when (rails/file-exist-p root file)
        file))))

(defun rails/rspec-fixture/fixture-p (file)
  (rails/with-root file
    (string-ext/start-p (rails/cut-root file) rails/rspec-fixture/dir)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Callbacks
;;

(defun rails/rspec-fixture/determine-type-of-file (rails-root file)
  (when (rails/rspec-fixture/fixture-p (concat rails-root file))
    (let ((name (rails/rspec-fixture/canonical-name file)))
      (make-rails/buffer :type   rails/rspec-fixture/buffer-type
                         :name   name
                         :resource-name (pluralize-string name)))))

(defun rails/rspec-fixture/goto-item-from-file (root file rails-current-buffer)
  (when (rails/resource-type-of-buffer rails-current-buffer)
    (when-bind (file-name
                (rails/rspec-fixture/exist-p root (rails/buffer-resource-name rails-current-buffer)))
      (make-rails/goto-item :group :rspec
                            :name "RSpec Fixture"
                            :file file-name))))

(defalias 'rails/rspec-fixture/goto-item-from-rails-buffer
  'rails/rspec-fixture/goto-item-from-file)


(defun rails/rspec-fixture/load ()
  (rails/define-bundle
   rails/rspec-fixture/buffer-type rails/rspec-fixture/buffer-type "RSpec"
   (rails/add-to-layouts-list :unit-test rails/rspec-fixture/buffer-type)
   (rails/define-goto-key "x" 'rails/rspec-fixture/goto-from-list)
   (rails/define-goto-menu "RSpec Fixture" 'rails/rspec-fixture/goto-from-list)
   (rails/define-toggle-key "x" 'rails/rspec-fixture/goto-current)
   (rails/define-toggle-menu "RSpec Fixture" 'rails/rspec-fixture/goto-current)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Interactives
;;

(defun rails/rspec-fixture/goto-from-list ()
  (interactive)
  (when-bind (root (rails/root))
    (rails/directory-to-goto-menu root
                                  rails/rspec-fixture/dir
                                  "Select a Rspec Fixture")))

(defun rails/rspec-fixture/goto-current ()
  (interactive)
  (rails/with-current-buffer
   (when-bind (goto-item
               (rails/rspec-fixture/goto-item-from-file (rails/root)
                                                  (rails/cut-root (buffer-file-name))
                                                  rails/current-buffer))
     (rails/toggle-file-by-goto-item (rails/root) goto-item))))

(provide 'rails-rspec-fixture-bundle)
