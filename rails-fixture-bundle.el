;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Constants
;;

(defconst rails/fixture/dir "test/fixtures/")
(defconst rails/fixture/goto-item-weight 1)
(defconst rails/fixture/buffer-weight 1)
(defconst rails/fixture/buffer-type :fixture)
(defconst rails/fixture/file-suffix ".yml")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Functions
;;

(defun rails/fixture/canonical-name (file)
  (let* ((name (file-name-sans-extension file))
         (name (string-ext/cut name rails/fixture/dir :begin)))
    name))

(defun rails/fixture/exist-p (root resource-name)
  (when resource-name
    (let ((file (concat rails/fixture/dir
                        resource-name
                        rails/fixture/file-suffix)))
      (when (rails/file-exist-p root file)
        file))))

(defun rails/fixture/fixture-p (file)
  (rails/with-root file
    (when-bind (buf (rails/determine-type-of-file (rails/root) (rails/cut-root file)))
      (eq rails/fixture/buffer-type (rails/buffer-type buf)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Callbacks
;;

(defun rails/fixture/determine-type-of-file (rails-root file)
  (when (string-ext/start-p file rails/fixture/dir)
    (let ((name (rails/fixture/canonical-name file)))
      (make-rails/buffer :type   rails/fixture/buffer-type
                         :weight rails/fixture/buffer-weight
                         :name   name
                         :resource-name (pluralize-string name)))))

(defun rails/fixture/goto-item-from-file (root file rails-current-buffer)
  (when-bind (type (rails/resource-type-p rails-current-buffer rails/fixture/buffer-type))
     (when-bind (file-name
                 (rails/fixture/exist-p root (rails/buffer-resource-name rails-current-buffer)))
       (make-rails/goto-item :group :test
                             :name "Fixture"
                             :file file-name))))

(defun rails/fixture/goto-item-from-rails-buffer (root file rails-current-buffer)
  (when-bind (item (rails/fixture/goto-item-from-file root file rails-current-buffer))
    (setf (rails/goto-item-weight item) rails/fixture/goto-item-weight)
    item))

(defun rails/fixture/load ()
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
  (rails/with-current-buffer
   (rails/directory-to-goto-menu (rails/root)
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