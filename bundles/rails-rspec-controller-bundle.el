;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Constants
;;

(defconst rails/rspec-controller/dir "spec/controllers/")
(defconst rails/rspec-controller/file-suffix "_controller_spec")
(defconst rails/rspec-controller/buffer-type :rspec-controller)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Functions
;;

(defun rails/rspec-controller/canonical-name (file)
  (let* ((name (file-name-sans-extension file))
         (name (string-ext/cut name rails/rspec-controller/dir :begin))
         (name (string-ext/cut name rails/rspec-controller/file-suffix :end)))
    name))

(defun rails/rspec-controller/exist-p (root resource-name)
  (when resource-name
    (let ((file (concat rails/rspec-controller/dir
                        resource-name
                        rails/rspec-controller/file-suffix
                        rails/ruby/file-suffix)))
      (when (rails/file-exist-p root file)
        file))))

(defun rails/rspec-controller/rspec-controller-p (file)
  (rails/with-root file
    (string-ext/start-p (rails/cut-root file) rails/rspec-controller/dir)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Callbacks
;;

(defun rails/rspec-controller/determine-type-of-file (rails-root file)
  (when (rails/rspec-controller/rspec-controller-p (concat (rails/root) file))
    (let ((name (rails/rspec-controller/canonical-name file)))
      (make-rails/buffer :type   rails/rspec-controller/buffer-type
                         :name   name
                         :resource-name name))))

(defun rails/rspec-controller/goto-item-from-file (root file rails-current-buffer)
  (when (rails/resource-type-of-buffer rails-current-buffer)
    (when-bind (file-name
                (rails/rspec-controller/exist-p root (rails/buffer-resource-name rails-current-buffer)))
       (make-rails/goto-item :group :rspec
                             :name "RSpec Controller"
                             :file file-name))))

(defalias 'rails/rspec-controller/goto-item-from-rails-buffer
          'rails/rspec-controller/goto-item-from-file)

(defun rails/rspec-controller/load ()
  (rails/define-bundle
   rails/rspec-controller/buffer-type rails/rspec-controller/buffer-type "RSpec"
  (rails/add-type-link :tests :controller rails/rspec-controller/buffer-type)
  (rails/define-goto-key "f" 'rails/rspec-controller/goto-from-list)
  (rails/define-goto-menu  "RSpec Controller" 'rails/rspec-controller/goto-from-list)
  (rails/define-toggle-key "f" 'rails/rspec-controller/goto-current)
  (rails/define-toggle-menu  "RSpec Controller" 'rails/rspec-controller/goto-current)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Interactives
;;

(defun rails/rspec-controller/goto-from-list ()
  (interactive)
  (when-bind (root (rails/root))
   (rails/directory-to-goto-menu root
                                 rails/rspec-controller/dir
                                 "Select a RSpec Controller"
                                 :name-by (funcs-chain file-name-sans-extension string-ext/decamelize))))

(defun rails/rspec-controller/goto-current ()
  (interactive)
  (rails/with-current-buffer
   (when-bind (goto-item
               (rails/rspec-controller/goto-item-from-file (rails/root)
                                                          (rails/cut-root (buffer-file-name))
                                                          rails/current-buffer))
       (rails/toggle-file-by-goto-item (rails/root) goto-item))))

(provide 'rails-rspec-controller-bundle)