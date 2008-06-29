;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Constants
;;

(defconst rails/controller/dir "app/controllers/")
(defconst rails/controller/file-suffix "_controller")
(defconst rails/controller/fast-goto-item-weight 5)
(defconst rails/controller/buffer-weight 1)
(defconst rails/controller/buffer-type :controller)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Functions
;;

(defun rails/controller/canonical-name (file)
  (let* ((name (file-name-sans-extension file))
         (name (string-ext/cut name rails/controller/dir :begin))
         (name (string-ext/cut-safe name rails/controller/file-suffix :end)))
    name))

(defun rails/controller/exist-p (root association-name)
  (let ((file (concat rails/controller/dir
                      (pluralize-string association-name)
                      rails/controller/file-suffix
                      rails/ruby/file-suffix)))
    (when (rails/file-exist-p root file)
      file)))

(defun rails/controller/controller-p (root file)
  (when-bind (buf (rails/determine-type-of-file root (concat rails/controller/dir file)))
    (when (eq rails/controller/buffer-type (rails/buffer-type buf))
      buf)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Callbacks
;;

(defun rails/controller/goto-item-from-file (root file rails-current-buffer)
  (when-bind (type (rails/associated-type-p rails-current-buffer rails/controller/buffer-type))
     (when-bind (file-name
                 (rails/controller/exist-p root (rails/buffer-association-name rails-current-buffer)))
       (make-rails/goto-item :name "Controller"
                             :file file-name))))

(defun rails/controller/fast-goto-item-from-file (root file rails-current-buffer)
  (when-bind (item (rails/controller/goto-item-from-file root file rails-current-buffer))
    (setf (rails/goto-item-weight item) rails/controller/fast-goto-item-weight)
    item))

(defun rails/controller/current-buffer-action-name ()
  (rails/ruby/current-method))

(defun rails/controller/goto-action-in-current-buffer (action-name)
  (rails/ruby/goto-method-in-current-buffer action-name))

(defun rails/controller/determine-type-of-file (rails-root file)
  (when (string-ext/start-p file rails/controller/dir)
    (let ((name (rails/controller/canonical-name file)))
      (make-rails/buffer :type   rails/controller/buffer-type
                         :weight rails/controller/buffer-weight
                         :name   name
                         :association-name name
                         :views-dir-name   name))))

;; (defun rails/controller/initialize (root file rails-current-buffer)
;;   )

(defun rails/controller/load ()
  (rails/add-to-associated-types-list rails/controller/buffer-type)
  (rails/define-goto-key "c" 'rails/controller/goto-from-list)
  (rails/define-fast-goto-key "c" 'rails/controller/goto-associated))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Interactives
;;

(defun rails/controller/goto-associated ()
  (interactive)
  (let ((file (buffer-file-name))
        (rails-buffer rails/current-buffer))
    (rails/with-root file
      (when-bind
       (goto-item
        (rails/controller/goto-item-from-file (rails/root)
                                              (rails/cut-root file)
                                              rails-buffer))
       (rails/fast-find-file-by-goto-item (rails/root) goto-item)))))

(defun rails/controller/goto-from-list ()
  (interactive)
  (let ((file (buffer-file-name)))
    (rails/with-root file
      (rails/directory-to-goto-menu (rails/root)
                                    rails/controller/dir
                                    "Select a Controller"
                                    'rails/controller/controller-p))))

(provide 'rails-controller-bundle)