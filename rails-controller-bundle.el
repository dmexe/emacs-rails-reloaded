(defconst rails/controller/dir "app/controllers/")
(defconst rails/controller/file-suffix "_controller")
(defconst rails/controller/goto-item-weight 5)
(defconst rails/controller/buffer-weight 1)
(defconst rails/controller/buffer-type :controller)

(defun rails/controller/goto-item-from-file (root file rails-buffer)
  (when-bind (type (rails/associated-type-p rails-buffer rails/controller/buffer-type))
     (when-bind (file-name
                 (rails/controller/exist-p root (rails/buffer-name rails-buffer)))
       (make-rails/goto-item :group :default
                             :name "Controller"
                             :file file-name))))

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

(defun rails/controller/fast-goto-item-from-file (root file rails-buffer)
  (when-bind (type (rails/associated-type-p rails-buffer rails/controller/buffer-type))
     (when-bind (file-name
                 (rails/controller/exist-p root (rails/buffer-name rails-buffer)))
       (make-rails/goto-item :group :default
                             :name "Controller"
                             :file file-name
                             :weight rails/controller/goto-item-weight))))

(defun rails/controller/determine-type-of-file (rails-root file)
  (when (string-ext/start-p file rails/controller/dir)
    (make-rails/buffer :type   rails/controller/buffer-type
                       :weight rails/controller/buffer-weight
                       :name   (rails/controller/canonical-name file))))

(defun rails/controller/canonical-name (file)
  (let* ((name (file-name-sans-extension file))
         (name (string-ext/cut name rails/controller/dir :begin))
         (name (string-ext/cut-safe name rails/controller/file-suffix :end)))
    name))

(defun rails/controller/exist-p (root canonical-name)
  (let ((file (concat rails/controller/dir
                      (pluralize-string canonical-name)
                      rails/controller/file-suffix
                      rails/ruby/file-suffix)))
    (when (rails/file-exist-p root file)
      file)))

(defun rails/controller/initialize (root file)
  (when (and (rails/buffer-p rails/current-buffer)
             (eq (rails/buffer-type rails/current-buffer) rails/controller/buffer-type))
    (add-hook 'rails/after-goto-file-hook 'rails/controller/after-goto-file-hook t t)))

(defun rails/controller/current-buffer-action-name ()
  (let (action
        (re "^ *def +\\([^ (\n]+\\)"))
    (save-excursion
      (end-of-line)
      (when (re-search-backward re nil t)
        (setq action (buffer-substring-no-properties (match-beginning 1) (match-end 1)))))
    action))

(defun rails/controller/goto-action-in-current-buffer (action)
    (let* (pos
           (re (format "^ *def +\\(%s\\)" (regexp-quote action))))
    (save-excursion
      (goto-char (point-min))
      (when-bind (action-pos (re-search-forward re nil t))
        (setq pos action-pos)))
    (when pos
      (goto-char pos)
      (beginning-of-line))))

(defun rails/controller/after-goto-file-hook ()
  (when (and (rails/goto-item-p goto-item)
             (rails/goto-item-action-name goto-item))
    (rails/controller/goto-action-in-current-buffer
     (rails/goto-item-action-name goto-item))))

(defun rails/controller/goto-from-list ()
  (interactive)
  (let ((file (buffer-file-name)))
    (rails/with-root file
      (rails/directory-to-goto-menu (rails/root)
                                    rails/controller/dir
                                    "Select a Controller"
                                    'rails/controller/controller-p))))

(defun rails/controller/controller-p (root file)
  (when-bind (buf (rails/determine-type-of-file root (concat rails/controller/dir file)))
    (when (eq rails/controller/buffer-type (rails/buffer-type buf))
      buf)))

(defun rails/controller/load ()
  (rails/add-to-associated-types-list rails/controller/buffer-type)
  (rails/define-goto-key "c" 'rails/controller/goto-from-list)
  (rails/define-fast-goto-key "c" 'rails/controller/goto-associated)
  )

(provide 'rails-controller-bundle)