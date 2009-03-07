(require 'cl)

(autoload 'rails/rake-bundle/task-run "bundles/rails-database-bundle")

;;; ---------------------------------------------------------
;;; - Variables
;;;
(defvar rails/database-bundle/keywords
  '(("^[=]+\s\\(.*\\)\s[=]+"
     (0 font-lock-string-face))
    ("^[-]+\s\\(.*\\)("
     (1 font-lock-function-name-face))
    ("\\(:[^ ,)]+\\)"
     (1 font-lock-constant-face))
    ("^\s+\\(->.*\\)"
     (1 font-lock-comment-face))))

;;; ---------------------------------------------------------
;;; - Functions
;;;

(defun rails/database-bundle/versions (root)
  (reverse
   (delete-if nil
              (mapcar
               #'(lambda (file)
                   (string-ext/string=~ "^\\([0-9]+\\)\\(.*\\)\\.rb"
                                        file
                                        (format "%s %s" $1 (string-ext/decamelize $2))))
               (directory-files
                (concat root "db/migrate/")
                nil rails/ruby/file-suffix)))))

(defmacro rails/database-bundle/retmsg (success failed)
  `(lambda (retval)
     (rails/notify
      (if (zerop retval) ,success ,failed))))

(defun rails/database-bundle/run-task (root task &optional args funcs)
  "Run a Database task in RAILS_ROOT with MAJOR-MODE."
  (when (and task root)
    (unless funcs
      (setq funcs '(rails/runner/popup-buffer-if-failed)))
    (rails/rake-bundle/task-run
     root (concat "db:" task) args rails/database-bundle/keywords funcs)))

;;; ---------------------------------------------------------
;;; - Interactives
;;;

(defun rails/database-bundle/migrate ()
  (interactive)
  (when-bind (root (rails/root))
    (rails/database-bundle/run-task root "migrate")))

(defun rails/database-bundle/migrate-to-version ()
  (interactive)
  (when-bind (root (rails/root))
    (let ((version
           (ido-completing-read "Version: "
                                (rails/database-bundle/versions root)
                                nil
                                t)))
      (when (not (string-ext/empty-p version))
        (rails/database-bundle/run-task
         root
         "migrate"
         (format "VERSION=%s" (car (split-string version " "))))))))

(defun rails/database-bundle/migrate-rollback ()
  (interactive)
  (when-bind (root (rails/root))
    (let* ((steps
            (rails/completing-read "Steps to rollback"))
           (steps (string-to-number steps)))
      (if (zerop steps)
          (rails/notify "Invalid step, muste be digits.")
        (rails/database-bundle/run-task root
                                        "rollback"
                                        (format "STEP=%s" steps))))))

(defun rails/database-bundle/migrate-redo ()
  (interactive)
  (when-bind (root (rails/root))
    (rails/database-bundle/run-task root "migrate:redo")))

(defun rails/database-bundle/clone ()
  (interactive)
  (when-bind (root (rails/root))
    (rails/database-bundle/run-task root "test:clone")))

;;; ---------------------------------------------------------
;;; - Bundle
;;;

(rails/defbundle "Database"
  (:menu
   (([clone]     (cons "Clone Development DB to Test DB" 'rails/database-bundle/clone))
    ([redo]      (cons "Redo Last Migration" 'rails/database-bundle/migrate-redo))
    ([rollback]  (cons "Migrate to Previous Version" 'rails/database-bundle/migrate-rollback))
    ([version]   (cons "Migrate to Version" 'rails/database-bundle/migrate-to-version))
    ([migrate]   (cons "Migrate" 'rails/database-bundle/migrate)))
   :keys
   (("d m" 'rails/database-bundle/migrate)
    ("d v" 'rails/database-bundle/migrate-to-version)
    ("d r" 'rails/database-bundle/migrate-rollback)
    ("d R" 'rails/database-bundle/migrate-redo)
    ("d c" 'rails/database-bundle/clone))
   :triggers
   (("db" "Database Tasks"
     (candidates
      .
      (lambda ()
        (when (string-match "^db" anything-pattern)
          (list
           (cons
            (format "db migrate %s" (propertize "# Migrate" 'face 'font-lock-comment-face))
            'rails/database-bundle/migrate)
           (cons
            (format "db clone %s" (propertize "# Clone Development DB to Test DB" 'face 'font-lock-comment-face))
            'rails/database-bundle/clone)
           (cons
            (format "db version %s" (propertize "# Migrate to Version" 'face 'font-lock-comment-face))
            'rails/database-bundle/migrate-to-version)
           (cons
            (format "db redo %s" (propertize "# Redo Last Migration" 'face 'font-lock-comment-face))
            'rails/database-bundle/migrate-redo)
           (cons
            (format "db rollback %s" (propertize "# Migrate to Previous Version" 'face 'font-lock-comment-face))
            'rails/database-bundle/migrate-rollback)))))
     (action ("Run" . (lambda (i) (funcall i))))
     (requires-pattern . 2)))))
