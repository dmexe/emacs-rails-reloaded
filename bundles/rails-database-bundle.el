(require 'cl)

(eval-when-compile
  (load "bundles/rails-migration-bundle"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Constants
;;

(defvar rails/database/keywords
  '(("^[=]+\s\\(.*\\)\s[=]+"
     (0 font-lock-string-face))
    ("^[-]+\s\\(.*\\)("
     (1 font-lock-function-name-face))
    ("\\(:[^ ,)]+\\)"
     (1 font-lock-constant-face))
    ("^\s+\\(->.*\\)"
     (1 font-lock-comment-face))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Functions
;;

(defun rails/database/versions (root)
  (reverse
   (delete-if nil
              (mapcar
               #'(lambda (file)
                   (string-ext/string=~ "^\\([0-9]+\\)\\(.*\\)\\.rb"
                                        file
                                        (format "%s %s" $1 (string-ext/decamelize $2))))
               (directory-files
                (concat root (or rails/migration/dir
                                 "db/migrate/"))
                nil rails/ruby/file-suffix)))))

(defmacro rails/database/retmsg (success failed)
  `(lambda (retval)
     (rails/notify
      (if (zerop retval) ,success ,failed))))

(defun rails/database/run-task (root task &optional funcs args)
  "Run a Database task in RAILS_ROOT with MAJOR-MODE."
  (when (and task root)
    (unless funcs
      (setq funcs '(rails/runner/popup-buffer)))
    (rails/runner/run root
                      "rake"
                      (if args
                          (format "RAILS_ENV=%s db:%s %s" rails/default-environment task args)
                        (format "RAILS_ENV=%s db:%s" rails/default-environment task))
                      :keywords rails/database/keywords)
      (setq rails/runner/after-stop-func-list funcs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Callbacks
;;

(defun rails/database/load ()
  (rails/define-key "d m" 'rails/database/migrate)
  (rails/define-key "d v" 'rails/database/migrate-to-version)
  (rails/define-key "d r" 'rails/database/migrate-rollback)
  (rails/define-key "d R" 'rails/database/migrate-redo)
  (rails/define-key "d c" 'rails/database/clone)
  (let ((map (make-sparse-keymap)))
    (define-keys map
      ([clone]     (cons "Clone Development DB to Test DB" 'rails/database/clone))
      ([redo]      (cons "Redo Last Migration" 'rails/database/migrate-redo))
      ([rollback]  (cons "Migrate to Previous Version" 'rails/database/migrate-rollback))
      ([version]   (cons "Migrate to Version" 'rails/database/migrate-to-version))
      ([migrate]   (cons "Migrate to Current" 'rails/database/migrate)))
    (rails/add-to-bundles-menu "Database" map)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Interactives
;;

(defun rails/database/migrate ()
  (interactive)
  (when-bind (root (rails/root))
    (rails/database/run-task root "migrate")))

(defun rails/database/migrate-to-version ()
  (interactive)
  (when-bind (root (rails/root))
    (let ((version
           (ido-completing-read "Version: "
                                (rails/database/versions root)
                                nil
                                t)))
      (when (not (string-ext/empty-p version))
        (rails/database/run-task
         root
         "migrate"
         nil
         (format "VERSION=%s" (car (split-string version " "))))))))

(defun rails/database/migrate-rollback ()
  (interactive)
  (when-bind (root (rails/root))
    (let* ((steps
            (rails/completing-read "Steps to rollback"))
           (steps (string-to-number steps)))
      (if (zerop steps)
          (rails/notify "Invalid step, muste be digits.")
        (rails/database/run-task root
                                 "rollback"
                                 nil
                                 (format "STEP=%s" steps))))))

(defun rails/database/migrate-redo ()
  (interactive)
  (when-bind (root (rails/root))
    (rails/database/run-task root "migrate:redo")))

(defun rails/database/clone ()
  (interactive)
  (when-bind (root (rails/root))
    (rails/database/run-task root
                             "test:clone"
                             (list
                              (rails/database/retmsg
                               "Database successfully cloned."
                               "Failed to clone database.")
                              'rails/runner/popup-buffer-if-failed))))

(provide 'rails-database-bundle)