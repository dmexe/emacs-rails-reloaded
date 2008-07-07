;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Constants
;;

(defconst rails/rake/command "rake")
(defconst rails/rake/tasks-cache-file "tmp/.tasks-cache")
(defvar rails/rake/history nil)
(defvar rails/rake/tasks-regexp "^rake \\([^ ]*\\).*# \\(.*\\)"
  "Regexp to match tasks list in `rake --tasks` output.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Functions
;;

(defun rails/rake/create-tasks-cache (file-name)
  "Create a cache file from rake --tasks output."
  (let ((tasks (loop for str in (split-string (shell-command-to-string "rake --tasks") "\n")
                     for task = (unless (string-ext/empty-p str)
                                  (string-ext/string=~ rails/rake/tasks-regexp str $1))
                     when task collect task)))
    (files-ext/write-string-to-file file-name (prin1-to-string tasks))
    tasks))

(defun rails/rake/list-of-tasks (root)
  "Return all available tasks and create tasks cache file."
  (let* ((cache-file (concat root rails/rake/tasks-cache-file)))
    (if (file-exists-p cache-file)
        (files-ext/read-from-file cache-file)
      (rails/rake/create-tasks-cache cache-file))))


(defun rails/rake/task-run (root task)
  "Run a Rake task in RAILS_ROOT with MAJOR-MODE."
  (when (and task root)
    (rails/runner/run root rails/rake/command task)
    (setq rails/runner/after-stop-func-list
          (cons 'rails/runner/popup-buffer rails/runner/after-stop-func-list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Callbacks
;;

(defun rails/rake/load ()
  (rails/define-key "r" 'rails/rake/run)
  (let ((map (make-sparse-keymap)))
    (define-keys map
      ([reset] (cons "Reset Tasks Cache" 'rails/rake/reset-cache))
      ([list] (cons "List Tasks" 'rails/rake/list-tasks))
      ([task] (cons "Run Rake Task" 'rails/rake/run)))
    (rails/add-to-bundles-menu "Rake" map)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Interactives
;;

(defun rails/rake/run ()
  "Run a Rake task."
  (interactive)
  (when-bind (root (rails/root))
    (let ((task (ido-completing-read "What task run: "
                                     (rails/rake/list-of-tasks (rails/root))
                                     nil t
                                     (car rails/rake/history)
                                     'rails/rake/history)))
     (rails/rake/task-run root task))))

(defun rails/rake/reset-cache ()
  (interactive)
  (when-bind (root (rails/root))
    (rails/rake/create-tasks-cache (concat root rails/rake/tasks-cache-file))))

(defun rails/rake/list-tasks ()
  (interactive)
  (when-bind (root (rails/root))
    (rails/rake/task-run root "-T")))

(provide 'rails-model-bundle)