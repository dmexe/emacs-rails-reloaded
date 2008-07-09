;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Constants
;;

(defconst rails/rake/command "rake")
(defconst rails/rake/tasks-cache-file "tmp/.tasks-cache")
(defvar rails/rake/history nil)
(defvar rails/rake/tasks-regexp "^rake \\([^ ]*\\).*# \\(.*\\)"
  "Regexp to match tasks list in `rake --tasks` output.")

(defvar rails/rake/task-keywords-alist
  '(("-T$"         . (("#.*$"
                       (0 font-lock-comment-face))
                      ("\\(rake\\) \\([^ ]+\\)"
                       (1 font-lock-function-name-face)
                       (2 font-lock-constant-face))))
    ("^notes.*$"   . (("^\\([^ ]+\\):"
                       (0 font-lock-string-face))
                      ("^ +\\(\\*\\) \\(\\[[ 0-9]+\\]\\( \\[\\w+\\]\\)?\\) \\(.*\\)$"
                       (1 font-lock-function-name-face)
                       (2 font-lock-constant-face)
                       (4 font-lock-comment-face))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Functions
;;

(defun rails/rake/create-tasks-cache (root)  
  "Create a cache file from rake --tasks output."
  (in-directory root
    (let ((tasks (loop for str in (split-string (shell-command-to-string "rake --tasks") "\n")
                       for task = (unless (string-ext/empty-p str)
                                    (string-ext/string=~ rails/rake/tasks-regexp str $1))
                       when task collect task)))
      (files-ext/write-string-to-file (concat root rails/rake/tasks-cache-file) (prin1-to-string tasks))
      tasks)))

(defun rails/rake/list-of-tasks (root)
  "Return all available tasks and create tasks cache file."
  (let* ((cache-file (concat root rails/rake/tasks-cache-file)))
    (if (file-exists-p cache-file)
        (files-ext/read-from-file cache-file)
      (rails/rake/create-tasks-cache root))))


(defun rails/rake/task-run (root task)
  "Run a Rake task in RAILS_ROOT with MAJOR-MODE."
  (when (and task root)
    (let ((keywords (cdr (find task rails/rake/task-keywords-alist :key 'car :test '(lambda(i j) (string-match j i))))))
      (rails/runner/run root rails/rake/command task :keywords keywords)
      (setq rails/runner/after-stop-func-list '(rails/runner/popup-buffer)))))

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
    (let ((task (rails/completing-read "What task run"
                                       (rails/rake/list-of-tasks (rails/root))
                                       nil
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

(provide 'rails-rake-bundle)