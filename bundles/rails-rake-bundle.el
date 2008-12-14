(require 'cl)

;;; ---------------------------------------------------------
;;; - Variables
;;;

(defconst rails/rake-bundle/command "rake")
(defconst rails/rake-bundle/tasks-cache-file "tmp/.tasks-cache")
(defvar rails/rake-bundle/history nil)
(defvar rails/rake-bundle/tasks-regexp "^rake\s+\\([^ ]+\\)\s+"
  "Regexp to match tasks list in `rake --tasks` output.")

(defvar rails/rake-bundle/task-keywords-alist
  '(("^-D$"        . (("^\s+.*"
                       (0 font-lock-comment-face))
                      ("\\(rake\\) \\([^ ]+\\)"
                       (1 font-lock-function-name-face)
                       (2 font-lock-string-face))))
    ("^notes.*$"   . (("^\\([^ ]+\\):"
                       (0 font-lock-string-face))
                      ("^ +\\(\\*\\) \\(\\[[ 0-9]+\\]\\( \\[\\w+\\]\\)?\\) \\(.*\\)$"
                       (1 font-lock-function-name-face)
                       (2 font-lock-constant-face)
                       (4 font-lock-comment-face))))))

(defvar rails/rake-bundle/task-after-stop-alist
  '(("^-D$"        . rails/rake-bundle/after-stop-task-list)
    ("^notes.*$"   . rails/rake-bundle/after-stop-notes)))

(defvar rails/rake-bundle/task-name nil)

;;; ---------------------------------------------------------
;;; - Functions
;;;

(defun rails/rake-bundle/create-tasks-cache (root)
  "Create a cache file from rake --tasks output."
  (in-directory root
                (let ((tasks (loop for str in (split-string (rails/proxy/shell-command-to-string
                                                             root "rake --tasks")
                                                            "\n")
                                   for task = (unless (string-ext/empty-p str)
                                                (string-ext/string=~ rails/rake-bundle/tasks-regexp str $1))
                                   when task collect task)))
                  (files-ext/write-string-to-file
                   (concat root rails/rake-bundle/tasks-cache-file)
                   (prin1-to-string tasks))
                  tasks)))

(defun rails/rake-bundle/list-of-tasks (root)
  "Return all available tasks and create tasks cache file."
  (let* ((cache-file (concat root rails/rake-bundle/tasks-cache-file)))
    (if (file-exists-p cache-file)
        (files-ext/read-from-file cache-file)
      (rails/rake-bundle/create-tasks-cache root))))


(defun rails/rake-bundle/task-run (root task &optional args keywords after-stop-funcs)
  "Run a Rake task in RAILS_ROOT with MAJOR-MODE."
  (when (and task root)
    (unless keywords
      (setq keywords
            (cdr (find task rails/rake-bundle/task-keywords-alist
                       :key 'car
                       :test '(lambda(i j) (string-match j i))))))
    (rails/runner/run root
                      rails/rake-bundle/command
                      (if args
                          (format "RAILS_ENV=%s %s %s" rails/default-environment task args )
                        (format "RAILS_ENV=%s %s" rails/default-environment task))
                      :keywords keywords)
    (with-current-buffer rails/runner/buffer-name
      (set (make-local-variable 'rails/rake-bundle/task-name) task))
    (rails/notify (format "Run task %s in %s" task rails/default-environment) :notice)
    (if after-stop-funcs
        (progn
          (setq rails/runner/after-stop-func-list after-stop-funcs))
      (progn
        (setq rails/runner/after-stop-func-list '(rails/runner/popup-buffer))
        (setq after-stop-funcs
              (cdr (find task rails/rake-bundle/task-after-stop-alist
                         :key 'car
                         :test '(lambda(i j) (string-match j i)))))
        (when after-stop-funcs
          (add-to-list 'rails/runner/after-stop-func-list after-stop-funcs t))))
    (add-to-list 'rails/runner/after-stop-func-list 'rails/rake-bundle/after-stop-notify t)))

;;; ---------------------------------------------------------
;;; - After stop functions
;;;

(defun rails/rake-bundle/after-stop-notify (ret-val)
  (if (zerop ret-val)
      (rails/notify (format "Task %s was successfuly stopped" rails/rake-bundle/task-name) :notice)
    (rails/notify (format "Failed to run task %s" rails/rake-bundle/task-name) :notice)))

(defun rails/rake-bundle/after-stop-task-list (&rest args)
  (save-excursion
    (while (re-search-forward rails/rake-bundle/tasks-regexp nil t)
      (let ((root rails/runner/buffer-rails-root)
            (task (match-string 1)))
        (make-button (match-beginning 1) (match-end 1)
                     :type 'rails/button
                     :task task
                     :func (lambda(ov)(rails/rake-bundle/run (overlay-get ov :task))))))))


(defun rails/rake-bundle/after-stop-notes (&rest args)
  (save-excursion
    (while (re-search-forward "^\\(\\w.*\\):$" nil t)
      (let ((root rails/runner/buffer-rails-root)
            (file (match-string 1))
            line)
        (make-button (match-beginning 1) (match-end 1)
                     :type 'rails/button
                     :file-name (concat root file))
        (setq line (buffer-substring-no-properties
                    (line-beginning-position)
                    (line-end-position)))
        (while (not (string-ext/empty-p line))
          (when (string-match "^\s+\\*\s+\\[\\([ 0-9]+\\)\\]" line)
            (make-button (+ (line-beginning-position) (match-beginning 1))
                         (+ (line-beginning-position) (match-end 1))
                         :type 'rails/button
                         :file-name (concat root file)
                         :line (string-to-number (match-string 1 line))))
          (goto-char (+ (line-end-position) 1))
          (setq line (buffer-substring-no-properties
                      (line-beginning-position)
                      (line-end-position))))))))

;;; ---------------------------------------------------------
;;; - Interactives
;;;

(defun rails/rake-bundle/run (&optional task)
  "Run a Rake task."
  (interactive)
  (when-bind (root (rails/root))
    (let ((task (if task
                    task
                  (rails/completing-read "What task run"
                                         (rails/rake-bundle/list-of-tasks root)
                                         nil
                                         (car rails/rake-bundle/history)
                                         'rails/rake-bundle/history))))
      (when (not (string-ext/empty-p task))
        (rails/rake-bundle/task-run root task)))))

(defun rails/rake-bundle/run-with-args ()
  "Run a Rake task with arguments ARGS."
  (interactive)
  (when-bind (root (rails/root))
    (let ((task (rails/completing-read "What task run"
                                       (rails/rake-bundle/list-of-tasks root)
                                       nil
                                       (car rails/rake-bundle/history)
                                       'rails/rake-bundle/history))
          args)
      (when (not (string-ext/empty-p task))
        (setq args (read-string (format "Arguments fo %s: " task)))
        (rails/rake-bundle/task-run root task args)))))

(defun rails/rake-bundle/reset-cache ()
  "Reset tasks cache."
  (interactive)
  (when-bind (root (rails/root))
    (rails/rake-bundle/create-tasks-cache root)))

(defun rails/rake-bundle/list-tasks ()
  "List of availabled Rake tasks."
  (interactive)
  (when-bind (root (rails/root))
    (rails/rake-bundle/task-run root "-D")))

;;; ---------------------------------------------------------
;;; - Bundle
;;;

(rails/defbundle "Rake"
  (:menu
   (([reset]     (cons "Reset Tasks Cache" 'rails/rake-bundle/reset-cache))
    ([list]      (cons "List Tasks" 'rails/rake-bundle/list-tasks))
    ([task-args] (cons "Run Rake Task with Arguments" 'rails/rake-bundle/run-with-args))
    ([task]      (cons "Run Rake Task" 'rails/rake-bundle/run)))
   :keys
   (("r"    'rails/rake-bundle/run)
    ("R"    'rails/rake-bundle/run-with-args)
    ("\C-r" 'rails/rake-bundle/list-tasks))))
