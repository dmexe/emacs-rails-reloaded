(require 'cl)

;;; ---------------------------------------------------------
;;; - Variables
;;;

(defconst rails/generator-bundle/tasks-cache-file "tmp/.generate-cache")

(defvar rails/generator-bundle/options "-s")
(defvar rails/generator-bundle/history nil)
(defvar rails/generator-bundle/tasks-regexp "^\s+\\(Plugins ([^)]+)\\|Builtin\\|Rubygems\\):\s+\\(.*\\)$"
  "Regexp to match tasks list in `rake --tasks` output.")

(defvar rails/generator-bundle/button-regexp "^\s+create\s+\\(.*\\)$")

(defvar rails/generator-bundle/font-lock-keywords
  '(("^\s+\\(exists\\|notempty\\)\s+\\(.*\\)$"
     (1 font-lock-function-name-face)
     (2 font-lock-comment-delimiter-face))
    ("^\s+\\(create\\|rm\\|rmdir\\)\s+\\(.*\\)$"
     (1 font-lock-function-name-face)
     (2 font-lock-string-face))))

;;; ---------------------------------------------------------
;;; - Functions
;;;

(defun rails/generator-bundle/make-buttons (&rest args)
  (save-excursion
    (while (re-search-forward rails/generator-bundle/button-regexp nil t)
      (let ((root rails/runner/buffer-rails-root)
            (file (match-string 1)))
        (when (and (rails/file-exist-p root file)
                   (not (rails/file-directory-p root file)))
          (make-button (match-beginning 1) (match-end 1)
                       :type 'rails/button
                       :file-name (concat root file))))))
  (rails/button-action (next-button (point-min))))

(defun rails/generator-bundle/create-cache (root)
  "Create a cache file from script/generate output."
  (in-directory root
                (let ((string (rails/proxy/shell-command-to-string
                               root
                               (format
                                "%s script/generate --help"
                                rails/ruby/command)))
                      (pos 0)
                      tasks)
                  (while (string-match rails/generator-bundle/tasks-regexp string pos)
                    (setq pos (match-end 2))
                    (setq tasks
                          (merge 'list
                                 tasks
                                 (split-string (match-string 2 string) ", ")
                                 'string-lessp)))
                  (files-ext/write-string-to-file
                   (concat root rails/generator-bundle/tasks-cache-file)
                   (prin1-to-string tasks))
                  tasks)))

(defun rails/generator-bundle/list-of-tasks (root)
  "Return all available tasks and create tasks cache file."
  (let ((cache-file (concat root rails/generator-bundle/tasks-cache-file)))
    (if (file-exists-p cache-file)
        (files-ext/read-from-file cache-file)
      (rails/generator-bundle/create-cache root))))

(defun rails/generator-bundle/run (root what task options &optional script-options)
  "Run a Rake task in RAILS_ROOT with MAJOR-MODE."
  (when (and task root)
    (unless script-options
      (setq script-options ""))
    (rails/runner/run root
                      rails/ruby/command (format "script/%s %s %s %s"
                                                 what
                                                 task
                                                 options
                                                 script-options)
                      :keywords rails/generator-bundle/font-lock-keywords)

    (setq rails/runner/after-stop-func-list
          '(rails/runner/popup-buffer rails/generator-bundle/make-buttons))))

;;; ---------------------------------------------------------
;;; - Interactives
;;;

(defun rails/generator-bundle/generate (&optional task)
  "Run a Generator task."
  (interactive)
  (rails/with-root nil
    (if task
        (let (options)
          (unless (string-ext/empty-p task)
            (setq options (read-string (format "script/generate %s " task)))
            (unless (string-ext/empty-p options)
              (rails/generator-bundle/run (rails/root)
                                          "generate"
                                          task
                                          options
                                          rails/generator-bundle/options))))
      (rails/anything/run-with-pattern "gen "))))

(defun rails/generator-bundle/destroy (&optional task)
  "Run a Destroy task."
  (interactive)
  (rails/with-root nil
    (if task
        (let (options)
          (unless (string-ext/empty-p task)
            (setq options (read-string (format "script/destroy %s " task)))
            (unless (string-ext/empty-p options)
              (rails/generator-bundle/run (rails/root) "destroy" task options))))
      (rails/anything/run-with-pattern "des "))))

(defun rails/generator-bundle/reset-cache ()
  (interactive)
  (when-bind (root (rails/root))
    (rails/generator-bundle/create-cache root)))

;;; ---------------------------------------------------------
;;; - Bundle
;;;

(rails/defbundle "Generator"
  (:menu
   (([reset]   (cons "Reset Cache" 'rails/generator-bundle/reset-cache))
    ([destroy] (cons "Destroy" 'rails/generator-bundle/destroy))
    ([create]  (cons "Generate" 'rails/generator-bundle/generate)))
   :keys
   (("e" 'rails/generator-bundle/generate)
    ("E" 'rails/generator-bundle/destroy))
   :triggers
   (("gen" "Generate"
     (candidates
      .
      (lambda ()
        (when (string-match "^gen" anything-pattern)
          (mapcar
           (lambda (i)
             (cons (format "gen %s" i) i))
           (rails/generator-bundle/list-of-tasks anything-rails-current-root)))))
     (action ("Run" . rails/generator-bundle/generate))
     (requires-pattern . 3))
    ("des" "Destroy"
     (candidates
      .
      (lambda ()
        (when (string-match "^des" anything-pattern)
          (mapcar
           (lambda (i)
             (cons (format "des %s" i) i))
           (rails/generator-bundle/list-of-tasks anything-rails-current-root)))))
     (action ("Run" . rails/generator-bundle/destroy))
     (requires-pattern . 3)))))
