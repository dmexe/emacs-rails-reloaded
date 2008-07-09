;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Constants
;;

(defconst rails/generator/tasks-cache-file "tmp/.generate-cache")

(defvar rails/generator/history nil)
(defvar rails/generator/tasks-regexp "^\s+\\(Plugins\s+([^)]+)\\|Builtin\\):\s+\\(.*\\)$"
  "Regexp to match tasks list in `rake --tasks` output.")

(defvar rails/generator/button-regexp "^\s+create\s+\\(.*\\)$")

(defvar rails/generator/font-lock-keywords
  '(("^\s+\\(exists\\|notempty\\)\s+\\(.*\\)$"
     (1 font-lock-function-name-face)
     (2 font-lock-comment-delimiter-face))
    ("^\s+\\(create\\|rm\\|rmdir\\)\s+\\(.*\\)$"
     (1 font-lock-function-name-face)
     (2 font-lock-string-face))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Functions
;;

(defun rails/generator/make-buttons (&rest args)
  (save-excursion
    (while (re-search-forward rails/generator/button-regexp nil t)
      (let ((root rails/runner/buffer-rails-root)
            (file (match-string 1)))
        (when (and (rails/file-exist-p root file)
                   (not (rails/file-directory-p root file)))
          (make-button (match-beginning 1) (match-end 1)
                       :type 'rails/button
                       :file-name (concat root file))))))
  (rails/button-action (next-button (point-min))))

(defun rails/generator/create-cache (root)
  "Create a cache file from script/generate output."
  (in-directory root
    (let ((string (shell-command-to-string (format "%s script/generate --help" rails/ruby/command)))
          (pos 0)
          tasks)
      (while (string-match rails/generator/tasks-regexp string pos)
        (setq pos (match-end 2))
        (setq tasks (merge 'list tasks (split-string (match-string 2 str) ", ") 'string-lessp)))
      (files-ext/write-string-to-file (concat root rails/generator/tasks-cache-file) (prin1-to-string tasks))
    tasks)))

(defun rails/generator/list-of-tasks (root)
  "Return all available tasks and create tasks cache file."
  (let ((cache-file (concat root rails/generator/tasks-cache-file)))
    (if (file-exists-p cache-file)
        (files-ext/read-from-file cache-file)
      (rails/generator/create-cache root))))

(defun rails/generator/run (root what task options)
  "Run a Rake task in RAILS_ROOT with MAJOR-MODE."
  (when (and task root)
    (rails/runner/run root
                      rails/ruby/command (format "script/%s %s %s" what task options)
                      :keywords rails/generator/font-lock-keywords)
    
    (setq rails/runner/after-stop-func-list
          '(rails/runner/popup-buffer rails/generator/make-buttons))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Callbacks
;;

(defun rails/generator/load ()
  (rails/define-key "e" 'rails/generator/generate)
  (rails/define-key "x" 'rails/generator/destroy)
  (let ((map (make-sparse-keymap)))
    (define-keys map
      ([reset] (cons "Reset Cache" 'rails/generator/reset-cache))
      ([destroy] (cons "Destroy" 'rails/generator/destroy))
      ([generate] (cons "Generate" 'rails/generator/generate)))
    (rails/add-to-bundles-menu "Generator" map)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Interactives
;;

(defun rails/generator/generate ()
  "Run a Generator task."
  (interactive)
  (when-bind (root (rails/root))
    (let ((task (rails/completing-read "What generate"
                                       (rails/generator/list-of-tasks root)
                                       nil
                                       (car rails/generator/history)
                                       'rails/generator/history))
          options)
      (unless (string-ext/empty-p task)
        (setq options (read-string (format "Name and/or options for [%s]: " task)))
        (unless (string-ext/empty-p options)
          (rails/generator/run root "generate" task options))))))

(defun rails/generator/destroy ()
  "Run a Destroy task."
  (interactive)
  (when-bind (root (rails/root))
    (let ((task (rails/completing-read "What destroy"
                                       (rails/generator/list-of-tasks root)
                                       nil
                                       (car rails/generator/history)
                                       'rails/generator/history))
          options)
      (unless (string-ext/empty-p task)
        (setq options (read-string (format "Name of [%s]: " task)))
        (unless (string-ext/empty-p options)
          (rails/generator/run root "destroy" task options))))))

(defun rails/generator/reset-cache ()
  (interactive)
  (when-bind (root (rails/root))
    (rails/generator/create-tasks-cache root)))

(provide 'rails-generator-bundle)