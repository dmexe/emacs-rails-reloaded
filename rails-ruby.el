(defvar rails/ruby/file-suffix ".rb")

(defun rails/ruby/current-method ()
  (let (action
        (re "^ *def +\\([^ (\n]+\\)"))
    (save-excursion
      (end-of-line)
      (when (re-search-backward re nil t)
        (setq action (buffer-substring-no-properties (match-beginning 1) (match-end 1)))))
    action))

(defun rails/ruby/goto-method-in-current-buffer (action)
    (let* (pos
           (re (format "^ *def +\\(%s\\)" (regexp-quote action))))
    (save-excursion
      (goto-char (point-min))
      (when-bind (action-pos (re-search-forward re nil t))
        (setq pos action-pos)))
    (when pos
      (goto-char pos)
      (beginning-of-line))))

(provide 'rails-ruby)