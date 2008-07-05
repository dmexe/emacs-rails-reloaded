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
           (cur-pos (point))
           (re (format "^ *def +\\<\\(%s\\)\\>" (regexp-quote action))))
    (save-excursion
      (goto-char (point-min))
      (when-bind (start-pos (re-search-forward re nil t))
        (setq pos start-pos)
        (when (fboundp 'ruby-end-of-defun)
          (ruby-end-of-defun)
          (when (and (< cur-pos (point))
                     (> cur-pos start-pos))
            (setq pos nil)))))
    (when pos
      (goto-char pos)
      (beginning-of-line))))

(provide 'rails-ruby)