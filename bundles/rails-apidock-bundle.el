(defconst rails/apidock-bundle/buffer-name "*rails-apidock-bundle*")
(defconst rails/apidock-bundle/script-file-name "bundles/rails-apidock-bundle/search.rb")

(defun rails/apidock-bundle/get-proc ()
  (let ((proc (get-buffer-process rails/apidock-bundle/buffer-name)))
    (unless proc
      (setq
       proc
       (start-process rails/apidock-bundle/buffer-name
                      rails/apidock-bundle/buffer-name
                      rails/ruby/command
                      (rails/apidock-bundle/locate-script)))
      (set-process-query-on-exit-flag proc nil))
    proc))

(defun rails/apidock-bundle/get-result (mod query)
  (let ((proc (rails/apidock-bundle/get-proc)))
    (with-current-buffer rails/apidock-bundle/buffer-name
      (kill-region (point-min) (point-max))
      (process-send-string proc (format "%s %s\n" mod query))
      (while
          (progn
            (sleep-for 0 100)
            (goto-char (point-max))
            (goto-char (point-at-bol 0))
            (not (string= "APIDOCK_EOF"
                          (buffer-substring (line-beginning-position)
                                            (line-end-position))))))
      (read (buffer-substring-no-properties (point-min)
                                            (point))))))

(defun rails/apidock-bundle/locate-script ()
  (let* ((path (file-name-directory (locate-library "rails-reloaded")))
         (script (concat path rails/apidock-bundle/script-file-name)))
    (when (file-exists-p script)
      script)))

(defun rails/apidock-bundle/search (query &optional mod)
  (let ((mod (or mod "rails")))
    (rails/apidock-bundle/get-result mod query)))

(rails/defbundle "Apidock"
  (:triggers
   (("apidock-rails" "Search on apidock.com/rails"
     (candidates
      .
      (lambda ()
        (let ((rs (rails/apidock-bundle/search anything-pattern "rails")))
          rs)))
     (action ("Browse Url" . browse-url))
     (requires-pattern . 3)
     (candidate-number-limit . 10)
     (volatile)
     (delayed))
    ("apidock-rspec" "Search on apidock.com/rspec"
     (candidates
      .
      (lambda ()
        (let ((rs (rails/apidock-bundle/search anything-pattern "rspec")))
          rs)))
     (action ("Browse Url" . browse-url))
     (requires-pattern . 3)
     (candidate-number-limit . 10)
     (volatile)
     (delayed)))))
