(defconst rails/apidock-bundle/script-file-name "bundles/rails-apidock-bundle/search.rb")

(defun rails/apidock-bundle/locate-script ()
  (let* ((path (file-name-directory (locate-library "rails-reloaded")))
         (script (concat path rails/apidock-bundle/script-file-name)))
    (when (file-exists-p script)
      script)))

(defun rails/apidock-bundle/search (q)
  (let ((result (shell-command-to-string
                 (format "ruby %s %s"
                         (rails/apidock-bundle/locate-script)
                         q))))
    (unless (string-ext/empty-p result)
      (read result))))

(rails/defbundle "Apidock"
  (:triggers
   (("apidock" "Search on Apidock"
     (candidates
      .
      (lambda ()
        (let ((rs (rails/apidock-bundle/search anything-pattern)))
          rs)))
     (action ("Goto" . browse-url))
     (requires-pattern . 3)
     (candidate-number-limit . 10)
     (volatile)
     (delayed)))))

(provide 'rails-apidock-bundle)
