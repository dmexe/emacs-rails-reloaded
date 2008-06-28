(defun list-ext/uniq (list)
  "Return a list of unique elements."
  (let ((result '()))
    (dolist (elem list)
      (when (not (member elem result))
        (push elem result)))
    (nreverse result)))

(defun list-ext/group-by (list func)
  (let ((res '()))
    (dolist (it list)
      (let* ((key (funcall func it))
             (res-key (assoc key res)))
        (if res-key
            (push it (cadr res-key))
          (add-to-list 'res (list key (list it))))))
    (mapcar
     #'(lambda(it)
         (list (car it) (nreverse (cadr it))))
     (nreverse res))))

(provide 'list-ext)
