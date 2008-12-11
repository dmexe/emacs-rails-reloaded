(defun list-ext/uniq (list)
  "Return a list of unique elements."
  (let ((result '()))
    (dolist (elem list)
      (when (not (member elem result))
        (push elem result)))
    (nreverse result)))

(defun list-ext/group-by (list func &optional sort)
  (let ((res '()))
    (dolist (it list)
      (let* ((key (funcall func it))
             (res-key (assoc key res)))
        (if res-key
            (push it (cadr res-key))
          (add-to-list 'res (list key (list it))))))
    (setq res
          (mapcar
           #'(lambda(it)
               (list (car it) (nreverse (cadr it))))
           (nreverse res)))
    (when sort
      (setq res (sort* res sort :key 'car)))
    res))

(defun list-ext/options-value (key list)
  (cadr (memq key list)))

(defalias 'opt-val 'list-ext/options-value)

(defun list-ext/swap-tail (key list)
  (let* ((list-len (length list))
         (tail (member key list))
         (beg (- list-len (length tail)))
         (i 0))
    (when tail
      (while (not (zerop (+ beg 1)))
        (add-to-list 'tail (nth i list) t)
        (decf beg)
        (incf i))
      tail)))

(provide 'list-ext)
