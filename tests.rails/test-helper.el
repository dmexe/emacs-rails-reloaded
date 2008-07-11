(defconst rails/test/root (expand-file-name "../tests.railsapp/"))

(defmacro rails/test/with-file (file &rest body)
  `(save-window-excursion
     (with-temp-buffer
       (find-file (rails/test/file ,file))
       ,@body
       (kill-buffer (current-buffer)))))

(defmacro rails/test/with-buffer (&rest body)
  `(save-window-excursion
     (with-temp-buffer
       ,@body
       (kill-buffer (current-buffer)))))

(defun rails/test/file (file)
  (concat rails/test/root file))

(defmacro my/each-let (let-exp &rest body)
  `(progn
     ,@(mapcar
        #'(lambda (expr)
            `(let ,let-exp
               ,(car expr)))
        body)))

(defmacro my/assert-block-call (block-name block-args &rest body)
  `(let ((assert-val
          (format "%s can't called" ',block-name)))
     (,block-name ,block-args
       (setq assert-val nil)
       ,@body)
     (assert-nil assert-val)))

(defmacro my/assert-block-not-call (block-name block-args &rest body)
  `(let (assert-val)
     (,block-name ,block-args
       (setq assert-val (format "%s called" ',block-name)))
     (assert-nil assert-val)
     ,@body))
