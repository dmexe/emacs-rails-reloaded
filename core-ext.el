(defmacro* when-bind ((var expr) &rest body)
  "Binds VAR to the result of EXPR.
If EXPR is not nil exeutes BODY.

 (when-bind (var (func foo))
  (do-somth (with var)))."
  `(let ((,var ,expr))
     (when ,var
       ,@body)))

(defmacro define-keys (key-map &rest key-funcs)
  "Define key bindings for KEY-MAP (create KEY-MAP, if it does
not exist."
  `(progn
     (unless (boundp ',key-map)
       (setf ,key-map (make-keymap)))
     ,@(mapcar
  #'(lambda (key-func)
      `(define-key ,key-map ,(first key-func) ,(second key-func)))
  key-funcs)))

(provide 'core-ext)