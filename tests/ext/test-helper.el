;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; core-ext
(defun define-keys-test-map ()
  (let ((map (make-sparse-keymap)))
    (define-keys map
      ("\C-c a" 'foo)
      ("\C-c b" 'bar))))
