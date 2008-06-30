(require 'elunit)
(require 'list-ext)

(defsuite list-ext ext)

(deftest list-ext/uniq list-ext
  (assert-equal '(1 2) (list-ext/uniq '(1 1 2 2)))
  (assert-equal '(1 2 3) (list-ext/uniq '(1 1 2 2 3)))
  (assert-equal '(1 2) (list-ext/uniq '(1 2))))

(deftest list-ext/group-by list-ext
  (let ((list '((:a 1)
                (:b 2)
                (:a 3)
                (:b 4)
                (:c 5))))
  (assert-equal '((:a ((:a 1) (:a 3)))
                  (:b ((:b 2) (:b 4)))
                  (:c ((:c 5))))
                (list-ext/group-by
                 list
                 '(lambda (it) (car it))))))
