(require 'elunit)
(require 'inflections)

(defsuite inflections ext)

(deftest can-loaded-inflections inflections
  (assert inflection-singulars)
  (assert inflection-plurals)
  (assert inflection-irregulars)
  (assert inflection-uncountables))

(deftest can-add-singular-inflections inflections
  (let ((inflection-singulars inflection-singulars))
    (assert-changed (length inflection-singulars)
      (define-inflectors
        (:singular "foo$" "bar")
        (:singular "foobar$" "baz")))
    (assert-equal "testbar" (singularize-string "testfoo"))
    (assert-equal "testbaz" (singularize-string "testfoobar"))))

(deftest can-add-plural-inflections inflections
  (let ((inflection-plurals inflection-plurals))
    (assert-changed (length inflection-plurals)
      (define-inflectors
        (:plural "foo$" "bar")
        (:plural "foobar$" "baz")))
    (assert-equal "testbar" (pluralize-string "testfoo"))
    (assert-equal "testbaz" (pluralize-string "testfoobar"))))

(deftest can-add-irregular-inflections inflections
  (let ((inflection-irregulars inflection-irregulars))
    (assert-changed (length inflection-irregulars)
      (define-inflectors
        (:irregular "foo" "bar")
        (:irregular "foobar" "baz")))
    (assert-equal "bar" (pluralize-string "foo"))
    (assert-equal "baz" (pluralize-string "foobar"))
    (assert-equal "foo" (singularize-string "bar"))
    (assert-equal "foobar" (singularize-string "baz"))))

(deftest can-add-uncountable-inflections inflections
  (let ((inflection-uncountables inflection-uncountables))
    (assert-changed (length inflection-uncountables)
      (define-inflectors
        (:uncountable "foo" "bar")))
    (assert-equal "foo" (pluralize-string "foo"))
    (assert-equal "foo" (singularize-string "foo"))
    (assert-equal "bar" (pluralize-string "bar"))
    (assert-equal "bar" (singularize-string "bar"))))
