(require 'elunit)
(require 'core-ext)

(defsuite core-ext ext)

(deftest when-bind-with-run-block core-ext
  "Test `when-bind' with block passed."
  (when-bind (val t)
    (assert-equal t val))
  (when-bind (val (emacs-version))
    (assert-equal (emacs-version) val)))

(deftest when-bind-with-skip-block core-ext
  "Test `when-bind' with block skiped block."
  (let ((res t))
    (when-bind (val nil)
      (setq res nil))
      (assert-equal t res)))

(deftest define-keys-macro core-ext
  "Test define keys using `define-keys' macro."
  (let ((map (make-sparse-keymap)))
    (assert
     (define-keys map
       ("\C-c a" 'foo)
       ("\C-c b" 'bar)))
    (assert-not-equal map (make-sparse-keymap))
    (assert-equal (lookup-key map "\C-c a") 'foo)
    (assert-equal (lookup-key map "\C-c b") 'bar)))

(deftest funcs-chain core-ext
  (assert-equal "A" (funcall (funcs-chain capitalize string-to-list car char-to-string) "Abcd")))