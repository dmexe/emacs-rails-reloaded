(require 'elunit)
(require 'rails-lib)
(require 'rails-reloaded)

(load-file "test-helper.el")

(defsuite rails-lib rails)

(deftest rails/root rails-lib
  (my/each-let (rails/projects)
    ; with current buffer
    ((rails/test/with-file "README"
       (assert-equal rails/test/root (rails/root)))
     (assert-equal rails/projects (list rails/test/root)))
    ; with specific file
    ((assert-equal rails/test/root  (rails/root (rails/test/file "README")))
     (assert-equal rails/projects (list rails/test/root)))
    ; with non rails root file
    ((assert-nil (rails/root (rails/test/file "../../README")))
     (assert-nil rails/projects))))

(deftest rails/with-root rails-lib
  (my/each-let (rails/projects)
    ; with valid files
    ((my/assert-block-call rails/with-root (rails/test/file "README")
      (assert-equal rails/test/root (rails/root))
      (assert-equal rails/projects (list rails/test/root))))
    ; with invalid files
    ((my/assert-block-not-call rails/with-root (rails/test/file "../../README")
      (assert-nil rails/projects)))))

(deftest rails/when-root rails-lib
  ; with valid files
  (my/assert-block-call rails/when-root (rails/test/file "README"))
  ; with invalid files
  (my/assert-block-not-call rails/when-root (rails/test/file "../../README")))

(deftest rails/cut-root rails-lib
  ; with absolute file name
  (assert-equal "README" (rails/cut-root (rails/test/file "README")))
  ; with relative file name
  (assert-equal "../README" (rails/cut-root "../README")))

(deftest rails/find-existing-root-for rails-lib
  (my/each-let ((rails/projects (list rails/test/root)))
    ; valid rails file
    ((assert-equal rails/test/root (rails/find-existing-root-for (rails/test/file "README"))))
    ; valid relative rails file
    ((assert-equal rails/test/root (rails/find-existing-root-for (rails/test/file "app/../README"))))
    ; invalid rails file
    ((assert-nil (rails/find-existing-root-for (rails/test/file "../../README")))))
  (let (rails/projects)
    ; valid file without existing projects
    (assert-nil (rails/find-existing-root-for (rails/test/file "README")))))

(deftest rails/find-root-for rails-lib
  (my/each-let (rails/projects)
    ; valid rails file
    ((assert-equal rails/test/root (rails/find-root-for (rails/test/file "README"))))
    ; valid relative rails file
    ((assert-equal rails/test/root (rails/find-root-for (rails/test/file "app/../README"))))
    ; invalid rails file
    ((assert-nil (rails/find-root-for (rails/test/file "../../README"))))))

(deftest rails/bundle-func rails-lib
  (flet ((rails/foobundle/baz () t))
    (assert-equal 'rails/foobundle/baz (rails/bundle-func 'foobundle "baz"))
    (assert-nil (rails/bundle-func 'foobundle "bar"))
    (assert-nil (rails/bundle-func 'bazbundle "bar"))))

(deftest rails/bundle-func-by-buffer rails-lib
  (flet ((rails/foobundle/baz () t))
    (let ((buf (make-rails/buffer :type :foobundle)))
      (assert-equal 'rails/foobundle/baz (rails/bundles-func-by-buffer buf "baz"))
      (assert-nil (rails/bundles-func-by-buffer buf "bar"))
      (setf (rails/buffer-type buf) :barbundle)
      (assert-nil (rails/bundles-func-by-buffer buf "baz")))))

(deftest rails/bundles-func rails-lib
  ; empty bundles
  (let (rails/bundles-func-list rails/bundles-list)
    (assert-nil (rails/bundles-func "foo")))
  (let ((rails/bundles-list '(foo bar))
        rails/bundles-func-list)
    ; caching bundles func
    (flet ((rails/foo/baz () t))
      (flet ((rails/bar/baz () t))
        (assert-equal '(rails/foo/baz rails/bar/baz)
                      (rails/bundles-func "baz"))
        (assert-equal '(("baz" (rails/foo/baz rails/bar/baz)))
                      rails/bundles-func-list)))
    ;; cached bundles func
    (assert-equal '(rails/foo/baz rails/bar/baz)
                  (rails/bundles-func "baz"))))

(deftest rails/file-exist-p rails-lib
  (assert (rails/file-exist-p rails/test/root "README"))
  (assert-nil (rails/file-exist-p rails/test/root "../README")))

(deftest rails/find-file rails-lib
  (rails/test/with-buffer
    (rails/find-file rails/test/root "README")
    (assert-equal (buffer-file-name) (rails/test/file "README"))))

(deftest rails/group-by-goto-item-group rails-lib
  (let* ((item-a (make-rails/goto-item :name "a"))
         (item-b (make-rails/goto-item :name "b"))
         (item-c (make-rails/goto-item :name "c" :group :c))
         (items (list item-a item-b item-c))
         (expected (list (list nil (list item-a item-b))
                         (list :c  (list item-c)))))
    (assert-equal expected (rails/group-by-goto-item-group items))))

(deftest rails/directory-to-goto-menu rails-lib
  ; default
  (let ((expected
         (list
          (list nil
                (list (make-rails/goto-item :name "application.rb" :file "app/controllers/application.rb")
                      (make-rails/goto-item :name "posts_controller.rb" :file "app/controllers/posts_controller.rb"))))))
    (flet ((x-popup-menu (&rest args) nil))
      (assert-equal
       expected
       (rails/directory-to-goto-menu rails/test/root
                                     "app/controllers/"
                                     "title"))))

  ; name-by
  (let ((expected
         (list
          (list nil
                (list (make-rails/goto-item :name "application" :file "app/controllers/application.rb")
                      (make-rails/goto-item :name "posts_controller" :file "app/controllers/posts_controller.rb"))))))
    (flet ((x-popup-menu (&rest args) nil))
      (assert-equal
       expected
       (rails/directory-to-goto-menu rails/test/root
                                     "app/controllers/"
                                     "title"
                                     :name-by 'file-name-sans-extension))))

  ; filter-by
  (let ((expected
         (list
          (list nil
                (list (make-rails/goto-item :name "posts_controller" :file "app/controllers/posts_controller.rb"))))))
    (flet ((x-popup-menu (&rest args) nil))
      (assert-equal
       expected
       (rails/directory-to-goto-menu rails/test/root
                                     "app/controllers/"
                                     "title"
                                     :name-by 'file-name-sans-extension
                                     :filter-by '(lambda(file) (string-ext/end-p file "_controller.rb" ) )))))
  ; append
  (let* ((append-item
          (make-rails/goto-item :name "append" :file "README" :group :default))
         (expected
          (list
           (list nil
                 (list (make-rails/goto-item :name "application.rb" :file "app/controllers/application.rb")
                       (make-rails/goto-item :name "posts_controller.rb" :file "app/controllers/posts_controller.rb")))
           (list :default
                 (list append-item)))))
    (flet ((x-popup-menu (&rest args) nil))
      (assert-equal
       expected
       (rails/directory-to-goto-menu rails/test/root
                                     "app/controllers/"
                                     "title"
                                     :append (list append-item))))))
