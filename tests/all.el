(load-file
 (concat
  (file-name-directory
   (locate-library "rails-reloaded"))
  "tests/test-helper.el"))

(rails/tests/load "ext/test-helper")

(save-window-excursion
  (rails/tests/load-test "ext" "core-ext")
  (rails/tests/load-test "ext" "files-ext")
  (rails/tests/load-test "ext" "inflections")
  (rails/tests/load-test "ext" "string-ext"))

(elk-test-run-all-buffers t)
