(defun files-ext/directory-of-file(file-name)
  "Return the parent directory of a file named FILE-NAME."
  (replace-regexp-in-string "[^/]*$" "" file-name))

(provide 'files-ext)