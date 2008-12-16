(require 'string-ext)
(require 'find-recursive)

(defun files-ext/file-special-p (file)
  (let ((file (file-name-nondirectory file)))
    (or (string= file ".")
        (string= file "..")
        (string-ext/start-p file ".#")
        (string-ext/start-p file "#")
        (string-ext/start-p file "~"))))

(defun files-ext/file-in-directory-p (dir file)
  (string-ext/start-p (expand-file-name file)
                      (expand-file-name dir)))

(defun files-ext/file-in-directories-p (dir-list file)
  (loop for dir in dir-list
        when (files-ext/file-in-directory-p dir file)
        return dir))

(defun files-ext/write-string-to-file (file string)
  "Write a string to a file (erasing the previous content)."
  (write-region string nil file))

(defun files-ext/read-from-file (file-name)
  "Read sexpr from a file named FILE-NAME."
  (with-temp-buffer
    (insert-file-contents file-name)
    (read (current-buffer))))

(provide 'files-ext)
