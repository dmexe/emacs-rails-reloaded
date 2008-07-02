(require 'string-ext)
(require 'find-recursive)

(defun files-ext/find-recursive-files (filter-func regexp directory)
  "Return a list of files, found in DIRECTORY and match them to FILE-REGEXP."
  (let ((files
         (find-recursive-filter-out
          find-recursive-exclude-files
          (find-recursive-directory-relative-files directory "" regexp)))
        (res))
    (dolist (it files)
      (when-bind (obj (funcall filter-func it))
        (add-to-list 'res obj t)))
    res))

(defun files-ext/file-special-p (file)
  (let ((file (file-name-nondirectory file)))
    (or (string= file ".")
        (string= file "..")
        (string-ext/start-p file "#")
        (string-ext/start-p file "~"))))

(defun files-ext/file-in-directory-p (dir file)
  (string-ext/start-p (expand-file-name file)
                      (expand-file-name dir)))

(defun files-ext/file-in-directories-p (dir-list file)
  (let (res)
    (mapcar
     '(lambda (it)
        (when (files-ext/file-in-directory-p it file)
          (setq res t)))
     dir-list)
    res))

(provide 'files-ext)
