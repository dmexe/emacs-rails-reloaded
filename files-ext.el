(require 'string-ext)


(defun files-ext/file-special-p (file)
  (let ((file (file-name-nondirectory file)))
    (or (string-match "^[.#~]" file)
        (string-match "[#~]$" file))))

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

(defun files-ext/directory-files-recursive (dir regexp)
  (let ((dir-stack '())
        (files-stack '())
        (res '())
        (dir (if (string-ext/end-p dir "/")
                 dir
               (concat dir "/")))
        cur-dir
        fst
        it
        done)
    (push "" dir-stack)
    (while (car dir-stack)
      (setq done t)
      (setq cur-dir (car dir-stack))
      (push (directory-files (concat dir cur-dir) nil regexp) files-stack)
      (while (and (> (length files-stack) 0)
                  done)
        (setq fst (pop files-stack))
        (while (and (car fst)
                    done)
          (setq it (car fst))
          (setq fst (cdr fst))
          (unless (files-ext/file-special-p it)
            (if (file-directory-p (concat dir cur-dir it))
                (progn
                  (push fst files-stack)
                  (push (concat cur-dir it "/") dir-stack)
                  (setq done nil))
              (progn
                (setq res (append res (list (concat cur-dir it))))))))
        (when done
          (pop dir-stack)
          (setq cur-dir (car dir-stack)))))
    res))

(provide 'files-ext)
