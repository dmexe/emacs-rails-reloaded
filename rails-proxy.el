(defvar rails/proxy/dir-list
  '(("z:/apps/" "dima-exe.d2.undev.ru" "/home/dima-exe/apps/")))

(defvar rails/proxy/local-root nil)
(defvar rails/proxy/remote-root nil)

(defun rails/proxy/make-command (root command &optional args)
  (let ((loc (files-ext/file-in-directories-p
              (mapcar 'car rails/proxy/dir-list) root)))
    (if loc
        (let ((host (cdr (find loc rails/proxy/dir-list :key 'car :test 'string=)))
              (dir (string-ext/cut root loc :begin)))
          (list "plink"
                (format
                 (concat "-batch %s \"(cd '%s%s' && %s " args ")\"")
                 (first host) (cadr host) dir command)
                host))
      (list command args))))

(defun rails/proxy/remote-p (cmd)
  (string= "plink" (car cmd)))


(defun rails/proxy/shell-command (root name buffer command &optional args)
  (let* ((cmd (rails/proxy/make-command root command args))
         (proc (start-process-shell-command name
                                            buffer
                                            (car cmd)
                                            (cadr cmd))))
    (if (rails/proxy/remote-p cmd)
        (rails/proxy/setup-remote root proc cmd)
      proc)))

(defun rails/proxy/setup-remote (root proc cmd)
  (with-current-buffer (process-buffer proc)
    (set (make-local-variable 'rails/proxy/local-root) root)
    (set (make-local-variable 'rails/proxy/remote-root) (car (nth 2 cmd))))
  proc)

(defun rails/proxy/shell-command-to-string (root command)
  (let ((cmd (rails/proxy/make-command root command)))
    (shell-command-to-string command)))


(provide 'rails-proxy)