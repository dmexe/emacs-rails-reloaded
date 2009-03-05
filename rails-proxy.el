;;; rails-proxy.el ---

;; Copyright (C) 2006 Dmitry Galinsky <dima dot exe at gmail dot com>

;; Authors: Dmitry Galinsky <dima dot exe at gmail dot com>,

;;; License

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

;;; Code:

(require 'cl)

(defvar rails/proxy/ssh "ssh")
(defvar rails/proxy/ssh-args "-t -t -q")
(defvar rails/proxy/tunnel-local-port "80")
(defvar rails/proxy/tunnel-args "-t -t -q -L %s:127.0.0.1:%s %s")
(defvar rails/proxy/tunnel-buffer "*tunnel %s*")

(defvar rails/proxy/dir-list
  '(("z:/apps/" "dima-exe@d2.undev.ru" "/home/dima-exe/apps/")))

(defvar rails/proxy/local-root nil)
(defvar rails/proxy/remote-root nil)

(defun rails/proxy/remote-p (dir)
  (files-ext/file-in-directories-p
   (mapcar 'car rails/proxy/dir-list) dir))

(defun rails/proxy/remote-list (dir)
  (find (rails/proxy/remote-p dir)
        rails/proxy/dir-list
        :key 'car :test 'string=))

(defun rails/proxy/make-command (root command &optional args)
  (if (rails/proxy/remote-p root)
      (let* ((plist (rails/proxy/remote-list root))
             (dir (car plist))
             (rdir (concat (caddr plist) (string-ext/cut root dir :begin)))
             (host (cadr plist)))
        (list rails/proxy/ssh
              (format (concat "%s %s \"(cd %s && %s " args ")\"")
                      rails/proxy/ssh-args
                      host
                      rdir
                      command)
              host))
    (list command args)))

(defun rails/proxy/shell-command (root name buffer command &optional args)
  (let* ((cmd (rails/proxy/make-command root command args))
         (proc (start-process-shell-command name
                                            buffer
                                            (car cmd)
                                            (cadr cmd))))
    (if (rails/proxy/remote-p root)
        (rails/proxy/setup-remote root proc)
      proc)))

(defun rails/proxy/setup-remote (root proc)
  (with-current-buffer (process-buffer proc)
    (let ((plist (rails/proxy/remote-list root)))
      (set (make-local-variable 'rails/proxy/local-root) (car plist))
      (set (make-local-variable 'rails/proxy/remote-root) (caddr plist)))
    proc))

(defun rails/proxy/shell-command-to-string (root command)
  (let ((cmd (rails/proxy/make-command root command)))
    (shell-command-to-string
     (if (cadr cmd)
         (format "%s %s" (car cmd) (cadr cmd))
       (car cmd)))))

(defun rails/proxy/up-tinnel-if-need (root remote-port)
  (when-bind (plist (rails/proxy/remote-list root))
    (let ((args (format rails/proxy/tunnel-args
                        rails/proxy/tunnel-local-port
                        remote-port
                        (cadr plist)))
          (name (format rails/proxy/tunnel-buffer remote-port)))
      (unless (get-buffer-process name)
        (start-process-shell-command name
                                     name
                                     rails/proxy/ssh
                                     args)))))

(defun rails/proxy/down-tunnel-if-need (remote-port)
  (let ((name (format rails/proxy/tunnel-buffer remote-port)))
    (when-bind (proc (get-buffer-process name))
      (kill-process proc))))

(provide 'rails-proxy)