;;; rails-lib.el --- library functions used in rails.

;; Copyright (C) 2006 Dmitry Galinsky <dima dot exe at gmail dot com>

;; Authors: Dmitry Galinsky <dima dot exe at gmail dot com>,
;;          Rezikov Peter <crazypit13 (at) gmail.com>

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

(require 'files-ext)
(require 'string-ext)
(require 'list-ext)

;;; ---------------------------------------------------------
;;; - Variables
;;;

(defvar rails/projects '())


(defcustom rails/search-files-in-dirs nil
  "Set this variable to search rails files only in them."
  :group 'rails
  :type '(repeat (directory :tag "Directory")))


;;; ---------------------------------------------------------
;;; - rails/root functions
;;;

(defun rails/root (&optional file)
  "Return RAILS_ROOT for FILE, if FILE not set using `buffer-file-name' or `default-directory' instead it.
If RAILS_ROOT not found, return nil."
  (let ((file (or file
                  (buffer-file-name)
                  default-directory)))
    (unless (and rails/search-files-in-dirs
                 (files-ext/file-in-directories-p rails/search-files-in-dirs
                                                  file))
      (or
       (rails/find-existing-root-for file)
       (rails/find-root-for file)))))

(defmacro* rails/with-root (&optional file &body body)
  "If you use `rails-project:root' or functions related on it
several times in a block of code, you can optimize your code by
using this macro. Also, blocks of code will be executed only if
rails-root exist.
 (rails/with-root (root)
    (foo root)
    (bar (rails-core:file \"some/path\")))
 "
  (let ((root (gensym)))
   `(let ((,root (rails/root ,file)))
      (when ,root
        (flet ((rails/root (&optional file) ,root))
          ,@body)))))

(defmacro rails/when-root (file &rest body)
  `(when (rails/root ,file)
       ,@body))

(defun rails/cut-root (file)
  (if (file-name-absolute-p file)
      (string-ext/cut file (rails/root file) :begin)
    file))

(defun rails/find-existing-root-for(file)
  "Search RAILS_ROOT for FILE in `rails/projects' and return,
else return nil"
  (let ((file (expand-file-name file))
        (project (car rails/projects))
        (projects (cdr rails/projects))
        (root))
    (while (and (not root)
                project)
      (if (string-ext/start-p file project)
          (setq root project)
        (progn
          (setq project (car projects))
          (setq projects (cdr projects)))))
    root))

(defun rails/find-root-for (file)
  "Return RAILS_ROOT if FILE is a part of a Rails application,
else return nil"
  (when file
    (let ((curdir (file-name-directory (expand-file-name file)))
          (max 10)
          (found nil))
      (while (and (not found) (> max 0))
        (progn
          (if (file-exists-p (concat curdir "config/environment.rb"))
              (progn
                (setq found t))
            (progn
              (setq curdir (concat curdir "../"))
              (setq max (- max 1))))))
      (when found
        (let ((root (expand-file-name curdir)))
          (setq rails/projects (list-ext/uniq (add-to-list 'rails/projects root)))
          root)))))

(defmacro rails/with-current-buffer (&rest body)
  `(if (and (rails/resource-buffer-p rails/current-buffer)
            (rails/resource-buffer-title rails/current-buffer))
       (rails/with-root (buffer-file-name)
         ,@body)
     (rails/notify "Curent buffer it's not a rails resource." :error)))

;;; ---------------------------------------------------------
;;; - Rails files functions
;;;

(defun rails/file-exist-p (root file)
  (file-exists-p (concat root file)))

(defun rails/file-directory-p (root dir)
  (file-directory-p (concat root dir)))

(defun rails/find-file (root file)
  (find-file (concat root file)))

(defun rails/directory-files (root directory &optional full match nosort)
  (let ((fullname (concat root "/" directory)))
    (when (file-directory-p fullname)
      (loop for file in (directory-files fullname full match nosort)
            for allow = (not (files-ext/file-special-p file))
            when allow
            collect file))))

(defun rails/directory-files-recursive (root directory &optional regexp)
  (let ((fullname (concat root "/" directory)))
    (when (file-directory-p fullname)
      (loop for file in (files-ext/directory-files-recursive
                         fullname regexp)
            for allow = (not (files-ext/file-special-p file))
            when allow
            collect file))))

;;; ---------------------------------------------------------
;;; - Menu functions
;;;

(defun rails/display-menu (title menu &optional force-ido)
  (let ((func
         (cond
          ((and window-system
                (not force-ido)
                (eq rails/display-menu-method 'popup))
           'rails/display-menu-using-popup)
          (t
           'rails/display-menu-using-ido))))
    (funcall func title menu)))

(defun rails/display-menu-using-popup (title menu)
  (add-to-list 'menu title)
  (x-popup-menu (list '(300 50) (get-buffer-window (current-buffer)))
                (list title
                      menu)))

(defun rails/display-menu-using-ido (title menu &optional dont-require-match)
  (let (choices value)
    (dolist (item menu) ; filter
      (let ((name (car item))
            (goto (cdr item)))
        (unless (and (stringp goto)
                     (string= "--" goto))
          (add-to-list 'choices (cons name goto) t))))
    (when-bind (value
                (ido-completing-read (format "%s: " title)
                                     (mapcar 'car choices)
                                     nil
                                     (not dont-require-match)))
      (cdr (find value choices :test 'string= :key 'car)))))

(defun rails/completing-read (title &optional list require default history)
  (let ((title (if default
                   (format "%s [%s]: " title default)
                 (format "%s: " title))))
    (completing-read title list nil require nil history default)))

(defun rails/button-action (ov)
  (when (overlayp ov)
    (let ((file-name (overlay-get ov :file-name))
          (func (overlay-get ov :func))
          (line (overlay-get ov :line)))
      (cond
       (func
        (funcall func ov))
       ((and file-name (file-exists-p file-name))
        (other-window 1)
        (find-file file-name)
        (when line
          (goto-line line)))))))

;;; ---------------------------------------------------------
;;; - Key functions
;;;

(defmacro rails/key (key)
  `(kbd ,(concat rails-minor-mode-prefix-key " " rails-minor-mode-prefix2-key  " " key)))

(defmacro rails/define-key (key func)
  `(define-key rails-minor-mode-map (rails/key ,key) ,func))

(defmacro rails/short-key (key)
  `(kbd ,(concat rails-minor-mode-prefix-key " " key)))

(defmacro rails/define-goto-key (goto-key goto-func)
 `(define-key rails-minor-mode-map (rails/key ,(concat "g " goto-key)) ,goto-func))

(defun rails/define-goto-menu (title func)
  (define-key rails-minor-mode-map
    (merge 'vector [menu-bar rails goto] (list (string-ext/safe-symbol title)) 'eq)
    (cons (concat "Go to " (pluralize-string title)) func)))

(defmacro rails/define-toggle-key (key func)
  `(define-key rails-minor-mode-map (rails/short-key ,key) ,func))

(defun rails/define-toggle-menu (title func)
  (define-key-after
    rails-minor-mode-map
    (merge 'vector '[menu-bar rails toggle] (list (string-ext/safe-symbol title)) 'eq)
    (list 'menu-item (concat "Go to current " title) func :enable t) 'separator))

;;; ---------------------------------------------------------
;;; - Misc functions
;;;

(defun rails/environments (root)
  (mapcar
   'file-name-sans-extension
   (directory-files
    (concat root "config/environments/")
    nil
    rails/ruby/file-suffix)))

(defun rails/default-environment-p (env)
  (string= env rails/default-environment))

(defmacro rails/toggle-environment-menu-func (env)
  `'(lambda() (interactive) (rails/set-default-environment ,(symbol-value env))))

;;; ---------------------------------------------------------
;;; - Snippet support functions
;;;

(defun rails/controller? ()
  (when (rails/resource-buffer-p rails/current-buffer)
    (eq 'controller (rails/resource-buffer-type rails/current-buffer))))

(defun rails/controller-spec? ()
  (when (rails/resource-buffer-p rails/current-buffer)
    (eq 'controller-spec (rails/resource-buffer-type rails/current-buffer))))

(defun rails/model? ()
  (when (rails/resource-buffer-p rails/current-buffer)
    (eq 'model (rails/resource-buffer-type rails/current-buffer))))

(defun rails/migration? ()
  (when (rails/resource-buffer-p rails/current-buffer)
    (eq 'migration (rails/resource-buffer-type rails/current-buffer))))

(defun rails/cur-res-title ()
  (when rails/current-buffer
   (rails/resource-buffer-title rails/current-buffer)))

(provide 'rails-lib)
