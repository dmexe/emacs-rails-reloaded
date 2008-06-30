;;; rails-lib.el --- library functions for rails.

;; Copyright (C) 2006 Dmitry Galinsky <dima dot exe at gmail dot com>

;; Authors: Dmitry Galinsky <dima dot exe at gmail dot com>,
;;          Rezikov Peter <crazypit13 (at) gmail.com>

;; Keywords: ruby rails languages
;; $URL: svn+ssh://rubyforge.org/var/svn/emacs-rails/trunk/rails.el $
;; $Id: rails.el 225 2008-03-02 21:07:10Z dimaexe $

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

(require 'files-ext)
(require 'string-ext)
(require 'list-ext)

(defvar rails/projects '())

(defun rails/root (&optional file)
  "Return RAILS_ROOT for FILE, if FILE not set using `buffer-file-name' instead it.
If RAILS_ROOT not found, return nil."
  (let (root
        (file (if file file (buffer-file-name))))
    (cond
     ((setq root (rails/find-existing-root-for file)))
     ((setq root (rails/find-root-for file))))
    root))

(defmacro* rails/with-root (file &body body)
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

;; (defmacro rails/in-root (file &rest body)
;;   "Set the default directory to the Rails root directory of FILE while
;; BODY is executed."
;;   (let ((root (gensym)))
;;     `(rails/with-root ,file
;;       (let ((default-dir (rails/root file)))
;;         ,@body))))

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

(defun rails/bundle-func (bundle-symbol func-name)
  (let* ((name (string-ext/from-symbol bundle-symbol))
         (name (concat "rails/" name "/" func-name))
         (name (intern name)))
    (if (fboundp name)
        name nil)))

(defun rails/bundles-func-by-buffer (rails-buffer func-name)
  (when (rails/buffer-p rails-buffer)
    (rails/bundle-func
     (string-ext/from-symbol (rails/buffer-type rails-buffer))
     func-name)))

(defun rails/bundles-func (func-name)
  (let ((list (cadr (assoc func-name rails/bundles-func-list))))
    (if list
        list
      (progn
        (dolist (bundle rails/bundles-list)
          (let ((func (rails/bundle-func bundle func-name)))
            (when func
              (add-to-list 'list func t))))
        (push (list func-name list) rails/bundles-func-list)
        list))))

(defun rails/file-exist-p (root file)
  (file-exists-p (concat root file)))

(defun rails/find-file (root file)
  (find-file (concat root file)))

(defun rails/group-by-goto-item-group (goto-items)
  (list-ext/group-by goto-items 'rails/goto-item-group))

(defun rails/directory-to-goto-menu (root dir title &rest options)
  (let ((files
         (files-ext/find-recursive-files '(lambda (it)
                                            (unless (files-ext/file-special-p it) it))
                                         (list-ext/options-value :regexp options)
                                         (concat root dir)))
        (filter-by
         (or (list-ext/options-value :filter-by options)
             'stringp))
        (name-by
         (or (list-ext/options-value :name-by options)
             'file-name-nondirectory))
        (append-items (list-ext/options-value :append options))
        goto-items)
    (when files
      (setq goto-items
        (loop for file in files
              for allow = (funcall filter-by (concat root dir file))
              when allow
              collect (make-rails/goto-item
                       :name (funcall name-by file)
                       :file (concat dir file))))
    (when append-items
      (dolist (it append-items)
        (add-to-list 'goto-items it t)))
    (setq goto-items (rails/group-by-goto-item-group goto-items))
    (rails/menu-from-goto-item-alist root title goto-items))
    goto-items))

(defun rails/add-to-associated-types-list (type)
  (add-to-list 'rails/associated-types-list type))

(defun rails/associated-type-p (rails-buffer &optional exclude-type)
  (when (rails/buffer-p rails-buffer)
    (let ((type (rails/buffer-type rails-buffer)))
      (unless (eq type exclude-type)
        (find type rails/associated-types-list)))))

(defmacro rails/define-key (key)
  `(kbd ,(concat rails-minor-mode-prefix-key " " rails-minor-mode-prefix2-key  " " key)))

(defmacro rails/define-short-key (key)
  `(kbd ,(concat rails-minor-mode-prefix-key " " key)))

(defmacro rails/define-goto-key (goto-key goto-func)
 `(define-key rails-minor-mode-map (rails/define-key ,(concat "g " goto-key)) ,goto-func))

(defun rails/define-goto-menu (vec func title)
  (define-key rails-minor-mode-map
    (merge 'vector [menu-bar rails goto-list] vec 'eq)
    (cons (concat "Go to " title) func)))

(defmacro rails/define-fast-goto-key (key func)
  `(define-key rails-minor-mode-map (rails/define-short-key ,key) ,func))

(defun rails/define-fast-goto-menu (vec func title &optional enable)
  (define-key-after
    rails-minor-mode-map
    (merge 'vector [menu-bar rails goto-fast] vec 'eq)
    (list 'menu-item (concat "Go to current " title) func :enable t) 'separator))

(provide 'rails-lib)