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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Variables
;;

(defvar rails/projects '())


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Customized variables
;;

(defcustom rails/search-files-in-dirs nil
  "Set this variable to search rails files only in them."
  :group 'rails
  :type '(repeat (directory :tag "Directory")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; rails/root functions
;;

(defun rails/root (&optional file)
  "Return RAILS_ROOT for FILE, if FILE not set using `buffer-file-name' instead it.
If RAILS_ROOT not found, return nil."
  (let ((file (or file
                  (buffer-file-name))))
    (unless  (and rails/search-files-in-dirs
                  (files-ext/file-in-directories-p
                   rails/search-files-in-dirs file))
      (or
       (rails/find-existing-root-for file)
       (rails/find-root-for file)))))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Bundles functions
;;

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

(defun rails/bundles-func (func-name &optional layout)
  (let ((key (car (assoc func-name rails/bundles-func-list)))
        (alist (cadr (assoc func-name rails/bundles-func-list))))
    (if key
        (rails/bundles-func-filter-by-layout layout alist)
      (progn
        (dolist (bundle rails/bundles-list)
          (let ((func (rails/bundle-func bundle func-name)))
            (when func
              (add-to-list 'alist (cons func bundle) t))))
        (push (list func-name alist) rails/bundles-func-list)
        (rails/bundles-func-filter-by-layout layout alist)))))

(defun rails/bundles-func-filter-by-layout (layout func-list)
  (when func-list
    (if layout
        (let* ((names (find layout rails/layouts-list :key 'car))
               (names (mapcar
                       (lambda (sym)
                         (intern (substring (symbol-name sym) 1)))
                       names)))
          (loop for (func . bundle) in func-list
                for allow = (memq bundle names)
                when allow
                collect func))
      (mapcar 'car func-list))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Files functions
;;

(defun rails/file-exist-p (root file)
  (file-exists-p (concat root file)))

(defun rails/find-file (root file)
  (find-file (concat root file)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Goto item functions
;;

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
        (reverse (list-ext/options-value :reverse options))
        (limit (list-ext/options-value :limit options))
        goto-items)
    (when files
      (setq goto-items
        (loop for file in files
              for allow = (funcall filter-by (concat root dir file))
              when allow
              collect (make-rails/goto-item
                       :name (funcall name-by file)
                       :file (concat dir file))))
      (when reverse
        (setq goto-items (reverse goto-items)))
      (when (and limit
                 (< limit (length goto-items)))
        (nbutlast goto-items (- (length goto-items) limit)))
      (when append-items
        (dolist (it append-items)
          (add-to-list 'goto-items it t)))
      (setq goto-items (rails/group-by-goto-item-group goto-items))
      (rails/menu-from-goto-item-alist root title goto-items))
    goto-items))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Menu functions
;;

(defun rails/display-menu (title menu)
  (let ((func
         (cond
          ((and (eq rails/display-menu-method 'popup)
                window-system)
           'rails/display-menu-using-popup)
          (t
           'rails/display-menu-using-ido))))
    (funcall func title menu)))

(defun rails/display-menu-using-popup (title menu)
  (add-to-list 'menu title)
  (x-popup-menu (list '(300 50) (get-buffer-window (current-buffer)))
                (list title
                      menu)))

(defun rails/display-menu-using-ido (title menu)
  (let (choices value)
    (dolist (item menu) ; filter
      (let ((name (car item))
            (goto (cdr item)))
        (when (rails/goto-item-p goto)
          (add-to-list 'choices (cons name goto) t))))
    (when-bind (value
                (ido-completing-read (format "%s: " title)
                                     (mapcar 'car choices)
                                     nil
                                     t))
      (cdr (find value choices :test 'string= :key 'car)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Resources and layouts functions
;;

(defun rails/add-to-resource-types-list (type)
  (add-to-list 'rails/resource-types-list type))

(defun rails/resource-type-p (rails-buffer &optional exclude-type)
  (when (rails/buffer-p rails-buffer)
    (let ((type (rails/buffer-type rails-buffer)))
      (unless (eq type exclude-type)
        (find type rails/resource-types-list)))))

(defun rails/add-to-layouts-list (layout child)
  (let ((exist (find layout rails/layouts-list :key 'car)))
    (if exist
        (setf (cdr (find layout rails/layouts-list :key 'car))
              (cdr (append exist (list child))))
      (add-to-list 'rails/layouts-list (list layout child)))
    rails/layouts-list))

(defun rails/layout-p (layout child)
  (car (memq child (find layout rails/layouts-list :key 'car))))

(defun rails/layout-for-type (type)
  (or (car (find type rails/layouts-list :key 'car))
      (loop for layout in (mapcar 'car rails/layouts-list)
            for allow = (rails/layout-p layout type)
            when allow do (return layout))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Keys and menus functions
;;

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

(defun rails/add-to-bundles-menu (title menumap)
  (unless (lookup-key rails-minor-mode-map
                      [menu-bar rails bundles-title])
    (define-key-after rails-minor-mode-map
      [menu-bar rails bundles-title]
      '(menu-item "Bundles:" "--" :enable nil) 'separator)
    (define-key-after rails-minor-mode-map
      [menu-bar rails bundles-separator]
      (cons "--" "--") 'bundles-title))
  (define-key-after rails-minor-mode-map
    (merge 'vector '(menu-bar rails) (list (intern (downcase title))) 'eq)
    (cons (concat title " Bundle") menumap)
    'bundles-title))

(provide 'rails-lib)
