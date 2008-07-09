;;; rails-lib.el --- library functions used in rails.

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

(eval-when-compile
  (require 'cl))

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
                  (buffer-file-name)
                  (and (eq (current-buffer)
                           (get-buffer rails/runner/buffer-name))
                       rails/runner/buffer-rails-root))))
    (unless (and rails/search-files-in-dirs
                 (files-ext/file-in-directories-p rails/search-files-in-dirs
                                                  file))
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
  `(when (rails/buffer-p rails/current-buffer)
     (rails/with-root (buffer-file-name)
       ,@body)))

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

(defun rails/add-to-bundles-group (group bundle)
  (if (find group rails/bundles-group-list :key 'car :test 'string=)
      (progn
        (push bundle (cdr (find group rails/bundles-group-list :key 'car :test 'string=)))
        (rails/add-to-bundles-group-menu group))
    (add-to-list 'rails/bundles-group-list (list group bundle))))

(defun rails/bundle-group (bundle)
  (loop for group in rails/bundles-group-list
        for found = (find bundle (cdr group))
        when found do (return (car group))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Files functions
;;

(defun rails/file-exist-p (root file)
  (file-exists-p (concat root file)))

(defun rails/file-directory-p (root dir)
  (file-directory-p (concat root dir)))

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

(defun rails/completing-read (title list &optional require default history)
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
        (find-file-other-window file-name)
        (when line
          (goto-line line)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Resources, layouts, type links and functions
;;

(defun rails/add-to-resource-types-list (type)
  (add-to-list 'rails/resource-types-list type))

(defun rails/resource-type-of-buffer (rails-buffer &rest options)
  (when (rails/buffer-p rails-buffer)
    (let ((type (rails/buffer-type rails-buffer)))
      (unless (eq type (opt-val :exclude options))
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
  (or (car (find type rails/layouts-list :key 'car))        ; TYPE is the layout
      (loop for layout in (mapcar 'car rails/layouts-list)  ; TYPE inside the layout
            for allow = (rails/layout-p layout type)
            when allow do (return layout))
      type))                                                ; TYPE is the layout, but not defined

(defun rails/add-type-link (group type link)
  (if (find group rails/linked-types-alist :key 'car)
      (push (cons type link) (cdr (find group rails/linked-types-alist :key 'car)))
    (add-to-list 'rails/linked-types-alist (list group (cons type link)))))

(defun rails/type-link-for (group type)
  (when-bind (group (find group rails/linked-types-alist :key 'car))
    (let ((links (cdr group)))
      (or (cdr (find type links :key 'car))
          (car (find type links :key 'cdr))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Keys and menus functions
;;

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

(defun rails/add-to-bundles-menu (title menumap)
  (define-key-after rails-minor-mode-map
    (merge 'vector [menu-bar rails] (list (string-ext/safe-symbol title)) 'eq)
    (cons (concat title " Bundle") menumap)
    'bundles-title))

(defun rails/add-to-bundles-group-menu (title)
  (unless (lookup-key rails-minor-mode-map
                      [menu-bar rails bundles-groups])
    (define-key-after rails-minor-mode-map
      [menu-bar rails bundles-groups]
      (cons "Bundles Groups" (make-sparse-keymap)) 'bundles-separator))
    (define-key rails-minor-mode-map
      (merge 'vector [menu-bar rails bundles-groups] (list (string-ext/safe-symbol title)) 'eq)
      (list  'menu-item title 'identity :button (cons :toggle '(lambda () t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Special types
;;

(defun rails/mailer-p (root file)
  (when (fboundp 'rails/mailer/mailer-p)
    (rails/mailer/mailer-p (concat root file))))

(defun rails/resource-mailer-p (root resource)
  (when (fboundp 'rails/mailer/exist-p)
    (rails/mailer/exist-p root resource)))

(defun rails/observer-p (root file)
  (when (fboundp 'rails/observer/observer-p)
    (rails/observer/observer-p (concat root file))))

(defun rails/resource-observer-p (root resource)
  (when (fboundp 'rails/observer/exist-p)
    (rails/observer/exist-p root resource)))


(provide 'rails-lib)
