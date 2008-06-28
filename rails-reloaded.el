;;; rails-reloaded.el --- minor mode for editing RubyOnRails code

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

(require 'core-ext)
(require 'string-ext)
(require 'files-ext)
(require 'list-ext)
(require 'inflections)

(require 'rails-lib)

(defvar rails/projects '())

(defstruct rails/buffer type name filename weight)
(defstruct rails/goto-item group name file)

(defvar rails/current-buffer nil)

(defvar rails/bundles-list '(controller
                             helper
                             model))

(defvar rails/bundles-func-list '())

(defvar rails/bundles-loaded-p nil)

(defvar rails/ruby/file-suffix ".rb")
(defvar rails/associated-types-list '())

(defun rails/load-bundles ()
  "Loading bundles from `rails/bundles'."
  (unless rails/bundles-loaded-p
    (dolist (bundle rails/bundles-list)
      (let* ((name (string-ext/from-symbol bundle))
             (name (concat "rails-" name "-bundle"))
             (name (intern name)))
        (require name)
        (when-bind (load-func (rails/bundle-func bundle "load"))
          (apply load-func (list))))))
    (setq rails/bundles-loaded-p t))

(defun rails/initialize-bundles (root file)
  (let ((file (rails/cut-root file)))
    (dolist (func (rails/bundles-func "initialize"))
      (apply func (list root file rails/current-buffer)))))

(defun rails/determine-type-of-file (root file &optional rails-buffer)
  (if (and rails-buffer
           (string= (rails/cut-root file) (rails/buffer-filename rails-buffer))
           (rails/buffer-type rails-buffer)
           (rails/buffer-name rails-buffer))
      rails-buffer
    (progn
      (let ((strip-file (rails/cut-root file))
            type
            name
            (weight 0))
        (when strip-file
          (dolist (func (rails/bundles-func "determine-type-of-file"))
            (when-bind (buffer-type (apply func (list root strip-file)))
              (when (and (rails/buffer-p buffer-type)
                         (rails/buffer-type buffer-type)
                         (rails/buffer-name buffer-type)
                         (rails/buffer-weight buffer-type)
                         (>= (rails/buffer-weight buffer-type) weight))
                (setq weight (rails/buffer-weight buffer-type))
                (setq type (rails/buffer-type buffer-type))
                (setq name (rails/buffer-name buffer-type)))))
          (when (and type name)
            (if rails-buffer
                (progn
                  (setf (rails/buffer-type rails-buffer) type)
                  (setf (rails/buffer-name rails-buffer) name)
                  (setf (rails/buffer-filename rails-buffer) strip-file)
                  (setf (rails/buffer-weight rails-buffer) weight)
                  rails-buffer)
              (make-rails/buffer :type type :name name :filename strip-file :weight weight))))))))

(defun rails/goto-item-alist-from-file (root file rails-buffer)
  (let ((goto-item-list '()))
    (dolist (func (rails/bundles-func "goto-item-from-file"))
      (let ((line (apply func (list root (rails/cut-root file) rails-buffer))))
        (when (rails/goto-item-p line)
          (add-to-list 'goto-item-list line t))))
    (list-ext/group-by
     goto-item-list #'(lambda(it) (rails/goto-item-group it)))))

(defun rails/menu-from-goto-item-alist (root title goto-alist)
  (let (menu item)
    (dolist (alist goto-alist)
      (let ((group (car alist))
            (list (cadr alist)))
        (dolist (it list)
          (add-to-list 'menu (cons (rails/goto-item-name it) it) t))))
    (when (> (length menu) 0)
      (add-to-list 'menu title)
      (setq item
            (x-popup-menu (list '(300 50) (get-buffer-window (current-buffer)))
                          (list title
                                menu)))
      (when item
        (rails/goto-from-goto-item root item)))))

(defun rails/goto-from-goto-item (root goto-item)
  (when goto-item
    (when-bind (file (rails/goto-item-file goto-item))
      (when (rails/file-exist-p root file)
        (rails/find-file root file)
        (when rails/current-buffer
          (rails/notify-by-rails-buffer rails/current-buffer))))))

(defun rails/notify (string)
  (message string))

(defun rails/notify-by-rails-buffer (rails-buffer)
  (rails/notify
   (format "Opened %s %s"
           (string-ext/cut (format "%s" (rails/buffer-type rails-buffer)) ":" :begin)
           (capitalize (rails/buffer-name rails-buffer)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Interactives
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rails/reload-bundles ()
  (interactive)
  (setq rails/bundles-loaded-p nil)
  (setq rails/bundles-func-list nil)
  (rails/load-bundles))

(defun rails/goto-from-current-file ()
  (interactive)
  (let ((file (buffer-file-name)))
    (rails/with-root file
      (let ((list (rails/goto-item-alist-from-file
                   (rails/root)
                   file
                   rails/current-buffer))
            (title (capitalize (string-ext/from-symbol
                                (rails/buffer-type rails/current-buffer)))))
        (rails/menu-from-goto-item-alist (rails/root) (format "Go to from %s to..." title) list)))))

(defun rails/initialize-for-current-buffer ()
  (interactive)
  (let ((file (buffer-file-name)))
    (rails/with-root file
      (rails/load-bundles)
      (set (make-local-variable 'rails/current-buffer)
           (rails/determine-type-of-file (rails/root) file))
      (rails-minor-mode t)
      (rails/initialize-bundles (rails/root) file))))

(defvar rails-minor-mode-map
  (let ((map (make-keymap)))
    map))

(define-keys rails-minor-mode-map
  ((kbd "\C-c <down>") 'rails/goto-from-current-file))


(define-minor-mode rails-minor-mode
  "RubyOnRails"
  nil
  " RoR"
  rails-minor-mode-map)

(provide 'rails-reloaded)