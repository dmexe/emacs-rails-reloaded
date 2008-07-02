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

;;; Commentary:

;; Basic steps to setup:
;;   1. Setup  `load-path':
;;      (setq load-path (cons (expand-file-name "~/.emacs.d/rails-reloaded") load-path))
;;   2. Put in your .emacs file:
;;      (require 'rails-autoload)
;;   3. To bytecompile after emacs restarted, type:
;;      [M-x] load-library [RET] rails-bytecompile

;;; Code:

(require 'core-ext)
(require 'string-ext)
(require 'files-ext)
(require 'list-ext)
(require 'inflections)

(require 'rails-ruby)
(require 'rails-lib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Variables
;;

(defconst rails/version "0.99")

(defvar rails/current-buffer nil)

(defvar rails/bundles-list '(controller
                             helper
                             model
                             unit-test
                             view))

(defvar rails/bundles-func-list '())
(defvar rails/bundles-group-list '())
(defvar rails/bundles-loaded-p nil)

(defvar rails/resource-types-list '())
(defvar rails/layouts-alist '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Customized variables
;;

(defgroup rails nil
  "Edit Rails projects with Emacs."
  :group 'programming
  :prefix "rails/")

(defcustom rails/disabled-bundles '()
  "Disabled bundles."
  :group 'rails
  :type '(repeat (symbol :tag "Bundle name")))

(defcustom rails/display-menu-method 'popup
  "Display menu method."
  :group 'rails
  :type '(choice (const :tag "Default" nil)
                 (const :tag "Using Popup Menu" popup)
                 (const :tag "Using ido-completion" ido)))

(defcustom rails-minor-mode-hook nil
  "Hook run when entering Rails minor mode."
  :type 'hook
  :group 'rails)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Structures
;;

(defstruct rails/buffer type
                        name
                        resource-name
                        file
                        (weight 0)
                        (views-name resource-name)
                        (tests-name resource-name))

(defstruct rails/goto-item group name file weight func)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Begin functions
;;

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

(defun rails/initialize-bundles (root file rails-current-buffer)
  (let ((file (rails/cut-root file)))
    (dolist (func (rails/bundles-func "initialize"))
      (apply func (list root file rails-current-buffer)))))

(defun rails/determine-type-of-file (root file &optional rails-buffer)
  (if (and rails-buffer
           (string= (rails/cut-root file) (rails/buffer-file rails-buffer))
           (rails/buffer-type rails-buffer)
           (rails/buffer-name rails-buffer)
           (rails/buffer-resource-name rails-buffer))
      rails-buffer
    (progn
      (let ((strip-file (rails/cut-root file))
            found-rails-buffer
            (weight 0))
        (when strip-file
          (dolist (func (rails/bundles-func "determine-type-of-file"))
            (when-bind (buf (apply func (list root strip-file)))
              (when (and (rails/buffer-p buf)
                         (rails/buffer-type buf)
                         (rails/buffer-name buf)
                         (rails/buffer-resource-name buf)
                         (rails/buffer-weight buf)
                         (> (rails/buffer-weight buf) weight))
                (setq found-rails-buffer (copy-rails/buffer buf))
                (setq weight (rails/buffer-weight buf))
                (unless (rails/buffer-file found-rails-buffer)
                  (setf (rails/buffer-file found-rails-buffer) strip-file)))))
          (when (rails/buffer-p found-rails-buffer)
            (if rails-buffer
                (setq rails-buffer (copy-rails/buffer found-rails-buffer))
              found-rails-buffer)))))))

(defun rails/goto-item-alist-from-file (root file rails-buffer)
  (let ((goto-item-list '()))
    (dolist (func (rails/bundles-func "goto-item-from-file"))
      (let ((line (apply func (list root (rails/cut-root file) rails-buffer))))
        (when (rails/goto-item-p line)
          (add-to-list 'goto-item-list line t))
        (when (listp line)
          (dolist (it line)
            (when (rails/goto-item-p it)
              (add-to-list 'goto-item-list it t))))))
    (list-ext/group-by
     goto-item-list #'(lambda(it) (rails/goto-item-group it)))))

(defun rails/menu-from-goto-item-alist (root title goto-alist)
  (let (menu item last-p)
    (dolist (alist goto-alist)
      (let ((group (car alist))
            (list (cadr alist)))
        (when last-p
          (add-to-list 'menu (list "--" group) t))
        (dolist (it list)
          (add-to-list 'menu (cons (rails/goto-item-name it) it) t)))
      (unless last-p
        (setq last-p t)))
    (when (> (length menu) 0)
      (when (setq item (rails/display-menu title menu))
        (if (rails/goto-item-func item)
            (funcall (rails/goto-item-func item) item)
          (rails/find-file-by-goto-item root item))))))

(defun rails/find-file-by-goto-item (root goto-item)
  (when goto-item
    (when-bind (file (rails/goto-item-file goto-item))
      (when (rails/file-exist-p root file)
        (rails/find-file root file)
        (when rails/current-buffer
          (rails/notify-by-rails-buffer rails/current-buffer))))))

(defun rails/fast-find-file-by-goto-item (root goto-item)
  (let ((action (rails/current-buffer-action-name)))
    (rails/find-file-by-goto-item root goto-item)
    (when (and action
               (rails/bundles-func-by-buffer rails/current-buffer "goto-action-in-current-buffer"))
      (rails/goto-action-in-current-buffer action)
      (rails/notify-by-rails-buffer rails/current-buffer action))))

(defun rails/current-buffer-action-name ()
  (when (rails/buffer-p rails/current-buffer)
    (when-bind (func (rails/bundles-func-by-buffer
                      rails/current-buffer "current-buffer-action-name"))
      (funcall func))))

(defun rails/goto-action-in-current-buffer (action)
  (when (and (rails/buffer-p rails/current-buffer)
             action)
    (when-bind (func (rails/bundles-func-by-buffer
                      rails/current-buffer "goto-action-in-current-buffer"))
      (funcall func action))))

(defun rails/notify (string)
  (message string))

(defun rails/notify-by-rails-buffer (rails-buffer &optional action-name)
  (rails/notify
   (if action-name
       (format "%s %s#%s"
               (string-ext/cut (format "%s" (rails/buffer-type rails-buffer)) ":" :begin)
               (rails/buffer-name rails-buffer)
               action-name)
     (format "%s %s"
             (string-ext/cut (format "%s" (rails/buffer-type rails-buffer)) ":" :begin)
             (rails/buffer-name rails-buffer)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Interactives
;;

(defun rails/reload-bundles ()
  (interactive)
  (setq rails/bundles-loaded-p nil)
  (setq rails/bundles-func-list nil)
  (setq rails/layouts-alist nil)
  (rails-minor-mode-reset-keymap)
  (rails/load-bundles))

(defun rails/goto-from-current-file ()
  (interactive)
  (when (rails/buffer-p rails/current-buffer)
    (let ((file (buffer-file-name)))
      (rails/with-root file
        (let ((list (rails/goto-item-alist-from-file
                     (rails/root)
                     file
                     rails/current-buffer))
              (title (capitalize (string-ext/from-symbol
                                  (rails/buffer-type rails/current-buffer)))))
          (rails/menu-from-goto-item-alist (rails/root) (format "Go to from %s to..." title) list))))))

(defun rails/fast-goto-from-current-file ()
  (interactive)
  (let ((file (buffer-file-name))
        (weight 0)
        goto-item)
    (rails/with-root file
      (dolist (func (rails/bundles-func
                     "fast-goto-item-from-file"
                     (rails/layout-for-type (rails/buffer-type rails/current-buffer))))
        (let ((item (funcall func (rails/root) (rails/cut-root file) rails/current-buffer)))
          (when (and (rails/goto-item-p item)
                     (>= (rails/goto-item-weight item) weight))
                (setq weight (rails/goto-item-weight item))
                (setq goto-item item))))
      (when goto-item
        (rails/fast-find-file-by-goto-item (rails/root) goto-item)))))

(defun rails/initialize-for-current-buffer ()
  (interactive)
  (let ((file (buffer-file-name)))
    (rails/with-root file
      (rails/load-bundles)
      (set (make-local-variable 'rails/current-buffer)
           (rails/determine-type-of-file (rails/root) file))
      (rails-minor-mode t)
      (rails/initialize-bundles (rails/root) file rails/current-buffer))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Rails minor mode
;;

(defun rails-minor-mode-reset-keymap ()
  (setf rails-minor-mode-map (rails-minor-mode-default-keymap))
  (setf (cdr (assoc 'rails-minor-mode minor-mode-map-alist))
        rails-minor-mode-map))

(defun rails-minor-mode-menu-bar-map ()
  (let ((map (make-sparse-keymap)))
    (define-keys map
      ([rails] (cons "RoR" (make-sparse-keymap "RubyOnRails")))
      ([rails version]    '(menu-item (concat "Version: " rails/version) 'foo :enable nil))
      ([rails separator1] (cons "--" "--"))
      ([rails goto-fast]  (cons "Go To From Current File" (make-sparse-keymap)))
      ([rails goto-fast separator] (cons "--" "--"))
      ([rails goto-fast goto-fast]  (cons "Go to with Menu" 'rails/goto-from-current-file))
      ([rails goto-fast goto]       (cons "Go to" 'rails/fast-goto-from-current-file))
      ([rails goto-list]    (cons "Go To" (make-sparse-keymap))))
  map))

(defun rails-minor-mode-default-keymap ()
  (let ((map (make-keymap)))
    (define-keys map
      ([menu-bar] (rails-minor-mode-menu-bar-map))
      ((rails/define-short-key "<down>") 'rails/goto-from-current-file)
      ((rails/define-short-key "<up>")   'rails/fast-goto-from-current-file))
    map))

(defvar rails-minor-mode-prefix-key "\C-c")
(defvar rails-minor-mode-prefix2-key "\C-c")

(defvar rails-minor-mode-map (rails-minor-mode-default-keymap))

(define-minor-mode rails-minor-mode
  "RubyOnRails"
  nil
  " RoR"
  rails-minor-mode-map
  (run-hooks 'rails-minor-mode-hook))

(provide 'rails-reloaded)