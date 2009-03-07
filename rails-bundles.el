;;; rails-bundles.el ---

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
(require 'rails-resources)

(defvar rails/bundles/disabled-list nil)
(defvar rails/bundles/loaded-list nil)
(defvar rails/bundles/loaded-p nil)
(defvar rails/bundles/trigger-list nil)
(defvar rails/bundles/after-load-list nil)

(defconst rails/bundles/file-regexp "^rails-\\(.*\\)-bundle\.el$")
(defconst rails/bundles/name-fmt "rails/%s-bundle/name")

(defun rails/bundles/enabled-p (title)
  (not (memq (string-ext/safe-symbol title) rails/bundles/disabled-list)))

(defun rails/bundles/toggle-enabled (title)
  (let ((sym (string-ext/safe-symbol title)))
    (if (rails/bundles/enabled-p title)
        (progn
          (add-to-list 'rails/bundles/disabled-list sym nil 'eq)
          nil)
      (setq rails/bundles/disabled-list (delete sym rails/bundles/disabled-list))
      t)))

(defun rails/bundles/load ()
  (unless rails/bundles/loaded-p
    (let* ((bdir (concat
                  (file-name-directory (locate-library "rails-reloaded"))
                  "bundles"))
           (files (directory-files bdir nil rails/bundles/file-regexp)))
      (dolist (file files)
        (load (concat bdir "/" (file-name-sans-extension file)))
        (let* ((sym (intern
                     (string-ext/string=~ rails/bundles/file-regexp file $1)))
               (title (symbol-value
                       (intern (format rails/bundles/name-fmt sym)))))
          (add-to-list 'rails/bundles/loaded-list (cons sym title))
          (rails/bundles/add-to-loaded-menu title))))
    (mapc 'funcall rails/bundles/after-load-list)
    (setq rails/bundles/loaded-p t)))

(defun rails/bundles/reload ()
  (interactive)
  (setq rails/bundles/loaded-list nil)
  (setq rails/bundles/loaded-p nil)
  (setq rails/bundles/trigger-list nil)
  (setq rails/bundles/after-load-list nil)
  (rails/resources/clear)
  (rails/bundles/load))

(defun rails/bundles/add-to-loaded-menu (title)
  (unless (lookup-key rails-minor-mode-map
                      [menu-bar rails bundles-loaded])
    (define-key-after rails-minor-mode-map
      [menu-bar rails bundles-loaded]
      (cons "Loaded Bundles" (make-sparse-keymap))
      'env))
  (define-key rails-minor-mode-map
    (merge 'vector [menu-bar rails bundles-loaded]
           (list (string-ext/safe-symbol title))
           'eq)
    (list 'menu-item title
          `(lambda()
             (interactive)
             (rails/bundles/toggle-enabled ,title)
             (rails/reload-all))
          :button (cons :toggle (rails/bundles/enabled-p title)))))

(defun rails/bundles/add-to-bundles-menu (title menumap)
  (define-key-after rails-minor-mode-map
    (merge 'vector [menu-bar rails] (list (string-ext/safe-symbol title)) 'eq)
    (cons (concat title " Bundle") menumap)
    'bundles-title))

(defmacro* rails/defbundle (name (&key menu keys triggers after-load-bundles) &body body)
  `(progn
     (defconst
       ,(intern (format rails/bundles/name-fmt (string-ext/safe-symbol name)))
       ,name)
     (when (rails/bundles/enabled-p ,name)
       (when ,(not (not menu))
         (rails/bundles/add-to-bundles-menu
          ,name
          (let ((map (make-sparse-keymap)))
            (define-keys map
              ,@menu))))
       (when ,(not (not triggers))
         ,@(loop for tr in triggers
                 collect
                 `(add-to-list 'rails/bundles/trigger-list ',tr)))
       (when ,(not (not after-load-bundles))
         (setq rails/bundles/after-load-list
               (cons ,after-load-bundles rails/bundles/after-load-list)))
       (when ,(not (not keys))
         ,@(loop for (key func) in keys collect
                 `(rails/define-key ,key ,func)))
       ,@body)))

(provide 'rails-bundles)