;;; rails-autoload.el --- minor mode for editing RubyOnRails code

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

(require 'string-ext)

(defvar rails/buffer '())
(make-variable-buffer-local 'rails/buffer)

(defvar rails/bundles '())

(defvar rails/projects '())

(setq rails/projects '())

(defun rails/root (&optional file)
  "Return RAILS_ROOT for FILE, if FILE not set using `buffer-file-name' instead it,
else RAILS_ROOT not found, return nil."
  (let (root
        (file (if file file (buffer-file-name))))
    (cond
     ((setq root (rails/find-existing-root-for file)))
     ((setq root (rails/find-root-for file))))
    root))

(defun rails/find-existing-root-for(file)
  "Search RAILS_ROOT for FILE in `rails/projects' and return it,
else return nil"
  (let ((project (car rails/projects))
        (projects (cdr rails/projects))
        (root))
    (while (and (not root)
                project)
      (if (string-ext/start-with? file project)
          (setq root project)
        (progn
          (setq project (car projects))
          (setq projects (cdr projects)))))
    root))

(defun rails/find-root-for (file)
  "Return RAILS_ROOT if FILE is a part of a Rails application,
else return nil"
  (let ((curdir (directory-of-file (expand-file-name file)))
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
        (setq rails/projects (uniq-list (add-to-list 'rails/projects root)))
        root))))

(provide rails-autoload)