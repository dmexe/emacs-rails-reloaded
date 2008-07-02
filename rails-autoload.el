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

(defun rails/bytecompile ()
  "Byte compile rails-reloaded library."
  (interactive)
  (require 'rails-reloaded)
  (mapcar
   #'(lambda (file)
       (unless (string= (file-name-nondirectory file)
                        "rails-bytecompile.el")
         (byte-compile-file file)))
   (directory-files
    (file-name-directory (locate-library "rails-reloaded"))
    t "\\.el\\'")))

(defun rails/selftest ()
  "Run unit tests for rails-reloaded library."
  (interactive)
  (load-file
   (concat
    (file-name-directory
     (locate-library "rails-reloaded"))
    "tests/all.el"))
  (message (format "rails/selftest: done")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Setup autoload
;;

(defun rails/find-file-hook ()
  "Activate `rails-minor-mode' if opened file inside RAILS_ROOT."
  (when (rails/root (buffer-file-name))
    (rails/initialize-for-current-buffer)))

(defun rails/setup-auto-modes-alist ()
  (let ((modes
         '((ruby-mode "\\.rb\\'" "\\.rake\\'" "Rakefile\\'" "\\.rjs\\'" "\\.rxml\\'" "\\.builder\\'")
           (html-mode "\\.erb\\'" "\\.rhtml\\'"))))
    (dolist (mode-line modes)
      (loop for regexp in (cdr mode-line)
            for allow = (not (find regexp auto-mode-alist :key 'car :test 'string=))
            when allow
            do
            (setq auto-mode-alist (cons (cons regexp (car mode-line)) auto-mode-alist))))))

(autoload 'rails/initialize-for-current-buffer "rails-reloaded" nil t)
(autoload 'rails/root "rails-lib" nil t)

(add-hook 'find-file-hooks 'rails/find-file-hook)
(rails/setup-auto-modes-alist)

(provide 'rails-autoload)
