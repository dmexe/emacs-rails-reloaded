;;; rails-compile.el --- run compilation process for rails application.

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

(defvar rails/compile/single-file-list '())
(defvar rails/compile/current-method-list '())

(defun rails/compile/match-error (limit)
  (catch 'found
    (while (re-search-forward "\\(?:\\[\\|^\\|\s+\\|(\\)?\\([^ :\n\]+\\):\\([0-9]+\\)+\\b" limit t)
      (let ((file (match-string 1))
            (root (rails/root default-directory)))
        (when root
          (unless (file-name-absolute-p file)
            (setq file (concat root file)))
          (setq file (expand-file-name file))
          (when (and (file-exists-p file)
                     (not (files-ext/file-in-directory-p (concat root "vendor/") file)))
            (throw 'found t)))))))

(defun rails/compile/error-regexp-alist ()
  (list
   (list 'rails/compile/error 'rails/compile/match-error 1 2 nil 2 1)))


(define-derived-mode rails/compilation-mode compilation-mode "RCompile"
  "Major mode for RoR tests."
  (set (make-local-variable 'font-lock-keywords-only) t)
  (set (make-local-variable 'font-lock-keywords) nil)
  (set (make-local-variable 'compilation-mode-font-lock-keywords) nil)
;       rails/compile/font-lock-keywords)
  (set (make-local-variable 'compilation-error-regexp-alist-alist)
       (rails/compile/error-regexp-alist))
  (set (make-local-variable 'compilation-error-regexp-alist)
       '(rails/compile/error)))

(defun rails/compile/run (root command args)
  (rails/runner/run root command args :mode 'rails/compilation-mode)
  (setq rails/runner/after-stop-func-list
        '(rails/runner/popup-buffer-if-failed)))

(defun rails/compile/run-file (root rails-buffer bundle-group-name command args-pattern)
  (let* ((type (rails/buffer-type rails-buffer))
         (link (rails/type-link-by-bundle-group-and-layout bundle-group-name
                                                           (rails/buffer-layout rails-buffer)
                                                           :tests type)))
    (when (and link type)
      (when-bind (func (rails/bundle-func link "goto-item-from-rails-buffer"))
        (let ((goto-item (funcall func (rails/root)
                                  (rails/cut-root (rails/buffer-file rails-buffer))
                                  rails-buffer)))
          (when (rails/goto-item-p goto-item)
            (rails/compile/run (rails/root)
                               command
                               (format args-pattern
                                       (rails/goto-item-file goto-item)))
            t))))))

(defun rails/compile/single-file ()
  (interactive)
  (rails/with-current-buffer
   (loop for func in rails/compile/single-file-list
         for res = (funcall func (rails/root) rails/current-buffer)
         when res
         do (return res))))

(defun rails/compile/current-method ()
  (interactive)
  (rails/with-current-buffer
   (loop for func in rails/compile/current-method-list
         for res = (funcall func (rails/root) rails/current-buffer)
         when res
         do (return res))))

(provide 'rails-compile)
