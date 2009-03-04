;;; rails-compile.el --- run compilation process for rails application.

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

(require 'rails-resources)

(defvar rails/compile/single-file-list '())
(defvar rails/compile/current-method-list '())

(defvar rails/compile/font-lock-keywords
  '(("\\([[:digit:]]+ tests\\), \\([[:digit:]]+ assertions\\), \\([[:digit:]]+ failures\\), \\([[:digit:]]+ errors\\)"
     (1 compilation-info-face)
     (2 compilation-info-face)
     (3 compilation-error-face)
     (4 compilation-error-face))
    ("^\s+\\([0-9]+)\s+\\(Error\\|Failure\\):\\)"
     1 compilation-error-face)
    ("^[.EF]+$" . compilation-info-face)
    ("^\\([a-z0-9_]+\\)(\\(.*\\))\\(:$\\|\n\s+\\[\\|\s+\\[\\)"
     (1 font-lock-function-name-face)
     (2 font-lock-type-face))
    ("^<\\(.*\\)> \\(expected but was\\)\n<\\(.*\\)>.$"
     (1 font-lock-constant-face)
     (2 font-lock-string-face)
     (3 font-lock-constant-face))
    ("`\\(.+\\)'"
     (1 font-lock-function-name-face))))

(defun rails/compile/match-error (limit)
  (catch 'found
    (while (re-search-forward "\\(?:\\[\\|^\\|\\s+\\|(\\)?\\([^ :\n\]+\\):\\([0-9]+\\)+\\b" limit t)
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
;;  (set (make-local-variable 'font-lock-defaults) nil) ; to enable fontify by ansi-color
;;   (set (make-local-variable 'font-lock-defaults)
;;        '(rails/compile/font-lock-keywords t))
  (set (make-local-variable 'compilation-mode-font-lock-keywords)
       rails/compile/font-lock-keywords)
  (set (make-local-variable 'compilation-error-regexp-alist-alist)
       (rails/compile/error-regexp-alist))
  (set (make-local-variable 'compilation-error-regexp-alist)
       '(rails/compile/error)))

(defun rails/compile/run (root command args)
  (rails/runner/run root command args :mode 'rails/compilation-mode)
  (setq rails/runner/after-stop-func-list
        '(rails/runner/popup-buffer-if-failed)))

(defun rails/compile/run-file (root rails-buffer command args-pattern &optional file-pattern)
  (let* ((item
          (when rails-buffer
            (rails/resources/get-associated-test-item-for-buffer
             root
             rails-buffer)))
         (match (when file-pattern
                  (string-ext/string=~ file-pattern
                                       (or
                                        (when item (rails/resource-item-file item))
                                        (rails/resource-buffer-file rails-buffer))
                                      t)))
        file)
    (cond
     ((and item (if file-pattern match t))
      (setq file (rails/resource-item-file item))
      (rails/compile/run root
                         command
                         (format args-pattern
                                 file)))
     ((and file-pattern
           match)
      (rails/compile/run root
                         command
                         (format args-pattern
                                 (rails/cut-root (buffer-file-name)))))
     (t
      (rails/notify "Can't run current file as a test." :error)))))

(defun rails/compile/single-file ()
  (interactive)
  (when-bind (root (rails/root))
   (loop for func in rails/compile/single-file-list
         for res = (funcall func root rails/current-buffer)
         when res
         do (return res))))

(defun rails/compile/current-method ()
  (interactive)
  (when-bind (root (rails/root))
   (loop for func in rails/compile/current-method-list
         for res = (funcall func root rails/current-buffer)
         when res
         do (return res))))

(provide 'rails-compile)
