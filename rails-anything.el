;;; rails-anything.el --- anything integration

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

;; load anything from rails-reloaded/vendor directory unless they loaded.
(unless (featurep 'anything)
  (load (locate-library "vendor/anything")))

(defvar anything-rails-current-buffer nil)
(defvar anything-rails-current-root nil)

(defvar anything-c-source-rails-associated
      '((name . "Goto from current to")
        (init . rails/anything/init-func)
        (candidates . rails/anything/associated-func)
        (candidate-number-limit . 100)
        (type . file)))

(defun rails/anything/load-triggers ()
  (loop for triggers in rails/bundles/trigger-list
        collect
        (let ((name (cadr triggers))
              (its (cddr triggers))
              result)
          (add-to-list 'result (cons 'name name))
          (add-to-list 'result (cons 'init 'rails/anything/init-func))
          (unless (find 'candidate-number-limit its :key 'car)
            (add-to-list 'result (cons 'candidate-number-limit 100)))
          (dolist (tr its)
            (add-to-list 'result tr))
          result)))

(defun rails/anything/hightlight-current (current)
  (setq current (concat ">" current "<"))
  (setq current (propertize current 'face 'font-lock-keyword-face)))

(defun rails/anything/init-func ()
  (setq anything-rails-current-buffer
        (current-buffer))
  (setq anything-rails-current-root
        (rails/root)))

(defun rails/anything/make-filename (root item)
  (concat root (rails/resource-item-file item)))

(defun rails/anything/find-file-by-item (item)
  (with-current-buffer anything-rails-current-buffer
    (rails/resources/find-file-by-item (rails/root) item)))

(defun rails/anything/associated-func ()
  (let ((buf anything-rails-current-buffer))
    (with-current-buffer buf
      (let ((root (rails/root))
            items result)
        (setq items (rails/resources/get-associated-items root rails/current-buffer))
        (setq items
              (list-ext/group-by
               items
               '(lambda(i) (rails/resource-item-resource-group (car i)))
               'string<))
        (dolist (i items)
          (dolist (ii (cadr i))
            (if (and (= 1 (length ii))
                     (not (rails/resource-options-p
                           (rails/resources/find (rails/resource-item-resource-type (car ii)))
                           'expand-in-menu)))
                (progn
                  (add-to-list 'result (cons (rails/resource-item-resource-title (car ii))
                                             (car ii)) t))
              (progn
                (dolist (it ii)
                  (add-to-list 'result (cons
                                        (concat (rails/resource-item-resource-title it)
                                                " "
                                                (rails/resource-item-title it))
                                        it) t))))))
        (setq result
              (mapcar
               '(lambda (i)
                  (let ((title (car i)))
                    (when (string= (rails/resource-item-file (cdr i))
                                   (rails/resource-buffer-file rails/current-buffer))
                      (setq title (rails/anything/hightlight-current title)))
                    (cons title
                          (rails/anything/make-filename root (cdr i)))))
               result))
         result))))

(defun rails/anything/associated ()
  (interactive)
  (rails/with-current-buffer
   (let ((root (rails/root)))
     (anything (list anything-c-source-rails-associated)))))

(defun rails/anything/goto-resource-items-alist (root buffer resource)
  (let ((dir (rails/resource-dir resource))
        (type (rails/resource-type resource))
        (comp-alist (rails/resources/get-compared-resources-alist resource))
        file-mask files item)
    (setq file-mask
          (cdr (find resource comp-alist :key 'car)))
    (setq files (rails/directory-files-recursive root dir file-mask))
    (setq files
          (loop for file in files
                for alist    = (rails/resources/compare-file-by-compared-alist (concat dir file)
                                                                               comp-alist)
                for res      = (car alist)
                for res-name = (cdr alist)
                when (eq (rails/resource-type res) type)
                collect
                (make-rails/resource-item
                 :file (concat dir file)
                 :title res-name
                 :resource-type (rails/resource-type resource)
                 :resource-group (rails/resource-group resource)
                 :resource-title res-name)))
    (let ((suffix (rails/resource-file-suffix resource))
          (ext (rails/resource-file-ext resource)))
      (mapcar
       (lambda (i)
         (let ((title (concat (rails/resource-item-title i) suffix ext))
               (file (rails/anything/make-filename root i)))
           (when (string= file (buffer-file-name buffer))
             (setq title (rails/anything/hightlight-current title)))
           (cons title file)))
       files))))

(defun rails/anything/goto (&optional resource-type)
  (interactive)
  (rails/with-root (buffer-file-name)
    (let ((root (rails/root))
          (resources (if resource-type
                         (list (rails/resources/find resource-type))
                       rails/resources/list-defined))
          result)
      (loop for res in  resources
            for cand = (rails/anything/goto-resource-items-alist
                        root
                        (current-buffer)
                        res)
            do
            (add-to-list 'result
                         (list (cons 'name  (rails/resource-title res))
                               (cons 'init  'rails/anything/init-func)
                               (cons 'candidates  cand)
                               (cons 'candidate-number-limit 100)
                               (cons 'type 'file))
                         t))
      (anything result))))

(defun rails/anything/run-with-pattern (pattern)
  (anything (rails/anything/load-triggers) pattern))

;;; ---------------------------------------------------------
;;; - advice anything sources
;;;
(defadvice anything-normalize-sources (around rails-anything activate)
  (let ((sources ad-do-it))
    (when (rails/root)
      (let ((result (rails/anything/load-triggers)))
        (dolist (it sources)
          (add-to-list 'result it t))
        (setq ad-return-value result)))))

(provide 'rails-anything)
