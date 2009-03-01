;;; list-ext.el ---

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

(defun list-ext/uniq (list)
  "Return a list of unique elements."
  (let ((result '()))
    (dolist (elem list)
      (when (not (member elem result))
        (push elem result)))
    (nreverse result)))

(defun list-ext/group-by (list func &optional sort)
  (let ((res '()))
    (dolist (it list)
      (let* ((key (funcall func it))
             (res-key (assoc key res)))
        (if res-key
            (push it (cadr res-key))
          (add-to-list 'res (list key (list it))))))
    (setq res
          (mapcar
           #'(lambda(it)
               (list (car it) (nreverse (cadr it))))
           (nreverse res)))
    (when sort
      (setq res (sort* res sort :key 'car)))
    res))

(defun list-ext/options-value (key list)
  (cadr (memq key list)))

(defalias 'opt-val 'list-ext/options-value)

(defun list-ext/swap-tail (key list)
  (let* ((list-len (length list))
         (tail (member key list))
         (beg (- list-len (length tail)))
         (i 0))
    (when tail
      (while (not (zerop (+ beg 1)))
        (add-to-list 'tail (nth i list) t)
        (decf beg)
        (incf i))
      tail)))

(provide 'list-ext)
