;;; core-ext.el ---

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

(defmacro* when-bind ((var expr) &rest body)
  "Binds VAR to the result of EXPR.
If EXPR is not nil exeutes BODY.

 (when-bind (var (func foo))
  (do-somth (with var)))."
  `(let ((,var ,expr))
     (when ,var
       ,@body)))

(defmacro define-keys (key-map &rest key-funcs)
  "Define key bindings for KEY-MAP (create KEY-MAP, if it does
not exist."
  `(progn
     (unless (boundp ',key-map)
       (setf ,key-map (make-keymap)))
     ,@(mapcar
        #'(lambda (key-func)
            `(define-key ,key-map ,(first key-func) ,(second key-func)))
        key-funcs)
     ,key-map))

(defmacro funcs-chain (&rest list-of-funcs)
  `(lambda(it)
     (dolist (l (quote ,list-of-funcs))
       (setq it (funcall l it)))
     it))

(defmacro in-directory (dir &rest body)
  `(let ((default-directory ,dir))
     ,@body))

(provide 'core-ext)