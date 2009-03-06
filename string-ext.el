;;; string-ext.el ---

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

(defun string-ext/cut (string cut from)
  "Cut from STRING fragment CUT from FROM,
FROM must equal :begin or :end. Return result string,
else return nil."
  (cond
    ((and (eq from :begin)
          (string-ext/start-p string cut))
     (substring string (length cut) (length string)))
    ((and (eq from :end)
          (string-ext/end-p string cut))
     (substring string 0 (- (length string) (length cut))))))
;;    (t string)))

(defun string-ext/cut-safe (string cut from)
  (let ((str (string-ext/cut string cut from)))
    (if str
        str
      string)))

(defun string-ext/from-symbol (sym)
  "Convert symbol SYM to string."
  (string-ext/cut-safe (format "%s" sym) ":" :begin))

(defun string-ext/start-p (string start)
  "Return t if STRING start with START, else return nil."
  (let ((len (length start))
        (orig-len (length string)))
    (when (<= len orig-len)
      (string= start (substring string 0 len)))))

(defun string-ext/end-p (string end)
  "Return t if STRING end with END, else return nil."
  (let* ((len (length end))
         (orig-len (length string))
         (from (- orig-len len)))
    (when (<= len orig-len)
      (string= end (substring string from orig-len)))))

(defmacro string-ext/string=~ (regex string &rest body)
  "regex matching similar to the =~ operator found in other languages."
  (let ((str (gensym)))
    `(lexical-let ((,str ,string))
       ;; Use lexical-let to make closures (in flet).
       (when (string-match ,regex ,str)
         (symbol-macrolet ,(loop for i to 9 collect
                                 (let ((sym (intern (concat "$" (number-to-string i)))))
                                   `(,sym (match-string ,i ,str))))
           (flet (($ (i) (match-string i ,str))
                  (sub (replacement &optional (i 0) &key fixedcase literal-string)
                       (replace-match replacement fixedcase literal-string ,str i)))
             (symbol-macrolet ( ;;before
                               ($b (substring ,str 0 (match-beginning 0)))
                               ;;match
                               ($m (match-string 0 ,str))
                               ;;after
                               ($a (substring ,str (match-end 0) (length ,str))))
               ,@body)))))))

(defun string-ext/decamelize (string)
  "Convert from camel_case/string to CamelCase::String."
  (let ((case-fold-search nil))
    (replace-regexp-in-string " " ""
      (replace-regexp-in-string "  " "::"
        (capitalize
         (replace-regexp-in-string "\_" " "
           (replace-regexp-in-string "\/" "  " string)))))))

(defalias 'decamelize-string  'string-ext/decamelize)

(defun string-ext/camelize (string)
  "Convert from CamelCase::String to camel_case/string."
  (let ((case-fold-search nil))
    (downcase
     (replace-regexp-in-string "::" "/"
       (replace-regexp-in-string
        "\\([A-Z]+\\)\\([A-Z][a-z]\\)" "\\1_\\2"
          (replace-regexp-in-string
            "\\([a-z\\d]\\)\\([A-Z]\\)" "\\1_\\2"
            string))))))

(defalias 'camelize-string  'string-ext/camelize)

(defun string-ext/empty-p (str) ;(+)
  "Return t if string STR is not empty."
  (not (and (stringp str)
            (not (string-equal "" str))
            (not (string-match "^ +$" str)))))

(defun string-ext/safe-symbol (str)
  "Return symbol from STR and replace and non word chars to '-'."
  (intern (replace-regexp-in-string "[^a-zA-z0-9]+" "-"
           (downcase str))))

(provide 'string-ext)