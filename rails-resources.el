;;; rails-resources.el ---

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
(require 'core-ext)
(require 'inflections)
(require 'rails-lib)

(defvar rails/current-buffer nil)

(defstruct rails/resource type ;; symbol, required
                          title ;; string, required
                          group ;; string required
                          dir file-ext file-suffix skip-file-suffix ;; string
                          options ;; (availabled 'pluralize 'expand-in-menu), list or symbol
                          file-pattern ;; pattern
                          test-to ;; symbol
                          toggle ;; alist (toggle-to-func . toggle-from-func)
                          (weight 1)) ;; integer required

(defstruct rails/resource-buffer type
                                 title
                                 file
                                 (weight 1)) ;; must be integer

(defstruct rails/resource-item file
                               title
                               resource-type
                               resource-group
                               resource-title)

(defvar rails/resources/list-defined nil)

;;; ---------------------------------------------------------
;;; - Struct functions
;;;

(defun rails/resource-options-p (resource test)
  (not (not (memq test (rails/resource-options resource)))))

;;; ---------------------------------------------------------
;;; - Menu functions
;;;

(defun rails/resources/add-goto-menu (resource)
  (let ((title (rails/resource-title resource))
        (type  (rails/resource-type resource)))
    (define-key rails-minor-mode-map
      (merge 'vector [menu-bar rails goto]
             (list type)
             'eq)
      (list 'menu-item (pluralize-string title)
            `(lambda()
               (interactive)
               (rails/goto ',type))))))

;;; ---------------------------------------------------------
;;; - CRUD functions
;;;

(defun* rails/defresource (type title &key group
                                           dir file-suffix file-ext
                                           skip-file-suffix
                                           file-pattern
                                           options
                                           test-to
                                           toggle
                                           weight)
  (when (rails/resources/find type)
    (error (format "Resource %s already defined" type)))
  (let (res)
    (setq res (make-rails/resource :type type
                                   :group (if group (symbol-name group) "acore")
                                   :title title
                                   :dir (if (string-ext/end-p dir "/") dir (concat dir "/"))
                                   :file-suffix file-suffix
                                   :skip-file-suffix skip-file-suffix
                                   :file-ext  (if (string-ext/start-p file-ext ".")
                                                  file-ext
                                                (when file-ext
                                                  (concat "." file-ext)))
                                   :options (if (listp options) options (list options))
                                   :test-to (if (symbolp test-to)
                                                test-to
                                              (error "rails/resource#test-to must be the symbol"))
                                   :file-pattern file-pattern
                                   :toggle toggle
                                   :weight (if (not weight) 1 weight)))
    (add-to-list 'rails/resources/list-defined res t)
    (rails/resources/add-goto-menu res)))

(defun rails/resources/find (resource-type)
  (when (stringp resource-type)
    (setq resource-type (intern resource-type)))
  (find resource-type rails/resources/list-defined :key 'rails/resource-type))

(defun rails/resources/delete (resource-type)
  (setq
   rails/resources/list-defined
   (remove* resource-type rails/resources/list-defined :key 'rails/resource-type)))

(defun rails/resources/clear ()
  (setq rails/resources/list-defined nil))

(defun rails/resources/find-file-by-item (root item)
  (when (rails/resource-item-p item)
    (rails/find-file root (rails/resource-item-file item))))

(defun rails/resources/notify-item (item &optional title file)
  (rails/notify (format "%s: %s"
                        (if title title (rails/resource-item-resource-title item))
                        (if file file (rails/resource-item-file item)))
                :notice))

;;; ---------------------------------------------------------
;;; - Comparable functions
;;;
(defun rails/resources/get-compared-resources-alist (resource-or-file)
  (let (resources)
    (setq resources
          (loop for res in rails/resources/list-defined
                when (or
                      (and (rails/resource-p resource-or-file)
                           (string-ext/start-p (rails/resource-dir res)
                                               (rails/resource-dir resource-or-file)))
                      (and (stringp resource-or-file)
                           (string-ext/start-p resource-or-file
                                               (rails/resource-dir res))))
                collect res))
    (when (rails/resource-p resource-or-file)
      (add-to-list 'resources resource-or-file))
    (loop for res in resources
          for file-mask = ""
          for file-suffix = ""
          collect
          (progn
            ;; skip-file-suffix
            (when-bind (skip-file-suffix (rails/resource-skip-file-suffix res))
              (setq file-suffix skip-file-suffix))
            ;; file-suffix
            (when-bind (file-suffix (rails/resource-file-suffix res))
              (setq file-mask (concat file-mask file-suffix)))
            ;; file-ext
            (when-bind (file-ext (rails/resource-file-ext res))
              (setq file-mask (concat file-mask file-ext)))
            (setq file-mask (format "\\(.*%s\\)%s$"
                                    (regexp-quote file-suffix)
                                    (regexp-quote file-mask)))
            (cons res
                  file-mask)))))

(defun rails/resources/compare-file-by-compared-alist (file alist)
  (let ((max-weight 0)
        max-match
        max-res)
    (loop for (res . regexp) in alist
          for dir    = (rails/resource-dir res)
          for in-dir = (string-ext/start-p file dir)
          for match  = (string-ext/string=~ regexp
                                            (string-ext/cut file
                                                            dir
                                                            :begin)
                                            $1)
          when (and in-dir
                    match
                    (> (rails/resource-weight res) max-weight))
          do
          (progn
            (setq max-weight (rails/resource-weight res)
                  max-match match
                  max-res res)))
    (when max-res
      (cons max-res max-match))))

;;; ---------------------------------------------------------
;;; - Lookup resource-buffer for file
;;;

(defun rails/resources/get-buffer-for-file(root file)
  (let* ((comp-alist (rails/resources/get-compared-resources-alist file))
         (res-alist (rails/resources/compare-file-by-compared-alist file
                                                                    comp-alist))
         res-name
         res)
    (when res-alist
      (setq res-name (cdr res-alist)
            res      (car res-alist))
      (when (rails/resource-options-p res 'pluralize)
        (setq res-name (pluralize-string res-name)))
      (when-bind (pattern (rails/resource-file-pattern res))
        (setq pattern (replace-regexp-in-string "{name}" "\\\\(.*\\\\)" pattern))
        (setq res-name (string-ext/string=~ pattern res-name $1)))
      (make-rails/resource-buffer :type (rails/resource-type res)
                                  :title res-name
                                  :file file
                                  :weight (rails/resource-weight res)))))

;;; ---------------------------------------------------------
;;; - Lookup associated resources by resource-buffer
;;;

(defun rails/resources/resource-files-to-items (resource files)
  (unless (listp files)
    (setq files (list files)))
  (loop for it in files collect
        (let ((name it)
              (file it)
              (dir (rails/resource-dir resource)))
          (when (consp it)
            (setq name (car it)
                  file (cdr it)))
          (unless (string-ext/start-p file dir)
            (setq file (concat dir file)))
          (make-rails/resource-item :title name
                                    :file file
                                    :resource-type (rails/resource-type resource)
                                    :resource-group (rails/resource-group resource)
                                    :resource-title (rails/resource-title resource)))))

(defun rails/resources/filter-dublicated-files-in-items (items)
  (let (result deleted)
    (dolist (res-items items)
      (dolist (it res-items)
        (let* ((file (rails/resource-item-file it))
               (exist (find file result :test 'string= :key 'car)))
          (if exist
              (let* ((it-exist (cdr exist))
                     (res  (rails/resources/find (rails/resource-item-resource-type it)))
                     (eres (rails/resources/find (rails/resource-item-resource-type it-exist)))
                     (w    (rails/resource-weight res))
                     (ew   (rails/resource-weight eres)))
                (if (> w ew) ;; if weight if geater that exist weight
                    (progn
                      (delete* file result :test 'string= :key 'car) ;; remove from alist
                      (add-to-list 'deleted it-exist)                ;; place item to deleted
                      (add-to-list 'result (cons file it)))          ;; append a new item
                  (add-to-list 'deleted it)))
            (add-to-list 'result (cons file it)))))) ;; save files alist
    (setq result nil)
    (dolist (res-items items)
      (let ((its res-items))
        (dolist (del deleted)
          (setq its (remove* del its :test 'equal)))
        (when its
          (add-to-list 'result its t))))
    result))

(defun rails/resources/get-associated-items-by-resource (root rails-buffer resource &optional unused-no-buffer-filter)
  (let ((file (rails/resource-buffer-title rails-buffer))
        result name pattern-p)
    (setq result
          (progn
            (setq name file)
            ;; singularize
            (when-bind (pluralize (rails/resource-options-p resource 'pluralize))
              (setq file (singularize-string file)))
            ;; pattern
            (when-bind (pattern (rails/resource-file-pattern resource))
              (setq file (replace-regexp-in-string "{name}" file pattern))
              (setq pattern-p t))
            ;; dir
            (when-bind (dir (rails/resource-dir resource))
              (setq file (concat dir file)))
            ;; skip-file-suffix
            (when-bind (skip-file-suffix (rails/resource-skip-file-suffix resource))
              (setq file (concat file skip-file-suffix)))
            ;; file-suffix
            (when-bind (file-suffix (rails/resource-file-suffix resource))
              (setq file (concat file file-suffix)))
            ;; file-ext
            (when-bind (file-ext (rails/resource-file-ext resource))
              (setq file (concat file file-ext)))
            (if pattern-p
                (let ((dir (file-name-directory file))
                      (file (file-name-nondirectory file))
                      files)
                  (loop for it in (rails/directory-files-recursive root dir file)
                        collect (cons it (concat dir it))))
              (when (file-exists-p (concat root file))
                (list (cons name file))))))
    (rails/resources/resource-files-to-items resource result)))

(defun rails/resources/get-associated-items(root rails-buffer)
  (rails/resources/filter-dublicated-files-in-items
   (loop for res in rails/resources/list-defined
         for items = (rails/resources/get-associated-items-by-resource root rails-buffer res)
         when items
         collect items)))

(defun rails/resources/get-associated-resources(root rails-buffer)
  (loop for res in rails/resources/list-defined
        for items = (rails/resources/get-associated-items-by-resource root rails-buffer res)
        when items
        collect res))


;;; ---------------------------------------------------------
;;; - Toggle resource
;;;

(defun rails/resources/toggle ()
  (interactive)
  (rails/with-current-buffer
   (let* ((root (rails/root))
          (resource (rails/resources/find
                     (rails/resource-buffer-type rails/current-buffer)))
          (toggle (cdr (rails/resource-toggle resource)))) ; toggle from current
     (unless (and
              toggle
              (funcall toggle root rails/current-buffer))
       (setq toggle
             (loop for res in rails/resources/list-defined
                   for func = (car (rails/resource-toggle res))
                   for result = (when func
                                  (funcall func root rails/current-buffer))
                   when result
                   return t))
       (unless toggle
         (rails/notify "Can't toggle" :error))))))


;;; ---------------------------------------------------------
;;; - Toggle tests
;;;

(defun rails/resources/test-buffer-p (root rails-buffer)
  "Return resource contains :test-to link, otherwise return nil."
  (let ((resource (rails/resources/find
                   (rails/resource-buffer-type rails-buffer))))
    (when (rails/resource-test-to resource)
      resource)))

(defun rails/resources/get-associated-test-item-for-buffer (root rails-buffer)
  (let ((test-res (rails/resources/test-buffer-p root rails-buffer)))
    (if test-res
        ;; it's the test, run it
        (make-rails/resource-item :file (rails/resource-buffer-file rails-buffer)
                                  :title (rails/resource-buffer-title rails-buffer)
                                  :resource-type (rails/resource-type test-res)
                                  :resource-group (rails/resource-group test-res)
                                  :resource-title (rails/resource-title test-res))
      ;; it's have the link to test
      (rails/resources/test-item-by-buffer
       root
       rails-buffer))))

(defun rails/resources/test-item-by-buffer (root rails-buffer)
  (let* ((type (rails/resource-buffer-type rails-buffer))
         (resource (rails/resources/find type))
         (assoc-ress (rails/resources/get-associated-resources root rails-buffer))
         test-res)
    (setq
     test-res
     (loop for res in assoc-ress
           for test = (eq type (rails/resource-test-to res))
           when test
           return res))
    (when test-res
      (let ((items (rails/resources/get-associated-items-by-resource
                    root
                    rails-buffer
                    test-res))
            (bfile (regexp-quote (file-name-nondirectory
                                  (rails/resource-buffer-file rails-buffer)))))
        (if (= 1 (length items))
            (car items)
          ;; try find in multiple items by regexp (eq using to find view spec)
          (loop for it in items
                for match = (string-ext/string=~
                             bfile
                             (rails/resource-item-file it)
                             it)
                when match
                return it))))))

(defun rails/resources/item-by-test-buffer (root rails-buffer)
  (let* ((type (rails/resource-buffer-type rails-buffer))
         (resource (rails/resources/find type)))
    (when-bind (test-res (rails/resources/find (rails/resource-test-to resource)))
      (when-bind (items (rails/resources/get-associated-items-by-resource
                         root
                         rails-buffer
                         test-res))
        (if (= 1 (length items))
            (car items)
          ;; try find in multiple items by regexp (eq using to find view spec)
          (loop for it in items
                for bfile = (regexp-quote (file-name-nondirectory
                                           (rails/resource-item-file it)))
                for match = (string-ext/string=~
                             bfile
                             (rails/resource-buffer-file rails-buffer)
                             it)
                when match
                return it))))))

(defun rails/resources/toggle-test ()
  (interactive)
  (rails/with-current-buffer
   (let ((root (rails/root))
         item)
     (setq item
           (if (rails/resources/test-buffer-p root rails/current-buffer)
               (rails/resources/item-by-test-buffer root rails/current-buffer)
             (rails/resources/test-item-by-buffer root rails/current-buffer)))
     (if item
         (progn
           (rails/resources/find-file-by-item root item)
           (rails/resources/notify-item item))
       (rails/notify "Can't toggle")))))

(provide 'rails-resources)
