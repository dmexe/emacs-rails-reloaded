;;; rails-reloaded.el --- minor mode for editing RubyOnRails code.

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

;;; Commentary:

;; Basic steps to setup:
;;   1. Setup  `load-path':
;;      (setq load-path (cons (expand-file-name "~/.emacs.d/rails-reloaded") load-path))
;;   2. Put in your .emacs file:
;;      (require 'rails-autoload)
;;   3. To bytecompile after emacs restarted, type:
;;      [M-x] rails/bytecompile
;;   4. Optional: run unit tests for rails-reloaded:
;;      [M-x] rails/selftest

;;; Code:

(require 'cl)

(require 'core-ext)
(require 'string-ext)
(require 'files-ext)
(require 'list-ext)
(require 'inflections)

(require 'rails-ruby)
(require 'rails-lib)
(require 'rails-runner)
(require 'rails-compile)
(require 'rails-project)
(require 'rails-resources)
(require 'rails-bundles)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Variables
;;

(defconst rails/version "0.99"
  "emacs-rails version string")

(defvar rails/current-buffer nil)

(defvar rails/bundles-list '(controller
                             mailer
                             helper
                             model
                             observer
                             migration

                             unit-test
                             functional-test
                             fixture

                             rspec-controller
                             rspec-helper
                             rspec-model
                             rspec-fixture

                             view
                             javascript
                             rake
                             generator
                             database
                             webserver
                             test-unit
                             rspec)
  "List of availabled bundles, don't edit the list manualy.
To disable bundle loading setup the `rails/disabled-bundles' variable.")

(setq rails/bundles-list nil)

(defvar rails/bundles-func-list '()
  "Cached bundles functions, don't edit the list manualy.

Structure of this list:

((\"not-existing-func\" nil)
 (\"func-name\"
   (rails/bundle/func . bundle)
   (rails/an-bundle/func . an-bundle))
 ...)")

(defvar rails/bundles-group-list '())

(defvar rails/bundles-loaded-p nil
  "Non nil if all bundles from rails/bundles list are loaded.")

(defvar rails/resource-types-list '())
(defvar rails/layouts-list '())
(defvar rails/linked-types-alist '())
(defvar rails/bundles-group-list '())
(defvar rails/disabled-bundles-group-list '())
(defvar rails/notify-func-list '(rails/notify-growl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Customized variables
;;

(defgroup rails nil
  "Edit Rails projects with Emacs."
  :group 'programming
  :prefix "rails/")

(defcustom rails/disabled-bundles '()
  "Disabled bundles."
  :group 'rails
  :type '(repeat (symbol :tag "Bundle name")))

(defcustom rails/display-menu-method 'popup
  "Display menu method."
  :group 'rails
  :type '(choice (const :tag "Default" nil)
                 (const :tag "Using Popup Menu" popup)
                 (const :tag "Using ido-completion" ido)))

(defcustom rails-minor-mode-hook nil
  "Hook run when entering Rails minor mode."
  :type 'hook
  :group 'rails)

(defcustom rails/default-environment "development"
  "Default Railt environment."
  :type 'string
  :group 'rails)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Structures
;;

(defstruct rails/buffer type
                        name
                        resource-name
                        layout
                        file
                        (weight 1))

(defstruct rails/goto-item group
                           name
                           file
                           (weight 1)
                           func)

(define-button-type 'rails/button
  'follow-link t
  'action #'rails/button-action)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Begin functions list
;;

(defun rails/load-bundles () )
  "Loading bundles from `rails/bundles'."
  (unless rails/bundles-loaded-p
    (dolist (bundle rails/bundles-list)
      (let* ((name (string-ext/from-symbol bundle))
             (name (concat "rails-" name "-bundle")))
;             (name (intern name)))
        (load (concat (file-name-directory (locate-library "rails-reloaded")) "bundles/" name))
        (when-bind (load-func (rails/bundle-func bundle "load"))
          (apply load-func (list))))))
    (setq rails/bundles-loaded-p t))

(defun rails/initialize-bundles (root file rails-current-buffer)
  (let ((file (rails/cut-root file)))
    (dolist (func (rails/bundles-func "initialize"))
      (apply func (list root file rails-current-buffer)))))

(defun rails/determine-type-of-file (root file &optional rails-buffer)
  (if (and rails-buffer
           (string= (rails/cut-root file) (rails/buffer-file rails-buffer))
           (rails/buffer-type rails-buffer)
           (rails/buffer-name rails-buffer)
           (rails/buffer-resource-name rails-buffer))
      rails-buffer
    (progn
      (let ((strip-file (rails/cut-root file))
            found-rails-buffer
            (weight 0))
        (when strip-file
          (dolist (func (rails/bundles-func "determine-type-of-file"))
            (when-bind (buf (apply func (list root strip-file)))
              (when (and (rails/buffer-p buf)
                         (rails/buffer-type buf)
                         (rails/buffer-name buf)
                         (rails/buffer-resource-name buf)
                         (rails/buffer-weight buf)
                         (> (rails/buffer-weight buf) weight))
                (setq found-rails-buffer (copy-rails/buffer buf))
                (setq weight (rails/buffer-weight buf))
                (unless (rails/buffer-file found-rails-buffer)
                  (setf (rails/buffer-file found-rails-buffer) strip-file))
                (unless (rails/buffer-layout found-rails-buffer)
                  (setf (rails/buffer-layout found-rails-buffer)
                        (rails/layout-for-type (rails/buffer-type found-rails-buffer)))))))
          (when (rails/buffer-p found-rails-buffer)
            (if rails-buffer
                (setq rails-buffer (copy-rails/buffer found-rails-buffer))
              found-rails-buffer)))))))

(defun rails/goto-item-alist-from-file (root file rails-buffer)
  (let ((goto-item-list '()))
    (dolist (func (rails/bundles-func "goto-item-from-file"))
      (let ((line (apply func (list root (rails/cut-root file) rails-buffer))))
        (unless (listp line)
          (setq line (list line)))
        (dolist (it line)
          (when (and (rails/goto-item-p it)
                     (not (string= (rails/goto-item-file it)
                                   (rails/buffer-file rails-buffer))))
            (add-to-list 'goto-item-list it t)))))
    (list-ext/group-by
     goto-item-list #'(lambda (it) (rails/goto-item-group it)))))

(defun rails/menu-from-goto-item-alist (root title goto-alist)
  (let (menu item last-p)
    (dolist (alist goto-alist)
      (let ((group (car alist))
            (list (cadr alist)))
        (when last-p
          (add-to-list 'menu (list "--" group) t))
        (dolist (it list)
          (add-to-list 'menu (cons (rails/goto-item-name it) it) t)))
      (unless last-p
        (setq last-p t)))
    (when (> (length menu) 0)
      (when (setq item (rails/display-menu title menu))
        (if (rails/goto-item-func item)
            (funcall (rails/goto-item-func item) item)
          (rails/find-file-by-goto-item root item))))))

(defun rails/find-file-by-goto-item (root goto-item)
  (when goto-item
    (when-bind (file (rails/goto-item-file goto-item))
      (when (and (not (string= (concat root file) (buffer-file-name)))
                 (rails/file-exist-p root file))
        (rails/find-file root file)
        (when rails/current-buffer
          (rails/notify-by-rails-buffer rails/current-buffer))))
    goto-item))

(defun rails/toggle-file-by-goto-item (root goto-item)
  (let ((action (rails/current-buffer-action-name)))
    (rails/find-file-by-goto-item root goto-item)
    (when (and action
               (rails/bundles-func-by-buffer rails/current-buffer "goto-action-in-current-buffer"))
      (rails/goto-action-in-current-buffer action)
      (rails/notify-by-rails-buffer rails/current-buffer action))
    goto-item))

(defun rails/current-buffer-action-name ()
  (when (rails/buffer-p rails/current-buffer)
    (when-bind (func (rails/bundles-func-by-buffer
                      rails/current-buffer "current-buffer-action-name"))
      (funcall func))))

(defun rails/goto-action-in-current-buffer (action)
  (when (and (rails/buffer-p rails/current-buffer)
             action)
    (when-bind (func (rails/bundles-func-by-buffer
                      rails/current-buffer "goto-action-in-current-buffer"))
      (funcall func action))))

(defun rails/notify (string &optional level)
  (message string)
  (when level
    (dolist (i rails/notify-func-list)
      (funcall i string level))))

(defun rails/notify-by-rails-buffer (rails-buffer &optional action-name)
  (rails/notify
   (if action-name
       (format "%s %s#%s"
               (string-ext/cut (format "%s" (rails/buffer-type rails-buffer)) ":" :begin)
               (rails/buffer-name rails-buffer)
               action-name)
     (format "%s %s"
             (string-ext/cut (format "%s" (rails/buffer-type rails-buffer)) ":" :begin)
             (rails/buffer-name rails-buffer)))))

(defun rails/notify-growl (string level)
  (when (fboundp 'growl)
    (growl "Emacs rails" string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Interactives
;;

(defun rails/reload-bundles ()
  (interactive)
  (setq rails/bundles-loaded-p    nil)
  (setq rails/bundles-func-list   nil)
  (setq rails/bundles-group-list  nil)
  (setq rails/layouts-list        nil)
  (setq rails/resource-types-list nil)
  (setq rails/linked-types-alist  nil)
  (setq rails/compile/single-file-list nil)
  (setq rails/compile/current-method-list nil)
  (rails-minor-mode-reset-keymap)
  (rails/load-bundles))

(defun rails/goto-from-current-file ()
  (interactive)
  (rails/with-current-buffer
   (let ((list (rails/goto-item-alist-from-file
                (rails/root)
                (buffer-file-name)
                rails/current-buffer))
         (title (capitalize (string-ext/from-symbol
                             (rails/buffer-type rails/current-buffer)))))
     (rails/menu-from-goto-item-alist (rails/root) (format "Go to from %s to..." title) list))))

(defun rails/toggle-current-file ()
  (interactive)
  (rails/with-current-buffer
   (let* ((file (rails/cut-root (buffer-file-name)))
          (curlay (rails/buffer-layout rails/current-buffer))
          (layouts (list-ext/swap-tail (rails/buffer-type rails/current-buffer)
                                       (rails/layout-childs curlay))))
     (when (and curlay layouts)
       (when-bind (goto-item
                   (loop for layout in (cdr layouts)
                         for func = (rails/bundle-func layout "goto-item-from-rails-buffer")
                         for item = (when func (funcall func (rails/root) file rails/current-buffer))
                         when (and (rails/goto-item-p item)
                                   (not (string= (rails/buffer-file rails/current-buffer)
                                                 (rails/goto-item-file item))))
                         do (return item)))
         (rails/toggle-file-by-goto-item (rails/root) goto-item))))))

(defun rails/toggle-current-file-by-link ()
  (interactive)
  (rails/with-current-buffer
   (let* ((type (rails/buffer-type rails/current-buffer))
          (link-to (or (rails/type-link-for :tests type)                  ; the directly link
                       (rails/type-link-for
                        :tests
                        (rails/buffer-layout rails/current-buffer)))))    ; link to the layout
     (when link-to
       (when-bind (func (rails/bundle-func link-to "goto-item-from-rails-buffer"))
         (let ((goto-item (funcall func (rails/root)
                                   (rails/cut-root (buffer-file-name))
                                   rails/current-buffer)))
           (when (and (rails/goto-item-p goto-item)
                      (not (string= (rails/buffer-file rails/current-buffer)
                                    (rails/goto-item-file goto-item))))
             (rails/toggle-file-by-goto-item (rails/root) goto-item))))))))

(defun rails/initialize-for-current-buffer ()
  (interactive)
  (rails/with-root (buffer-file-name)
    (when-bind (file (rails/cut-root (buffer-file-name)))
      (rails/project/apply (rails/root)
                           (current-buffer)
                           (rails/project/read-config (rails/root)))
      (rails/load-bundles)
      (set (make-local-variable 'rails/current-buffer)
           (rails/resources/get-buffer-for-file (rails/root) file))
;;           (rails/determine-type-of-file (rails/root) file))
      (rails-minor-mode t)
      (rails/initialize-bundles (rails/root) file rails/current-buffer))))

(defun rails/set-default-environment (&optional env)
  (interactive)
  (when-bind (root (rails/root))
    (unless env
      (setq env (rails/completing-read "Default environment"
                                       (rails/environments root)
                                       t
                                       rails/default-environment)))
    (when (not (string-ext/empty-p env))
      (setq rails/default-environment env))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Rails minor mode
;;

(defcustom rails-minor-mode-prefix-key "\C-c"
  "Key prefix for Rails minor mode."
  :group 'rails
  :type 'string)

(defcustom rails-minor-mode-prefix2-key "\C-c"
  "Additional key prefix for Rails minor mode."
  :group 'rails
  :type 'string)

(defun rails-minor-mode-reset-keymap ()
  (setf rails-minor-mode-map (rails-minor-mode-default-keymap))
  (setf (cdr (assoc 'rails-minor-mode minor-mode-map-alist))
        rails-minor-mode-map))

(defun rails-minor-mode-menu-bar-map ()
  (let ((map (make-sparse-keymap)))
    (define-keys map
      ([rails] (cons "RoR" (make-sparse-keymap "RubyOnRails")))
      ([rails version]            '(menu-item (concat "Version: " rails/version) 'foo :enable nil))
      ([rails bundles-separator]   (cons "--" "--"))
      ([rails bundles-title]      '(menu-item "Bundles:" "--" :enable nil))
      ([rails separator]           (cons "--" "--"))
      ([rails env]                '(menu-item "Environments" (make-sparse-keymap)
                                              :filter rails-minor-mode-environments-menu))
      ([rails project]             (cons "Project" (make-sparse-keymap)))
      ([rails project update]        (cons "Apply Project Settings"  'rails/project/update))
      ([rails project edit]          (cons "Edit Project Settings" 'rails/project/edit))
      ([rails env-separator]       (cons "--" "--"))
      ([rails toggle]              (cons "Go To From Current File" (make-sparse-keymap)))
      ([rails toggle separator]      (cons "--" "--"))
      ([rails toggle toggle-test]    (cons "Toggle Test/Implementation" 'rails/toggle-current-file-by-link))
      ([rails toggle goto]           (cons "Go to..." 'rails/goto-from-current-file))
      ([rails toggle toggle]         (cons "Toggle" 'rails/toggle-current-file))
      ([rails goto]                (cons "Go To" (make-sparse-keymap))))
  map))

(defun rails-minor-mode-environments-menu (&optional args)
  (let ((map (make-sparse-keymap))
        (envs (rails/environments (rails/root))))
    (dolist (e envs)
      (define-key map
        (vector (string-ext/safe-symbol e))
        (list 'menu-item
              (capitalize e)
              `(lambda()(interactive)(rails/set-default-environment ,e))
              :button (cons :radio (rails/default-environment-p e)))))
    (define-key map [separator] (cons "--" "--"))
    (define-key map [toggle]    (cons "Set Default Environment" 'rails/set-default-environment))
    map))

(defun rails-minor-mode-default-keymap ()
  (let ((map (make-keymap)))
    (define-keys map
      ([menu-bar] (rails-minor-mode-menu-bar-map))
      ((rails/short-key "<down>") 'rails/resources/goto-associated)
      ((rails/short-key "<up>")   'rails/resources/toggle)
      ((rails/short-key "t")      'rails/resources/toggle-test)
      ((rails/short-key "/")      'rails/runner/toggle-output-window)
      ((kbd "\e\e e")             'rails/set-default-environment)
      ((rails/key ".")            'rails/compile/single-file)
      ((rails/key ",")            'rails/compile/current-method))
    map))

(defvar rails-minor-mode-map (rails-minor-mode-default-keymap))

(define-minor-mode rails-minor-mode
  "RubyOnRails"
  nil
  " RoR"
  rails-minor-mode-map)

(provide 'rails-reloaded)