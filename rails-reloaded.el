;;; rails-reloaded.el --- minor mode for editing RubyOnRails code.

;; Copyright (C) 2006 Dmitry Galinsky <dima dot exe at gmail dot com>

;; Authors: Dmitry Galinsky <dima dot exe at gmail dot com>,
;;          Rezikov Peter <crazypit13 (at) gmail.com>

;; Keywords: ruby rails languages

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
(require 'rails-anything)

;;; ---------------------------------------------------------
;;; - Variables
;;;

(defconst rails/version "0.99"
  "emacs-rails version string")

(defvar rails/notify-func-list '(rails/notify-growl))

;;; ---------------------------------------------------------
;;; - Customized variables
;;;

(defgroup rails nil
  "Edit Rails projects with Emacs."
  :group 'programming
  :prefix "rails/")

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

;;; ---------------------------------------------------------
;;; - Buttons
;;;

(define-button-type 'rails/button
  'follow-link t
  'action #'rails/button-action)

;;; ---------------------------------------------------------
;;; - Notify
;;;

(defun rails/notify (string &optional level)
  (message string)
  (when level
    (dolist (i rails/notify-func-list)
      (funcall i string level))))

(defun rails/notify-growl (string level)
  (when (fboundp 'growl)
    (growl "Emacs rails" string)))

;;; ---------------------------------------------------------
;;; - Interactives
;;;

(defun rails/reload-all ()
  (interactive)
  (setq rails/compile/single-file-list nil)
  (setq rails/compile/current-method-list nil)
  (rails-minor-mode-reset-keymap)
  (rails-minor-mode-reset-keymap)
  (rails/bundles/reload))

(defun rails/initialize-for-current-buffer ()
  (interactive)
  (rails/with-root (buffer-file-name)
    (when-bind (file (rails/cut-root (buffer-file-name)))
      (rails/project/apply (rails/root)
                           (current-buffer)
                           (rails/project/read-config (rails/root)))
      (rails/bundles/load)
      (set (make-local-variable 'rails/current-buffer)
           (rails/resources/get-buffer-for-file (rails/root) file))
      (rails-minor-mode t))))

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

(defun rails/goto (&optional resource-type)
  (interactive)
  (rails/anything/goto resource-type))

(defun rails/goto-associated ()
  (interactive)
  (rails/anything/associated))

;;; ---------------------------------------------------------
;;; - Rails minor mode
;;;

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
      ([rails toggle toggle-test]    (cons "Toggle Test/Implementation" 'rails/resources/toggle-test))
      ([rails toggle toggle]         (cons "Toggle" 'rails/resources/toggle))
      ([rails toggle goto]           (cons "Go to..." 'rails/goto-associated))
      ([rails goto]                (cons "Go To" (make-sparse-keymap)))
      ([rails find]                (cons "Find file" 'rails/goto)))
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
      ((rails/short-key "<up>")     'rails/resources/toggle)
      ((rails/short-key "t")        'rails/resources/toggle-test)
      ((rails/short-key "'")        'rails/goto-associated)
      ((rails/short-key ";")        'rails/goto)
      ((rails/short-key "/")        'rails/runner/toggle-output-window)
      ((kbd "\e\e e")               'rails/set-default-environment)
      ((rails/key ".")              'rails/compile/single-file)
      ((rails/key ",")              'rails/compile/current-method))
    map))

(defvar rails-minor-mode-map (rails-minor-mode-default-keymap))

(define-minor-mode rails-minor-mode
  "RubyOnRails"
  nil
  " RoR"
  rails-minor-mode-map)

(provide 'rails-reloaded)