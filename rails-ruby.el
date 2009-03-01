;;; rails-ruby.el --- ruby-mode functions and variables used in rails.

;; Copyright (C) 2006 Dmitry Galinsky <dima dot exe at gmail dot com>

;; Authors: Dmitry Galinsky <dima dot exe at gmail dot com>,
;;          Rezikov Peter <crazypit13 (at) gmail.com>

;; Keywords: ruby languages

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

(require 'inf-ruby)

(defvar rails/ruby/file-suffix ".rb")
(defvar rails/ruby/command "ruby")

(defun rails/ruby/current-method ()
  (let (action
        (re "^ *def +\\([^ (\n]+\\)"))
    (save-excursion
      (end-of-line)
      (when (re-search-backward re nil t)
        (setq action (buffer-substring-no-properties (match-beginning 1) (match-end 1)))))
    action))

(defun rails/ruby/goto-method-in-current-buffer (action)
    (let* (pos
           (cur-pos (point))
           (re (format "^ *def +\\<\\(%s\\)\\>" (regexp-quote action))))
    (save-excursion
      (goto-char (point-min))
      (when-bind (start-pos (re-search-forward re nil t))
        (setq pos start-pos)
        (when (fboundp 'ruby-end-of-defun)
          (ruby-end-of-defun)
          (when (and (< cur-pos (point))
                     (> cur-pos start-pos))
            (setq pos nil)))))
    (when pos
      (goto-char pos)
      (beginning-of-line))))

(defun rails/ruby/run-in-buffer (buf script &optional params)
  "Run CMD as a ruby process in BUF if BUF does not exist."
  (let ((abuf (concat "*" buf "*")))
    (when (not (comint-check-proc abuf))
      (set-buffer (make-comint buf rails/ruby/command nil script params)))
    (inferior-ruby-mode)
    (make-local-variable 'inferior-ruby-first-prompt-pattern)
    (make-local-variable 'inferior-ruby-prompt-pattern)
    (setq inferior-ruby-first-prompt-pattern "^>> "
          inferior-ruby-prompt-pattern "^>> ")
    (setq ruby-buffer abuf)
    (rails-minor-mode t)
    (pop-to-buffer abuf)))

(defun rails/console ()
  (interactive)
  (when-bind (root (rails/root))
    (in-directory root
      (rails/ruby/run-in-buffer "ruby" "script/console" rails/default-environment))))

(provide 'rails-ruby)