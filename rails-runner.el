;;; rails-runner.el --- run external process.

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

(require 'font-lock)
(require 'ansi-color)
(require 'rails-proxy)

(defvar rails/runner/buffer-name "*ROutput*")
(defvar rails/runner/buffer-rails-root nil)
(defvar rails/runner/output-mode-hook nil)
(defvar rails/runner/show-buffer-hook nil)
(defvar rails/runner/after-stop-func-list nil)
(defvar rails/runner/script-name nil)

(defun rails/runner/running-p ()
  (get-buffer-process rails/runner/buffer-name))

(defun rails/runner/setup-output-buffer ()
  "Setup default variables and values for the output buffer."
  (set (make-local-variable 'scroll-margin) 0)
;;   (set (make-local-variable 'scroll-preserve-screen-position) nil)
  (make-local-variable 'after-change-functions)
  (rails-minor-mode t))

(defun rails/runner/scroll-up ()
  (with-current-buffer (get-buffer rails/runner/buffer-name)
    (goto-char (point-min))
    (vertical-motion (- next-screen-context-lines))
    (when-bind (win (get-buffer-window rails/runner/buffer-name))
      (set-window-start win (point-min)))))

(defun rails/runner/popup-buffer (&rest args)
  ;; args not used, added to compatibility of rails/runner/after-stop-func-list
  (unless (get-buffer-window rails/runner/buffer-name)
    (pop-to-buffer rails/runner/buffer-name t t)
    (shrink-window-if-larger-than-buffer
     (get-buffer-window rails/runner/buffer-name))
    (run-hooks 'rails/runner/show-buffer-hook)
    (other-window 1)))

(defun rails/runner/popup-buffer-if-failed (retval)
  (unless (zerop retval)
    (rails/runner/popup-buffer)))

(defun rails/runner/toggle-output-window ()
  (interactive)
  (let ((buf (get-buffer rails/runner/buffer-name)))
    (if buf
        (if (get-buffer-window rails/runner/buffer-name 'visible)
            (delete-windows-on buf)
          (rails/runner/popup-buffer))
      (rails/notify "No output window found. Try running a script or a rake task before."))))

(defun rails/runner/setup-font-lock (&optional keywords)
  (font-lock-add-keywords nil keywords nil)
  (font-lock-mode t))

(define-derived-mode rails/runner/output-mode fundamental-mode "ROutput"
  "Major mode to Rails Script Output."
  (rails/runner/setup-output-buffer)
  (set (make-local-variable 'font-lock-keywords-only) t)
  (set (make-local-variable 'font-lock-keywords) nil)
  (buffer-disable-undo)
  (setq buffer-read-only t))

(defun rails/runner/sentinel-proc (proc msg)
  (let* ((name rails/runner/script-name)
         (ret-val (process-exit-status proc))
         (buf (get-buffer rails/runner/buffer-name))
         (ret-message (if (zerop ret-val) "successful" "failure"))
         msg)
    (when (memq (process-status proc) '(exit signal))
      (rails/runner/scroll-up)
      (setq rails/runner/script-name nil
            msg (format "%s was stopped (%s)." name ret-message)))
    (message (replace-regexp-in-string "\n" "" msg))
    (when rails/runner/after-stop-func-list
      (dolist (func rails/runner/after-stop-func-list)
        (with-selected-window (or (get-buffer-window buf)
                                  (get-buffer-window (current-buffer)))
          (with-current-buffer buf
            (goto-char (point-min))
            (funcall func ret-val))))
      ret-val)))

(defun rails/runner/prepare-buffer (proc)
  (set-process-filter proc 'ansi-color-insertion-filter)
  (set-process-coding-system proc 'utf-8 'utf-8))

(defun rails/runner/run (root command parameters &rest options)
  "Run a Rails script COMMAND with PARAMETERS in ROOT with
BUFFER-MAJOR-MODE."
  (save-some-buffers)
  (if (rails/runner/running-p)
      (message "Only one instance rails-script allowed")

    (setq rails/runner/after-stop-func-list nil)
    (setq rails/runner/buffer-rails-root root)
    (when (get-buffer rails/runner/buffer-name)
      (with-current-buffer (get-buffer rails/runner/buffer-name)
        (let ((buffer-read-only nil))
          (delete-region (point-min) (point-max)))))
    (in-directory (if (string-ext/end-p root "/")
                      root (concat root "/"))
      (let ((proc (rails/proxy/shell-command root
                                             rails/runner/buffer-name
                                             rails/runner/buffer-name
                                             command
                                             parameters)))
        (rails/runner/prepare-buffer proc)
        (with-current-buffer (process-buffer proc)
          (setq default-directory (if (string-ext/end-p root "/")
                                      root (concat root "/")))
          (if (opt-val :mode options)
              (funcall (opt-val :mode options))
            (rails/runner/output-mode))
          (when (opt-val :keywords options)
            (rails/runner/setup-font-lock (opt-val :keywords options)))
          (set-process-sentinel proc 'rails/runner/sentinel-proc)
          (set (make-local-variable 'font-lock-unfontify-region-function)
               'ansi-color-unfontify-region) ; fixed conflict ansi-color and compilation-mode
          (setq rails/runner/script-name (format "%s %s" command parameters))
          (message "Starting %s." rails/runner/script-name))))))

(defun ansi-color-insertion-filter (proc string)
  (with-current-buffer (process-buffer proc)
    (let ((moving (= (point) (process-mark proc)))
          (buffer-read-only nil))
      (save-excursion
        ;; Insert the text, advancing the process marker.
        (goto-char (process-mark proc))
        ;; decode ansi color sequences
        (insert (ansi-color-apply string))
        (set-marker (process-mark proc) (point)))
      (if moving (goto-char (process-mark proc))))))

(provide 'rails-runner)
