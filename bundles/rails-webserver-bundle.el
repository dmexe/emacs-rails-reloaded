(require 'cl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Constants
;;

(defconst rails/webserver/buffer-name "*RServer*")
(defconst rails/webserver/types '("mongrel" "webrick" "lighttpd" "thin"))

(defcustom rails/webserver/default-type "mongrel"
  "Webserver default server type."
  :type 'string
  :group 'rails)

(defcustom rails/webserver/port 3000
  "Webserver defaut port."
  :type 'integer
  :group 'rails)

(defcustom rails/webserver/addr "127.0.0.1"
  "Webserver defaut bind address."
  :type 'string
  :group 'rails)

(defvar rails/webserver/process-type nil)
(defvar rails/webserver/process-env nil)
(defvar rails/webserver/process-port nil)
(defvar rails/webserver/process-addr nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Functions
;;

(defun rails/webserver/started-p ()
  (get-buffer-process (get-buffer rails/webserver/buffer-name)))

(defun rails/webserver/type-p (type)
  (when (stringp type)
    (string= rails/webserver/default-type type)))

(defun rails/webserver/set-default-type (type)
  (setq rails/webserver/default-type type))

(defun rails/webserver/make-server-command (type port env addr)
  (cond
   ((string= "thin" type)
    (cons type
          (format "-p %s -e %s start"
                  port
                  env)))
   (t
    (cons rails/ruby/command
          (format "script/server %s -b %s -p %s -e %s"
                  type
                  addr
                  port
                  env)))))

(defun rails/webserver/sentinel-proc (proc msg)
  (let ((env rails/webserver/process-env)
        (port rails/webserver/process-port)
        (type rails/webserver/process-type)
        (addr rails/webserver/process-addr))
    (when (memq (process-status proc) '(exit signal))
      (setq rails/webserver/process-env nil)
      (setq rails/webserver/process-port nil)
      (setq rails/webserver/process-type nil)
      (setq rails/webserver/process-addr nil)
      (setq msg (format "stopped (%s)" msg)))
    (rails/notify
     (replace-regexp-in-string
      "\n" ""
      (format "%s (%s %s) - %s"
              (capitalize type)
              env
              port
              msg))
     :notice)))

(defun rails/webserver/start (root type env port addr)
  (let ((proc (get-buffer-process rails/webserver/buffer-name)))
    (if proc
        (rails/notify "Only one instance of rails/webserver allowed.")
      (in-directory root
        (let* ((cmd-alist (rails/webserver/make-server-command type port env addr))
               (proc (rails/proxy/shell-command root
                                                (car cmd-alist)
                                                rails/webserver/buffer-name
                                                (car cmd-alist)
                                                (cdr cmd-alist))))
          (when (processp proc)
            (set-process-sentinel proc 'rails/webserver/sentinel-proc)
            (setq rails/webserver/process-env env)
            (setq rails/webserver/process-type type)
            (setq rails/webserver/process-port port)
            (rails/notify (format "%s (%s) starting with %s:%s"
                                  (capitalize type)
                                  env
                                  addr
                                  port)
                          :notice)))))))

(defun rails/webserver/stop (root)
  "Stop the WebServer process."
  (let ((proc (get-buffer-process rails/webserver/buffer-name)))
    (when proc (kill-process proc t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Callbacks
;;

(defun rails/webserver/load ()
  (rails/define-key "w s" 'rails/webserver/toggle)
  (rails/define-key "w S" 'rails/webserver/run-with-env)
  (let ((map (make-sparse-keymap)))
    (dolist (type rails/webserver/types)
      (define-key map
        (vector (string-ext/safe-symbol type))
        (list 'menu-item
              (capitalize type)
              `(lambda () (interactive) (rails/webserver/set-default-type ,type))
              :button (cons :radio `(rails/webserver/type-p ,type)))))
    (define-keys map
      ([separator]  (cons "--" "--"))
      ([run]        (list 'menu-item
                          "Run with Environment and Port"
                          'rails/webserver/run-with-env
                          :enable '(not (rails/webserver/started-p))))
      ([toggle]     (cons "Toggle Start/Stop" 'rails/webserver/toggle)))
    (rails/add-to-bundles-menu "WebServer" map)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Interactives
;;

(defun rails/webserver/toggle ()
  (interactive)
  (when-bind (root (rails/root))
    (if (rails/webserver/started-p)
        (rails/webserver/stop root)
      (rails/webserver/start root
                             rails/webserver/default-type
                             rails/default-environment
                             rails/webserver/port
                             rails/webserver/addr))))

(defun rails/webserver/run-with-env (&optional env port)
  (interactive)
  (unless (rails/webserver/started-p)
    (when-bind (root (rails/root))
      (unless env
        (setq env (rails/completing-read "Run with environment"
                                         (rails/environments root)
                                         t
                                         rails/default-environment)))
      (unless port
        (setq port (rails/completing-read "and port"
                                          nil
                                          t
                                          (format "%s" rails/webserver/port)))
        (when (zerop (string-to-number port))
          (setq port rails/webserver/port)))
      (rails/webserver/start root rails/webserver/default-type env port rails/webserver/addr))))


(provide 'rails-webserver-bundle)