;;; ---------------------------------------------------------
;;; - Variables
;;;

(defconst rails/webserver-bundle/buffer-name "*RServer*")
(defconst rails/webserver-bundle/types '("mongrel" "webrick" "lighttpd" "thin"))

(defcustom rails/webserver-bundle/default-type "mongrel"
  "Webserver default server type."
  :type 'string
  :group 'rails)

(defcustom rails/webserver-bundle/port 3000
  "Webserver defaut port."
  :type 'integer
  :group 'rails)

(defcustom rails/webserver-bundle/addr "127.0.0.1"
  "Webserver defaut bind address."
  :type 'string
  :group 'rails)

(defvar rails/webserver-bundle/process-type nil)
(defvar rails/webserver-bundle/process-env nil)
(defvar rails/webserver-bundle/process-port nil)
(defvar rails/webserver-bundle/process-addr nil)

;;; ---------------------------------------------------------
;;; - Functions
;;;

(defun rails/webserver-bundle/started-p ()
  (get-buffer-process (get-buffer rails/webserver-bundle/buffer-name)))

(defun rails/webserver-bundle/type-p (type)
  (when (stringp type)
    (string= rails/webserver-bundle/default-type type)))

(defun rails/webserver-bundle/set-default-type (type)
  (setq rails/webserver-bundle/default-type type))

(defun rails/webserver-bundle/make-server-command (type port env addr)
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

(defun rails/webserver-bundle/sentinel-proc (proc msg)
  (let ((env rails/webserver-bundle/process-env)
        (port rails/webserver-bundle/process-port)
        (type rails/webserver-bundle/process-type)
        (addr rails/webserver-bundle/process-addr))
    (when (memq (process-status proc) '(exit signal))
      (rails/proxy/down-tunnel-if-need port)
      (setq rails/webserver-bundle/process-env nil)
      (setq rails/webserver-bundle/process-port nil)
      (setq rails/webserver-bundle/process-type nil)
      (setq rails/webserver-bundle/process-addr nil)
      (setq msg (format "stopped (%s)" msg)))
    (rails/notify
     (replace-regexp-in-string
      "\n" ""
      (format "%s (%s) %s:%s %s"
              (capitalize type)
              env
              addr
              port
              msg))
     :notice)))

(defun rails/webserver-bundle/start (root type env port addr)
  (let ((proc (get-buffer-process rails/webserver-bundle/buffer-name)))
    (if proc
        (rails/notify "Only one instance of rails/webserver allowed.")
      (in-directory root
                    (let* ((cmd-alist (rails/webserver-bundle/make-server-command type port env addr))
                           (proc (rails/proxy/shell-command root
                                                            (car cmd-alist)
                                                            rails/webserver-bundle/buffer-name
                                                            (car cmd-alist)
                                                            (cdr cmd-alist))))
                      (when (processp proc)
                        (rails/runner/prepare-buffer proc)
                        (rails/proxy/up-tinnel-if-need root port)
                        (set-process-sentinel proc 'rails/webserver-bundle/sentinel-proc)
                        (setq rails/webserver-bundle/process-env env)
                        (setq rails/webserver-bundle/process-type type)
                        (setq rails/webserver-bundle/process-port port)
                        (setq rails/webserver-bundle/process-addr addr)
                        (rails/notify (format "%s (%s) starting at %s:%s"
                                              (capitalize type)
                                              env
                                              addr
                                              port)
                                      :notice)))))))

(defun rails/webserver-bundle/stop (root)
  "Stop the WebServer process."
  (let ((proc (get-buffer-process rails/webserver-bundle/buffer-name)))
    (when proc (kill-process proc t))))

;;; ---------------------------------------------------------
;;; - Interactives
;;;

(defun rails/webserver-bundle/toggle ()
  (interactive)
  (when-bind (root (rails/root))
    (if (rails/webserver-bundle/started-p)
        (rails/webserver-bundle/stop root)
      (rails/webserver-bundle/start root
                                    rails/webserver-bundle/default-type
                                    rails/default-environment
                                    rails/webserver-bundle/port
                                    rails/webserver-bundle/addr))))

(defun rails/webserver-bundle/run-with-env (&optional env port)
  (interactive)
  (unless (rails/webserver-bundle/started-p)
    (when-bind (root (rails/root))
      (unless env
        (setq env (rails/completing-read "Run with environment"
                                         (rails/environments root)
                                         t
                                         rails/default-environment)))
      (unless port
        (setq port (rails/completing-read "and port"
                                          nil
                                          nil
                                          (format "%s" rails/webserver-bundle/port)))
        (when (zerop (string-to-number port))
          (setq port rails/webserver-bundle/port)))
      (rails/webserver-bundle/start root
                                    rails/webserver-bundle/default-type
                                    env
                                    port
                                    rails/webserver-bundle/addr))))

;;; ---------------------------------------------------------
;;; - Bundle
;;;

(rails/defbundle "Webserver"
  (:keys
   (("w s" 'rails/webserver-bundle/toggle)
    ("w S" 'rails/webserver-bundle/run-with-env)))

  ;;; ---------------------------------------------------------
  ;;; - Menu
  ;;;

  (let ((map (make-sparse-keymap)))
    (dolist (type rails/webserver-bundle/types)
      (define-key map
        (vector (string-ext/safe-symbol type))
        (list 'menu-item
              (capitalize type)
              `(lambda () (interactive) (rails/webserver-bundle/set-default-type ,type))
              :button (cons :radio `(rails/webserver-bundle/type-p ,type)))))
    (define-keys map
      ([separator]  (cons "--" "--"))
      ([run]        (list 'menu-item
                          "Run with Environment and Port"
                          'rails/webserver-bundle/run-with-env
                          :enable '(not (rails/webserver-bundle/started-p))))
      ([toggle]     (cons "Toggle Start/Stop" 'rails/webserver-bundle/toggle)))
    (rails/bundles/add-to-bundles-menu "WebServer" map)))

