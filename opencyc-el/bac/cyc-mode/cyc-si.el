(setq si-initialized nil)

(defun si-init () ""
  (setq cyc-host "localhost")
  (setq cyc-service "3601")
  (setq si-cyc-process (inferior-lisp "telnet localhost 3601"))
  (setq si-queue (tq-create (inferior-lisp-proc)))
  (setq si-initialized t))

(defun si-query (query) ""
  (if (not si-initialized)
      (si-init))
  (setq si-output-string nil)
  (setq it nil)
  (tq-enqueue si-queue query "\r\n" nil 'si-answer-filter)
  (while (string= si-output-string nil)
    (setq it (concat it "."))
    (display-message-or-buffer it)
    (sit-for .1 0 t))
  (string-match "[0-9]+ \\(.*\\)\r\n" si-output-string)
  (display-message-or-buffer (setq si-output-string  (match-string 1 si-output-string))))

(defun si-answer-filter (closure answer)
  (setq si-output-string answer))

(defun si-close ()
  (interactive)
  (tq-close si-queue)
  (setq si-initialized nil))


;; Functions for converting between strange and non-uniform
;; list types.

(defun si-list-to-alist (lst)
  (let ((alst))
    (while lst
      (setq alst (cons (list (car lst)) alst))
      (setq lst (cdr lst)))
    alst))

(defun si-do-list (inlst command)
  (let ((outlst))
    (while inlst
      (setq outlst (eval (read command)))
      (setq inlst (cdr inlst)))
    outlst))

(defun si-caar-alist-to-list (alst)
  (let ((lst))
    (while alst
      (setq lst (cons (list (caar alst)) lst))
      (setq alst (cdr alst)))
    lst))

(defun si-hash-table-to-alist (table)
  "Return an alist holding all key/value pairs of hash table TABLE."
  (let ((list))
    (maphash #'(lambda (key value)
		 (setq list (cons (cons key value) list)))
	     table)
    list))

(defun si-alist-to-hash-table (alst)
  (setq hash (make-hash-table :test #'equal))
  (while alst
    (puthash (caar alst) (cadar alst) hash)
    (setq alst (cdr alst)))
  hash)


;; Functions for starting and stopping the SI

(global-set-key "\M-s" 'si-cyc-api)

(defun si-stop-cyc-api ()
  (interactive)
  (clrhash si-cyc-api-function-args)
  (setq si-cyc-api-functions nil))

(defun si-start-cyc-api ()
  (interactive)
  (setq si-cyc-api-function-args 
	(si-alist-to-hash-table si-args))
  (setq si-cyc-api-functions nil)
  (setq si-cyc-api-functions
	(si-caar-alist-to-list si-args))
)

(defun si-cyc-api ()
  (interactive)
  (let* ((api-func (completing-read "Cyc API function: " si-cyc-api-functions))
	 (api-args (gethash api-func si-cyc-api-function-args))
	 (optional nil))
    (setq query (concat "(" api-func ))
    (while (and api-args (not optional))
      (setq token (car api-args))
      (if (string= token "&optional")
	  (setq optional t)
	(while (not (setq token-value (si-token-get token)))
	  nil)
	(setq query (concat query " " token-value))
	(setq api-args (cdr api-args))))
    (setq query (concat query ")\n"))
    (comint-send-string (inferior-lisp-proc) query)
    (display-message-or-buffer query)
    ))

(defun si-token-get (token)
  "Get a syntax element.
"
  (if (string-match "^<" token)
      (read-string (concat token " : "))
    (or
     (if (or
	  (equal token "fort-p")
	  (equal token "el-fort-p")
	  (equal token "fort-p")
	  (equal token "fort-p")
	  (equal token "fort-p")
	  (equal token "fort-p")
	  (equal token "fort-p")
	  (equal token "fort-p")
	  (equal token "fort-p")
	  )
	 (progn
	   (setq possibility (cm-constant-at-point))
	   (if (string= "T"
			(si-query (concat "(" token " " possibility ")\n")))
	       possibility
	     (read-string (concat token " : ")))))
     )
    (read-string (concat token " : "))
    ))
