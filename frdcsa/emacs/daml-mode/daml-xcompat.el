;;; COMPAT for DAML mode: functions used by DAML mode not found in GNU emacs
(unless (string-match "XEmacs\\|Lucid" (emacs-version))

(defun mapvector (fn list)
  (coerce (mapcar '(lambda (elt) (funcall fn elt))
		  list) 'vector))

(defmacro char= (a b) `(and (numberp ,a)(numberp ,b)(= ,a ,b)))
(defmacro characterp (c) `(and (numberp ,c)(< ,c 256)))

) ;; end unless




