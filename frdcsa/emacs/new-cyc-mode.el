(global-set-key "\C-ccc" 'cm-get-comment-for-constant)
   
(defun cm-get-comment-for-constant ()
 "Return the comment for a constant"
 (interactive)
 (let*
  ((constant (thing-at-point 'symbol)))
  (message constant)
  ;; (uea-send-contents (concat "OpenCyc, (comment " constant ")"))
  ))
