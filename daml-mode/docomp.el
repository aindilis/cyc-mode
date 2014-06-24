;;; compile/load all .el files in this directory.
(setq in-xemacs-p (string-match "XEmacs\\|Lucid" (emacs-version)))

(require 'w3)

(defun compile-it ()
  (let ((files (directory-files "." t ".*.[eE][lL]$" nil)))
    (while files
      (if (and (not 
		(file-directory-p (car files)))
		(not (string-match "docomp.el" (car files))))
	  (byte-compile-file (car files)))
      (setq files (cdr files)))))

(compile-it)
