;;; damlite.el --- a simple DAML mode for Xemacs

;;; (C) 2001 BBN Technologies
;;; by Mark Burstein, with assistance from Ken Anderson and Richard Shapiro


;;; This package is designed to be used with XEmacs and Gnu Emacs

;;; XEmacs is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.

;;; XEmacs is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with XEmacs; see the file COPYING.  If not, write to the Free
;;; Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;; Code:

(require 'cl)
(require 'easymenu)

;;; which emacs are we in?
(defvar in-xemacs-p (string-match "XEmacs\\|Lucid" (emacs-version)))


(defvar daml-mode-syntax-table
  (let ((s (copy-syntax-table text-mode-syntax-table)))
;;; from html mode
    (modify-syntax-entry ?< "(>" s)
    (modify-syntax-entry ?> ")<" s)
    (modify-syntax-entry ?\" ".   " s)
    (modify-syntax-entry ?\\ ".   " s)
    (modify-syntax-entry ?'  "w   " s)
  ;;; change : and _ to be part of words for RDF
    (modify-syntax-entry ?:  "w   " s)
    (modify-syntax-entry ?_  "w   " s)
    s))


;;; define some shortcuts (future use)
(defvar daml-mode-abbrev-table nil
  "Abbrev table in use in sgml-mode.")

(define-abbrev-table 'daml-mode-abbrev-table ())

;;; The DAML command key table
(defvar daml-mode-map nil "Keymap for DAML mode")
(unless daml-mode-map
  (setq daml-mode-map (make-sparse-keymap)))

(defgroup daml nil
  "DAML Markup Language")

;;; ================ user variables ================

(defcustom daml-helper-build-new-buffer t
  "*If not nil, then insert `daml-helper-new-buffer-strings' for new buffers."
  :type 'boolean
  :group 'daml)

(defcustom daml-mode-hook '(daml-parse-prolog)
  "A hook or list of hooks to be run when entering DAML mode."
  :type 'hook
  :group 'daml)

(defcustom daml-indent-step 2
  "Number of spaces to indent succeeding layers of tag forms."
  :type 'integer
  :group 'daml)

;;; ================================================================
;;; The next two strings define what is inserted into a new DAML buffer
;;; when creating it from scratch. It consists of a default XML prolog
;;; and a default RDF Tag body, including XMLNS definitions that should 
;;; be most commonly used.

(defcustom daml-helper-xml-version 
  (concat
    "<?xml version='1.0' encoding='ISO-8859-1'?>\n<!DOCTYPE uridef[\n"
"  <!ENTITY rdf \"http://www.w3.org/1999/02/22-rdf-syntax-ns\">\n"
"  <!ENTITY rdfs \"http://www.w3.org/2000/01/rdf-schema\">\n"
"  <!ENTITY daml \"http://www.daml.org/2001/03/daml+oil.daml\">\n"
"  <!ENTITY xmls \"http://www.w3.org/2000/10/XMLSchema\">\n]>\n\n"

   )
  "first lines of file if specifying xml version and doctype" 
  :type 'string
  :group 'daml)

(defun daml-helper-created-string ()
  (format "<!-- Created: %s -->\n" (current-time-string)))


(defcustom daml-helper-new-buffer-template
    '(daml-helper-xml-version
      "<rdf:RDF\n"
      "  xmlns:rdf = \"&rdf;#\"\n"
      "  xmlns:rdfs =\"&rdfs;#\"\n"
      "  xmlns:daml =\"&daml;#\"\n"
      "  xmlns:xmls =\"&xmls;#\"\n"
      "  xmlns     =\"\"\n"
      " >\n\n"

      "<daml:Ontology rdf:about=\"\">\n"
      " <daml:versionInfo> </daml:versionInfo>\n"
      " <rdfs:comment> \n"
      " </rdfs:comment>\n"
      " <daml:imports rdf:resource=\"&daml;\"/>\n"
      "</daml:Ontology>\n\n"
      (daml-helper-created-string) ;; put date/time in the file (future)
      "\n</rdf:RDF>\n"
      )


  "*Template for new DAML buffers.
Inserted by `daml-helper-insert-new-buffer-strings' if
`daml-helper-build-new-buffer' is set to t"
  :type 'sexp
  :group 'daml)

;;; ================ REQUIRED FILES ================

(load "daml-xcompat") ; things needed to make GNU happy
(load "daml-menu") ;; defines menu of commands 
(load "daml-fontify") ;; font lock definitions
(load "daml-insert")  ;; insert new definition commands
(load "daml-motion") ;; defines commands for moving about among tags TBD
(load "daml-w3") ;; command for finding definitions (meta-dot)
(load "daml-summary") ;; summary mode window

;;; ================================================

(defun daml-mode ()
  "Major mode for editing DAML documents.
DAML, or DAML+OIL as it is formally known, is an XML-based knowledge
representation language for describing semantic content. See
http://www.daml.org for more information. 

This current mode is NOT based on PSGML mode, which uses an SGML parser.
Instead, we use local heuristics for navigation and lookup that 
enable its use with 'mostly syntactically correct' files that 
occur while the ontologies are under development. 

Comments to burstein@bbn.com
Key bindings:
 \\{daml-mode-map}

Note also that M-C-f and M-C-b move forward, backward over balanced
< > pairs as though they were parenthasized expressions. 
"
  (interactive)
  (kill-all-local-variables)
  (setq local-abbrev-table daml-mode-abbrev-table)
  (use-local-map daml-mode-map)
  (setq mode-name "DAML")
  (setq major-mode 'daml-mode)
  (make-local-variable 'daml-default-doctype-name)
  (setq 
	daml-default-doctype-name    "daml"
	daml-always-quote-attributes t 
	daml-xml-p                   t
	)

  ;; font-lock setup for various emacsen: XEmacs, Emacs 19.29+, Emacs <19.29.
  ;; By Ulrik Dickow <dickow@nbi.dk>.  (Last update: 05-Sep-1995).
  (cond	(in-xemacs-p
	 (put major-mode 'font-lock-keywords-case-fold-search nil))
	((string-lessp "19.28.89" emacs-version) ; Emacs 19.29 and later
	 (make-local-variable 'font-lock-defaults)
	 (setq font-lock-defaults '(daml-font-lock-keywords nil nil)))

	;; this probably isnt working anyway:
	(t ; Emacs 19.28 and older
	 (make-local-variable 'font-lock-keywords-case-fold-search)
	 (make-local-variable 'font-lock-keywords)
	 (make-local-variable 'font-lock-no-comments)
	 (setq font-lock-keywords-case-fold-search nil) ; was t
	 (setq font-lock-keywords daml-font-lock-keywords)
	 (setq font-lock-no-comments t)))
  
  (set-syntax-table daml-mode-syntax-table)
  (easy-menu-add daml-menu) ; in xemacs/lisp/easymenu.el
  (easy-menu-add daml-insert-menu) ; in xemacs/lisp/easymenu.el
  (let ((newbuf (zerop (buffer-size))))
    (if (and daml-helper-build-new-buffer newbuf)
	(daml-helper-insert-new-buffer-strings))

  ;; Set up the syntax table. -- using xml-syntax so should be ok. 

    (run-hooks 'text-mode-hook 'daml-mode-hook)

    (font-lock-mode 1) ;; turn on font-lock mode to see the pretty colors!
;; this doesn't work for some reason
;    (when newbuf
;      (end-of-buffer)
;      (search-backward "<!-- Created" nil t)
;      (dbg "after created")
;      (next-line 1))
    ))


(put 'daml-mode 'font-lock-defaults '(daml-font-lock-keywords nil nil))


;;; This function replaces the only piece of  tempo.el we still are using:
;;; the template code for this simple case
(defun daml-insert-strings (template &optional sublist)
    (cond ((stringp template) (insert template))
	  ((symbolp template)
	   (when (symbol-value template)
	     (daml-insert-strings (symbol-value template))))
	  ((and (consp template) sublist)
	   (daml-insert-strings (eval template)))
	  ((consp template)
	   (dolist (elt template)
	     (daml-insert-strings elt t)))))


(defun daml-helper-insert-new-buffer-strings ()
  "Insert `daml-helper-new-buffer-template'."
;  (tempo-template-daml-skeleton)
  (daml-insert-strings daml-helper-new-buffer-template)
  )

;;;###autoload
(autoload 'daml-mode "damlite" "DAML mode." t)

(push (cons "\\.daml" 'daml-mode) auto-mode-alist)


