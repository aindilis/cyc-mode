;;; daml-w3.el --- a part of the simple DAML mode for Xemacs

;;; (C) 2001 BBN Technologies
;;; by Mark Burstein, with assistance from Ken Anderson and Richard Shapiro

;;; This package is designed to be used with XEmacs and Gnu Emacs



(defvar daml-w3-loadedp nil)

(cond ((locate-library "w3")
       (require 'w3)
       (setq daml-w3-loadedp t)))

;;; adaptation/overlay on w3 package for DAMLITE mode

(defvar *daml-url-buffer-map* nil 
  "assoc list of buffer names and the URLs that were loaded into them")

(defvar *daml-in-buffer-quietly* t 
  "If finding id in loaded buffer, don't ask.") 

;;; cons of buffer name and (point)
(defvar *daml-last-position* nil 
  "Location prior to meta dot - use <c-x p> to go back to it.")

(defun buffer-deleted-p (buffer)
  (setq buffer (get-buffer buffer)) ;; just to be sure
  (not (and buffer (buffer-live-p buffer))))

;;; add a new url buffer to the active list
(defun daml-url-new-buffer (url buf) 
  ;; remove any deleted buffers
  (setf *daml-url-buffer-map*
    (remove-if (function (lambda (pr) (buffer-deleted-p (cdr pr))))
	       *daml-url-buffer-map*))
  (let ((buffer (get-buffer buf)))
    (when buffer
      (push (cons url buffer) *daml-url-buffer-map*))))

;;; see if the url (up through file name) is in a buffer
(defun daml-url-already-loaded (url)
  (when url
    (let* ((cell (assoc url *daml-url-buffer-map*))
	   (buf (and cell (cdr cell))))
      ;; if buffer was deleted (or renamed), remove the pointer
      (cond ((and cell (or (not buf) (not (buffer-live-p buf))))
	     (setf *daml-url-buffer-map* (delete as *daml-url-buffer-map*))
	     nil)
	    (t buf)))))

;;; *daml-url-buffer-map is (url . buffer)*
;;; get the url of a buffer (reverse assoc)
(defun daml-buffer-url (bufname)
  (let ((buf (get-buffer bufname)))
    (dolist (cell *daml-url-buffer-map*)
      (when (equal buf (cdr cell)) 
	(return (car cell))))))

;;; This needs to be buffer specific,
;;; so we make it a double alist (buffername . (ns . expansion)*)*
;;; (the right way may be to use MAKE-VARIABLE-BUFFER-LOCAL, 
;;; which enables different values by buffer)

;;; namespace associations from <rdf:RDF xmlns:foo="expansion">
(defvar *daml-xmlns-map* nil
  "Assoc list RDF xmlns names to their expansions.")

(make-variable-buffer-local '*daml-xmlns-map*)
(set-default '*daml-xmlns-map* nil)

;;; substitutions gleaned from <ENTITY name "expansion"> within DOCTYPE tag at beginning of file
(defvar *daml-entity-map* nil 
  "Assoc list of xml entity names to their expansions")

(make-variable-buffer-local '*daml-entity-map*)
(set-default '*daml-entity-map* nil)

(defun daml-entity-expansion (name)
  (let ((cell (assoc name *daml-entity-map*)))
     (when cell (cdr cell))))
    
;;; Given an XML namespace reference (NAME), return the expansion according to an
;;; XMLNS=line in the RDF tag, previously cached in *DAML-XMLNS-MAP*
;;; Do &ENTITY; expansion before returning the result.
;;;  For now, also strip off the ending # if there is one.

(defun daml-rdfns-expansion (name &optional no-remove-lb)
  (let* ((cell (assoc name *daml-xmlns-map*))
	 (url (and cell (cdr cell)))
	 numpos)
    (setf uri (daml-expand-entname url))
    ;;; remove trailing #, if there
    (when (and (not no-remove-lb) (setf numpos (position ?# uri)))
      (setf uri (substring uri 0 numpos)))
    uri))

;;; read the DOCTYPE tag for ENTITY names and substitution strings
;;; store mappings in *DAML-ENTITY-MAP* for this buffer

(defun daml-extract-entity-map ()
  (interactive)
  (let ((entmap nil))
    (beginning-of-buffer)
    (when (re-search-forward "<!DOCTYPE[ \t]*\\w*\\[" nil t)
      (let ((tagbe (match-end 0));; end of the match of doctype
	    (tagc nil))
	(goto-char tagbe)
	(setf tagc (re-search-forward "\]>" nil t))
	(goto-char tagbe)
  ;;; loop down from end of begin TAG (after '[') until end at ]> 
  ;;; matching <!ENTITY {1=name} {2="substitute-string"}  > 
  ;;; and build up (name .  subst-str) pairs
	    ;;args are: (re-search-forward regexp &optional bound no-error count buffer)
	(while (re-search-forward "<!ENTITY[ \t]+\\(\\w+\\)[ \t\n]+\"\\([^\"]*\\)\"" tagc t)

	  (let* ((mb1 (match-beginning 1))
		 (me1 (match-end 1))
		 (mb2 (match-beginning 2))
		 (me2 (match-end 2))
		 (ename (and mb1 me1 (buffer-substring-no-properties mb1 me1)))
		 (subst-str (and mb2 me2 (buffer-substring-no-properties mb2 me2))))
	    (goto-char me2)
	    (when (and ename subst-str)
	      (push (cons ename subst-str) entmap))))))
    entmap))



;;; buf is a string (buffer name)
;;; build assoc list of XML ENTITY substitutions
(defun daml-build-entity-map (&optional forcep)
  (interactive)
  (when (or forcep (null *daml-entity-map*))
    (let* ((emap (daml-extract-entity-map))
	   (this (assoc "THIS" emap)))
      (setq *daml-entity-map* emap)
      (if (and this (null (daml-buffer-url (current-buffer))))
	  (daml-url-new-buffer (cdr this) buf))
      )))

;;; given a string, expand using ENTITY substitutions
(defun daml-expand-entname (tok-string)
  (let* ((tok tok-string)
	 p1)
    (while (setq p1 (position ?& tok))
      (let* ((p2 (and p1 (position ?\; tok :start p1)))
	     (subst-name (and p2 (substring tok (1+ p1) p2)))
	     (replacement (daml-entity-expansion subst-name)))
        (if replacement 
	    (setf tok
		  (concat (substring tok 0 p1) 
			  replacement
			  (substring tok (1+ p2))))
	  (error "Could not find an expansion of %s in token %s at position %d" 
		 repacement tok-string (point))
	  )))
    tok))


;;; arg is all tabs or spaces
(defun all-whitespace (str)
  (loop for  i from 0 to (1- (length str))
    when (not (position (aref str i) "\ \t"))
    do (return nil)
    finally (return t)))

(defun daml-extract-rdfns-map ()
  (interactive)
  (let ((entmap nil))
    (beginning-of-buffer)
    (when (re-search-forward "<rdf:RDF" nil t)
      (let ((tagbe (match-end 0));; end of the match of doctype
	    (tagc nil))
	(goto-char tagbe)
	(setf tagc (re-search-forward ">" nil t))
	(goto-char tagbe)
;;; matching xmlns:[|{1=name}] = {2="substitute-string"}   
;;; and build up (name .  subst-str) pairs
;;;   (scratch-msg "rdf tag between %s and %s" tagbe tagc)
	(while (re-search-forward 
		"xmlns\\([ \t]*\\|:\\w+\\)[ \t]*=[ \t]*\"\\([^\"]*\\)\""
		tagc t)
	  (let* ((mb1 (match-beginning 1))
		 (me1 (match-end 1))
		 (mb2 (match-beginning 2))
		 (me2 (match-end 2))
		 (ename (and mb1 me1 (buffer-substring-no-properties mb1 me1)))
		 (subst-str (and mb2 me2 (buffer-substring-no-properties mb2 me2))))
	    (goto-char me2)
	    (when (and ename subst-str)
	      (cond ((all-whitespace ename) (setq ename nil))
		    ((eq ?: (aref ename 0))
		     (setq ename (substring ename 1))))
	      (push (cons ename subst-str) entmap))))))
  entmap))

;;; read the RDF tag for XMLNS names and substitution strings
;;; store mappings in *DAML-XMLNS-MAP* for this buffer
;;; buf is a string (buffer name) 
(defun daml-build-rdfns-map (&optional forcep)
  (interactive)
  (when (or forcep (null *daml-xmlns-map*))
    (let* ((emap (daml-extract-rdfns-map))
	   (this (or (assoc "THIS" emap)
		     (assoc "this" emap)))
	   (buf (current-buffer)))
      (setq *daml-xmlns-map* emap)
      (if (and this (not (daml-buffer-url buf)))
	  (daml-url-new-buffer (cdr this) buf))
      emap
      )))

;;; read ENTITY and RDF substitution definitions from prolog to file
;;; Should be called when file is loaded, or at least the first time needed... 
;;; need to work out when else to do it after that (using forcep)

(defun daml-parse-prolog ()
  (daml-build-entity-map t)
  (daml-build-rdfns-map t))

;;; We need to break up and expand xmlns refs like daml:class based on the rdf xmlns attributes
;;; and references to URIs in resource attribute values that use XML ENTITY shorthands.
;;; RDF tokens can either be full URI's or shortened using XMLNS abbreviations
;;; we deal with the URI's that start with HTTP: only at this point. 

(defstruct (daml-name (:type list)) 
  target
  namespace
  uri
  type
  source)

;;; Break up a token and return a DAML-NAME tuple
;;; where namespace is the prefix before : (except http:)
;;; where uri-base is the URI PATH up to filename
;;; where filename is the local filename (e.g. "foo.daml")
;;; and where name is what follows the # in an expanded URI
;;; if an expanded URI (starts with http) then namespace is nil

(defun daml-parse-rdf-token (tok-string  &optional type no-subst buf)
  (unless no-subst
    (setf tok-string (daml-expand-entname tok-string)))
;  (dbg "Expanded tok: %s type %S" tok-string type)
  (let* ((pos (position ?: tok-string))
	 (ns (substring tok-string 0 pos)) ;; string before :
	 (sharp-pos (position ?# tok-string :from-end t))
	 (target (if sharp-pos (substring tok-string (1+ sharp-pos) nil)))
	 uri
	 )
    ;; if, after ent expansion, it starts with http, then get uri
    (cond ((string-equal (upcase ns) "HTTP")
	   (cond (target
		  (setq uri (substring tok-string 0 sharp-pos)))
		 (t (setf uri tok-string target nil))))
	  ;; if begin with sharp, a ref within the file
	  ((and sharp-pos (zerop sharp-pos)) ;; just a #name
	   (setf target (substring tok-string 1 nil)
		 uri nil))
	   ;; else an xmlns name prefix - these substitutions MAY include the #, so 
	   ;; we strip it off (until I know what is legal)
	   ;; if has a colon, and a kwd, then do xmlns expansion
	  (pos ;; a non HTTP with colon
	   (setq uri (daml-rdfns-expansion ns)) ;; expand the prefix
	   (setq target (substring tok-string (1+ pos) nil)) ;; name after :
	   )
	  ;;; NOTE!! EQ is case sensitive on symbols here!!
	  ((member type '(ATTNAME ENTNAME)) ; NO prefix, just a name in the default namespace
	   (setf target tok-string uri (daml-rdfns-expansion nil)) ; the default xmlns
	   )
	  (t (setf target tok-string uri nil))) ;; a name in current buffer?
    (when (or target uri)
      (make-daml-name :target target :uri uri :namespace ns :type type :source tok-string))))


;;; from url.el
;;;(eval-and-compile
;;;  (defvar url-get-url-filename-chars "-%.?@a-zA-Z0-9()_/:~=&"
;;;    "Valid characters in a URL")
;;;  )

;;; use this instead. 
(eval-and-compile
  (defvar daml-get-token-chars "-.a-zA-Z0-9_/:~&#;" ;; took out ()=%?@ 
    "Valid characters in a URL")
  )

;;; Note, this includes dbl-quote in the list of things to ignore when searching back
(defvar daml-space-chars "[ \t\n\]")

(defvar daml-nontok-chars "[] \t\n\"]")

(defmacro daml-backup-over-whitespace ()
    `(skip-chars-backward daml-space-chars))

(defun next-char-is (pat)
  (if (characterp pat) 
      (if (not (eobp))(char= pat (char-after)))
    (looking-at pat)))

(defun prev-char-is (pat)  
  (when (not (bobp))
    (if (characterp pat) (char= pat (char-before))
      ;; else a regexp for a char
      (string-match pat (char-to-string (char-before))))))

(defmacro between-chars (pat1 pat2)
  `(and (prev-char-is ,pat1) (next-char-is ,pat2)))

;;; look back before point matching each char to next elt of match-elts
;;; if (second match-elts) is '* then zero or more of (first match-elts)
(defun daml-look-before (pt match-elts)
  (let ((old-pt (point))
	(ok t))
    (goto-char pt)
    (while (and match-elts ok)
      (let ((pat (car match-elts))
	    (star (and (cdr match-elts) (eq (second match-elts) '*)))
	    match)
	(backward-char 1)
	(cond  (star
		(while (next-char-is pat)
		  (backward-char 1))
		(forward-char 1)
		(setf match-elts (cddr match-elts))
		) ;; set up next loop
	       ((next-char-is pat) 
		(pop match-elts))
	       (t (setq ok nil)))))
    (goto-char old-pt)
    ok))
	
;;; non-space char preceding matches pat
(defmacro daml-char-before (pt pat)  
  `(daml-look-before ,pt '("[ \t\n]" * ,pat)))


;;; find a token or a string that might be a resource reference.
;;; look backward if over spaces, forward if on < or =
;;; don't ask. based partly on url-get-url-at-point (url.el)

;;; NOTE: WE NOW DISTINGUISH THE CASE OF AN ENTITY NAME AND A REFERENCE IN QUOTES.
;;; Entity names can use XMLNS prefixes followed by :, and references can't.
;;; In references, "#NAME" means name relative to current file. 
;;; ALSO NOTE: RDF:ID="NAME" is the same as RDF:ABOUT="#NAME", though I'm still unsure why. 
;;; There seems to be no way to do an unqualified <tagname ...> 
;;; unless the default XMLNS is declared. (per discussion with MDEAN 10/24/01)

;;; We set toktype to ENTNAME(after <) or REFERENCE (in quotes after =) or ATTNAME (before =)
;;; returns (cons <found token string> <parsed form as a DAML-NAME>)
(defun daml-token-at-or-before (&optional pt buf)
    (setq buf (or buf (current-buffer)))
    (save-excursion
      (set-buffer buf)
      (if pt (goto-char pt) (setf pt (point)))
      (let (start end toktype tok)
	;;; skip forward over initial line whitespace
	(if (looking-at "^[ \t]*")(skip-chars-forward " \t"))
       ;;; if at beginning of a tag, move past < or </ and declare entity
	(cond ((or (next-char-is ?<) (between-chars ?< ?/))
	       (forward-char 1)
	  ;; skip / if begin of end tag.
	       (when (next-char-is ?/)
		 (setf toktype 'ENTNAME)
		 (forward-char 1)
		 (skip-chars-forward "\ \t\n")))
	      ;; if in '=..space.."' skip forward to the word
	      ;; do not change position for this test (ugh)
	      ;;; note: if after the opening dblquote, then handled by else. 
	      ((let ((pe (point)))
		 (skip-chars-backward "\ \t=")
		 (prog1 (looking-at "=[\ \t\n]*\"")
		   (goto-char pe)))
	       (goto-char (match-end 0))
	       (setf toktype 'REFERENCE)) 
	 
	      (t ;; else move backward till you find a token
	       (when (looking-at "/>") (backward-char))
	       (unless (between-chars "[\ \t\n]" "\w")
		 (daml-backup-over-whitespace))
	       ;; if at or after end of tag, skip back over tag end 
	       (when (or (prev-char-is ?>)
			 (between-chars ?/ ?>))
		 (skip-chars-backward "/>"))
	       ;; skip any additional whitespace to find a word
	       (daml-backup-over-whitespace)
	       (if (between-chars ?\" "[\ \t\n/>]") ;; dbl-quote
		   (backward-char))
	       ;;; then get to begining of word
	       (skip-chars-backward daml-get-token-chars)
	       ;; since token-chars can contain /, check for end tag begin.
	       (if (between-chars ?< ?/) (forward-char))
	       ))
	(setq start (point))
;	(dbg "%s start %s.%s" toktype (char-before) (char-after))
	(skip-chars-forward daml-get-token-chars)
	(setq end (point))
;	(dbg "%s end %s.%s" toktype (char-before) (char-after))
	(setq tok (buffer-substring-no-properties start end))
	(when (null toktype)
	  (setf toktype
		(cond ((looking-at "[ \t\n]*=") 'ATTNAME)
		      ((daml-look-before start '(?\" "[ \t\n]" * ?=) ) 'REFERENCE)
		      ((daml-char-before start "[</]") 'ENTNAME)
		      ((daml-char-before start ">") 'VALUE)
		      (t nil))))
;	(list tok toktype)
;	  (dbg "found token %s" (list tok toktype))
	(daml-parse-rdf-token tok toktype)
	)))


(defun daml-name-to-string (tup &optional no-target) 
  (let ((base (daml-name-uri tup))
	(target (daml-name-target tup)))
    (cond ((and (stringp base) (or no-target (null target))) base)
	  ((and (stringp base) (stringp target)
		(concat base "#" target)))
	  ;; in this case, we really want to look in the local buffer
	  ;; which wasn't declared by a default XMLNS
	  ((and (stringp target) (null base)) (concat "#" target)))))


    
;;; break directory, filename, (optional target) as list (path file . target)
;;; e.g. http://www.daml.org/file.ext#target -> ("http://www.daml.org/" "file.ext" . "target")

;;; Note: We may not need this because:
;;; fn (url-basepath "http://www.daml.org/file.ext") -> "http://www.daml.org/"
;;; and with optional arg T, the actual filename -> "file.ext"
;;; it uses fns file-name-directory and file-name-nondirectory that are part of emacs

(defun daml-filename-parts (fname &optional no-target)
  (let* ((tpos (position ?# fname :from-end t))
	 (slpos (position ?/ fname :from-end t)))
    (setq slpos (1+ slpos)) ; move after /
    (cons (substring fname 0 slpos)
	  (cons (substring fname slpos  tpos)
		(if (and tpos (not no-target))
		    (substring fname (1+ tpos) nil)
		  nil)))))

;;; defined in w3/url-parse.el:
;;;(url-generic-parse-url "http://www.daml.org/2001/03/daml+oil.daml#Restriction")
;;;  = ["http" nil nil "www.daml.org" nil "/2001/03/daml+oil.daml" "Restriction" nil t]
;;; =[type user pwd host port filename target attributes fullness] 
;;; (accessors like {set-}url-<field>)

(defun daml-make-url-string (type host path &optional target)
  (let ((base (format "%s://%s/%s" type host path)))
    (when target (concat base "#" target))))


;;; see if a file with the same name exists in the directory of the current buffer
;;; url should not include target id
(defun daml-file-found-locally (url &optional buf)
  (let* ((curfile (or (and buf (buffer-file-name (get-buffer buf))) (buffer-file-name)))
	 (curdir (when curfile (file-name-directory curfile)))
	 (urlfile (and url (file-name-nondirectory url)))
	 (localfile nil))
    (setq localfile (when (and curdir urlfile) (concat curdir urlfile)))
    (cond ((and localfile (file-exists-p localfile)) (file-truename localfile))
	  ((and localfile (not (file-name-extension localfile))) ;; no extension
	   (setq localfile (concat localfile ".daml"))
	   (and (file-exists-p localfile)(file-truename localfile))
	  ))))

;;; Prompt for the url, with smart default based on context around (POINT)
;;; use the nearest thing to POINT in the daml file as the default thing to find
;;; url will include the ID of the thing in the file (relative ref) to search for.
;;; build an expanded URL string, including target name


;;; returns a triple of < (BUFFER | FILE), (buffername | url), targetID >
;;; tuple (a uri struct)  used if recursive call 
(defun daml-read-url-with-default (&optional tuple)
  (interactive) ;; interactive for testing only
  (let ((in-daml-buffer (eq 'daml-mode major-mode))
	tup)
    (setf tup
	  (cond (tuple)
		;; TUP is a DAML-NAME tuple
		(in-daml-buffer (daml-token-at-or-before))
		(t (make-daml-name :source ""))))
  (let* ((url (daml-name-uri tup)) ;; the base uri up to target
	 (target (daml-name-target tup))
	 (in-buf (cond ((null url) (if in-daml-buffer (current-buffer)))
		       (t (daml-url-already-loaded url))))
	 (locally (when (and (not in-buf) url) 
		    (daml-file-found-locally url)))
	 (default (or (and tup (daml-name-to-string tup))
		      "http://www.")) ;; full expanded name w. target
	 confirm confirmp locally-short
	 note)
;;    (scratch-msg "url %S target %S locally %S in-buf %S\n" url target locally in-buf)
    ;; shorten local pathname for presentation in minibuf w. URL
    ;; to make a prompt that doesnt take up too much room
    (when locally
      (setf locally-short
	    (if  (> (length locally) 15)
		(concat "./" (file-name-nondirectory locally))
	      locally)))

    (setf note (cond ((eq (current-buffer) in-buf) " (this buffer)")
		     (in-buf (format " (buffer %s)" (buffer-name in-buf)))
		     (locally (format " (in %s)" locally-short))
		     (t "(www)")))
    (unless (and tuple in-buf)
;      (dbg "Note= %S" note)
      (setf confirm 
	    (completing-read (format "URL%s: " note)
			     'w3-url-completion-function nil nil default)))
    (setf confirmp (and confirm (equal confirm default)))
    (cond ((and confirm (not (equal confirm default)))
	   ;; see if typed 'newurl without backing up.
	   (setf confirm (daml-find-confirmation-replacement confirm)) 
	   (daml-read-url-with-default (daml-parse-rdf-token confirm nil)))
	   ;; if the base url path is local or in a buffer, say so unless *daml-in-buffer-quietly*
	   ((and in-buf 
		 (or *daml-in-buffer-quietly* 
		     (y-or-n-p (format "Find ID %s in existing buffer %s" target in-buf))))
	    (list 'BUFFER in-buf target))
	   ;; if local file, and already confirmed
	   ((and locally 
		 (or confirmp
		    (y-or-n-p (format "Find ID %s in local file %s" target locally))))
	    (list 'FILE locally target))
	   (t  (or confirm (and tuple in-buf default)))))))

;;; if string ends with '...stuff... then they typed in a replacement, 
;;; so remove everything up to and including '
;;; else the original string stands. 
(defun daml-find-confirmation-replacement (str)
  (let ((pos (position ?' str)))
    (if pos (substring str (1+ pos))
      str)))


;;; Get a name at point, prompt with it expand to a full URI, then go get it.
;;; If user edits name, allow for re-expansion.  
;;; to facilitate local caching, see if file is cached in a buffer or a local directory
;;; (local directory if it was used for another URI with the same basepath) (upto filename)
;;; First decide if the file is already in a buffer, in which case just go there.
;;; If not, but the uri is like that of some buffer up through directory, then ask
;;; if getting the local copy from a file is preferred. 
;;; Finally, load file from the web, then do search for ID
(defun daml-find-def-in-buffer (target &optional buf)
  (interactive "sBuffer Find URI:")
  (if buf (set-window-buffer (selected-window) buf)
    (daml-remember-position) ; if called stand-alone, work in this buffer, remember position.
    (setq buf (current-buffer)))
  (let ((oldpt (point))
	pos)
    (goto-char (point-min))
    (when (and target (stringp target) (> (length target) 0))
      (setq pos (or 
		 (search-forward-regexp
		  (format "\\(\ \\|rdf:\\)ID[\ \t\n]*=[\ \t\n]*\"%s\"" target) nil t)
		 (search-forward-regexp
		  (format "\\(\ \\|rdf:\\)about[\ \t\n]*=[\ \t\n]*\"#%s\"" target) nil t)))
      (if pos (beginning-of-line)
	(message "Cound not find definition of '%s' in buffer." target)
	(goto-char oldpt)))
    pos))


;;; THIS IS THE META DOT FUNCTION
;;; Use the reference under point as a default to find. 
;;; Do expansion is to the current or another existing buffer, just
;;; jump to it. 

;;; because we are trying to do this asynchronously, the post-retrieval actions are done by
;;; daml-post retrieval, rather than here. 

(defun daml-view-url (url)
  (interactive (list (daml-read-url-with-default)))
  (daml-remember-position)
;  (trace-msg "URL is %s" url)
  (cond ((listp url)
	 (case (car url)
	   (BUFFER (daml-find-def-in-buffer (third url) (second url)))
	   (FILE (find-file (second url))
		 (daml-mode) ;; in case not a .daml extension
		 (daml-find-def-in-buffer (third url) nil))))
	((and (stringp url) 
	      daml-w3-loadedp)
	 (daml-view-url-page url))
	(t (message "W3 Package not loaded. Only local files available.")
	)
  ))

  
(defun daml-remember-position () (interactive)
    (setf *daml-last-position* (cons (buffer-name (current-buffer)) (point))))

(defun daml-goto-previous-position () (interactive)
  (let* ((newpos *daml-last-position*)
	 (buf (get-buffer (car newpos))))
    (daml-remember-position)
    (unless (eq (current-buffer) buf)
      (set-window-buffer (selected-window) buf))
    (goto-char (cdr newpos))))


;;;;;;;;;;;;;;;;  WEB-RETRIEVE AND VIEW A DAML NAME or RESOURCE reference ;;;;;;;;;;;;;;;
;;; The result of a lot of digging: fn URL-RETRIEVE (url.el) calls
;;; URL-RETRIEVE-INTERNALLY (w3/url.el) that is the driver. 
;;;   this funcalls var HANDLER which is usu URL-HTTP (based on URL extension or default "auto")

;;; URL-HTTP starts a fork for the transfer and sets up URL-SENTINEL (see url-http.el)
;;; to run on completion of the transfer when url-be-asynchronous is T. 

;;; URL-SENTINEL calls URL-PARSE-MIME-HEADERS, which sets status to the status code in the 
;;; HTTP response header and sets URL-CURRENT-MIME-TYPE to return type 
;;; (e.g. "text/http" or "text/plain")
;;; and sets URL-CURRENT-MIME-VIEWER by calling MM-MIME-INFO (mm.el) on the type.
;;; it should also delete the header once parsed. 

;;; URL-SENTINEL funcall's the var URL-DEFAULT-RETRIEVAL-PROC which = 'URL-DEFAULT-CALLBACK
;;; except in case of certain status codes  '(401 301 302 303 204)
;;; 
;;; URL-DEFAULT-CALLBACK does (set-buffer buf) (buf is its arg) and
;;; funcall's URL-CURRENT-CALLBACK-FN if defined, or else
;;; calls W3-SENTINEL (w3/w3.el), which calls W3-BUILD-CONTINUATION 
;;; W3-BUILD-CONTINUATION builds the list '(url-uncompress w3-pass-to-viewer) 
;;; of functions that W3-SENTINEL calls before returning.

;;; W3-PASS-TO-VIEWER proceeds to rename the temp buffer using the non-directory part of the url
;;; and put it in fundamental mode (at least for daml files which are of type text/plain)

;;; for type text/http, however, (as returned with an error page generated by most servers)
;;; we get different behavior. in this case, 
;;; MM-MIME-INFO acts like this
;;;  mm-mime-info: st="text/html; charset=iso-8859-1" nd=nil request=t
;;;  <- mm-mime-info: (("viewer" . w3-prepare-buffer) ("test" fboundp (quote w3-prepare-buffer)) ("type" . "text/html"))
;;; and W3-PREPARE-BUFFER is funcalled by W3-PASS-TO-VIEWER. This *DELETES* the original buffer
;;; and spits the parse'd text into a new buffer, using the HTML commands in the original.

;;;;; ANALYSIS:

;;; For the purposes of daml files, which should be plain text, we can rely on the current buffer 
;;; containing the text after url-retrieve and its callback complete, 
;;; if it was successful. For other cases, we modify fns below
;;; to get the status and kill the buffer if not status 200. 

;;; The current-buffer we will see is named for the non-directory part of the url.
;;; and keep track of which buffer names correspond to each url, so we don't go generating them over 
;;; and over.  We should also make these buffers read only by default. 

;;; (url-generic-parse-url "http://www.daml.org/2001/03/daml+oil.daml#Class")
;;; = ["http" nil nil "www.daml.org" nil "/2001/03/daml+oil.daml" "Class" nil t]

(defvar *daml-last-retrieval-url* nil "url that was focus of last retrieval")

;;; This deals just with retrieving the web page to a buffer
(defun daml-view-url-page  (url)
  (interactive) ;; but no prompt
  (let* ((old-asynch (default-value 'url-be-asynchronous))
	 (url-inhibit-uncompression nil)
	 (url-mime-accept-string "*/*") ;; not sure what this does exactly
	 (pos (position ?# url))
	 retr) 

    (setf url-working-buffer "*URL-TEMP*")
    (setf *daml-last-retrieval-url* url)
    (when pos (setf url (substring url 0 pos))) ;; remove target in file
    (unwind-protect
	(progn
	  (setq-default url-be-asynchronous t)
	  (save-excursion
	    (setf retr  (daml-url-retrieve url)) ;; was url-retrieve
	    ))
      (setq-default url-be-asynchronous old-asynch))
    ))


(defun daml-check-http-header ()
  (beginning-of-buffer)
  (when (search-forward-regexp "HTTP[^\ ]* \\([0-9]\\)" nil t)
    (let* ((beg (match-beginning 1))
	   (end (match-ending 1))
	   (http-code (buffer-substring beg end)))
      (cond ((equal http-code "200") ; OK
	     (daml-strip-off-http-header)
	      nil)
	    ((equal http-code "404")
	     "URL NOT FOUND.")
	    (t (format "URL RETURNED ERROR %s" http-code))))))

;;; may not need this anymore, but not sure
(defun daml-strip-off-http-header ()
  (beginning-of-buffer)
;; better way to look for two CRLF's? ( =  ^M^J^M^J)
  (search-forward "

" nil t)
  (delete-region 0 (point)))


;;;;;;;;;;;;;;;;;;;;;;
;;;; THE APPROACH: do a modified url-retrieve that works specifically for text/plain
;;;; and saves the buffer (if any) and status for us. 


(defvar *daml-last-retrieval-status* nil "(status . buffer) when set by asynch retrieval")
;;; we force this to be saved as local on the buffer it is read in


;;; based on url-retrieve (url.el)
;;; don't know if this needs to be a separate fn. I was trying to join after forking.

(defun daml-url-retrieve (url &optional no-cache expected-md5)
  "Retrieve a document over the World Wide Web.
The document should be specified by its fully specified
Uniform Resource Locator.  No parsing is done, just return the
document as the server sent it.  The document is left in the
buffer specified by url-working-buffer.  url-working-buffer is killed
immediately before starting the transfer, so that no buffer-local
variables interfere with the retrieval.  HTTP/1.0 redirection will
be honored before this function exits."

  (daml-remember-position)
  (url-do-setup)
  ;;(url-download-minor-mode t)
  (if (and (fboundp 'set-text-properties)
	   (subrp (symbol-function 'set-text-properties)))
      (set-text-properties 0 (length url) nil url))
  (if (and url (string-match "^url:" url))
      (setq url (substring url (match-end 0) nil)))
  (let ((status (daml-retrieve-text-url url no-cache)))
    status))

;;; copied from recent version of url.el (for xemacs)
(defun daml-get-url-working-buffer-name ()
  "Get a working buffer name such as ` *URL-<i>*' without a live process and empty"
  (let ((num 1)
	name buf)
    (while (progn (setq name (format " *URL-%d*" num))
		  (setq buf (get-buffer name))
		  (and buf (or (get-buffer-process buf)
			       (save-excursion (set-buffer buf)
					       (> (point-max) 1)))))
      (setq num (1+ num)))
    name))


;;; Based on URL-RETRIEVE-INTERNALLY (FROM URL.EL)  
;;; mimimally modified to add a different callback function
(defun daml-retrieve-text-url (url &optional no-cache)
  (let* ((url-working-buffer (daml-get-url-working-buffer-name))
	 (urlobj (url-generic-parse-url url))
	 (type (url-type urlobj))
	 (url-using-proxy (if (url-host urlobj)
			      (url-find-proxy-for-url urlobj (url-host urlobj))
			    nil))
	 (handler nil)
	 (original-url url)
	 (cached nil)
	 (status nil)
	 working-buf
	 )
    (if url-using-proxy (setq type "proxy"))
    (setq cached (url-is-cached url)
	  cached (and cached (not (url-cache-expired url cached)))
	  handler (if cached
		      'url-cache-extract
		    (car-safe
		     (cdr-safe (assoc (or type "auto")
				      url-registered-protocols))))
	  url (if cached (url-cache-create-filename url) url))

    (save-excursion
      (set-buffer (get-buffer-create url-working-buffer))
      (setq working-buf (current-buffer))
      (setq url-be-asynchronous t)
      (make-local-variable 'track-mouse)
      (make-local-variable 'buffer-original-url)
      (setq buffer-original-url url)
      (if (boundp 'buffer-file-coding-system)
	  (setq buffer-file-coding-system nil))
      (setq url-current-can-be-cached (not no-cache)
	    url-current-object urlobj)

;;; Add a modified w3-sentinel as callback fn to be funcalled by url-default-callback 
;;; rather than defaulting to the real w3-sentinel
;;; This variable is buffer local, so don't need to worry about resetting it. 

      (setq url-current-callback-data nil
	    url-current-callback-func 'daml-w3-callback)
      ) ; end save-excursion
    

    (if (and handler (fboundp handler)) ;; should be url-http if starts with http:
	(funcall handler url)
      ;; else no handler - don't throw up anything (deleted code here- MB) 
      nil)

    (cond
     ((and url-be-asynchronous (not cached)
	   (member type '("http" "https" "proxy" "file" "ftp")))
      nil)
     ((and url-be-asynchronous (get-buffer url-working-buffer))
      (funcall url-default-retrieval-proc (buffer-name)))
     ((not (get-buffer url-working-buffer)) nil)
     ((and (not url-inhibit-mime-parsing)
	   (or cached (url-mime-response-p t)))
      (or cached (url-parse-mime-headers nil t))))
    (if (and (or (not url-be-asynchronous)
		 (not (equal type "http")))
	     url-current-object
	     (not url-current-mime-type))
	(progn
	  (setq url-current-mime-type 
	    (mm-extension-to-mime
	     (url-file-extension (url-filename url-current-object))))
	  (if (and (not url-current-mime-type)
		   (url-looks-like-html))
	      (setq url-current-mime-type "text/plain")))) ;; was text/html

    (if (not url-be-asynchronous)
	(url-store-in-cache url-working-buffer))
    (if (not url-global-history-hash-table)
	(setq url-global-history-hash-table (make-hash-table :size 131
							     :test 'equal)))
    (if (not (string-match "^\\(about\\|www\\):" original-url))
	(progn
	  (setq url-history-changed-since-last-save t)
	  (cl-puthash original-url (current-time)
		      url-global-history-hash-table)))
    (cons status working-buf))) ;; return the actual buffer, not the name, which changes (MB)


;;; DAML-W3-CALLBACK
;;; based on w3-sentinel, but made the url-current-callback-func by above
;;; so called by url-default-callback instead of w3-sentinel

(defun daml-w3-callback (&optional proc string)
  (let ((minibuffer-window (active-minibuffer-window))
	stat)
    (unwind-protect
	(progn
	  (if minibuffer-window (other-window 1))
	  (set-buffer url-working-buffer)
	  (if (or (stringp proc) (bufferp proc)) 
	      (setq w3-current-last-buffer proc))
	  (remove-hook 'after-change-functions 'url-after-change-function)
	  (if (fboundp 'clear-progress) (clear-progress))

	  (when url-be-asynchronous ;; should be T here (MB)
	    (cond
	     ((not (get-buffer url-working-buffer)) nil)
	     ((url-mime-response-p) 

	      (setf stat (url-parse-mime-headers))
	      (setf *daml-last-retrieval-status* (cons stat (current-buffer)))
;;;	      (scratch-msg "in callb %S" *daml-last-retrieval-status*)
	      )) ;; buffer local
		;; shouldn't happen, as we are always asynch - MB
	    (if (not url-current-mime-type)
		(setq url-current-mime-type 
		  (or 
		   (mm-extension-to-mime (url-file-extension (url-filename url-current-object)))
		   "text/plain")))) ;; was text/html, but that's not the default we want (MB)

	  ;; hack for charset not indicated in MIME headers but in a META tag ...
	  (if (not url-current-mime-charset)
	      (save-excursion
		(goto-char (point-min))
		(if (or (re-search-forward w3-meta-content-type-charset-regexp nil t)
			(re-search-forward w3-meta-charset-content-type-regexp nil t))
		    (setq url-current-mime-type
			  (buffer-substring-no-properties (match-beginning 1) (match-end 1))
			  url-current-mime-charset
			  (buffer-substring-no-properties (match-beginning 2) (match-end 2))))))
	  (if (not (string-match "^www:" (or (url-view-url t) "")))
	      (w3-convert-code-for-mule (and (stringp url-current-mime-type)
					     (downcase url-current-mime-type))
					(and (stringp url-current-mime-charset)
					     (downcase url-current-mime-charset))
					(and (stringp url-current-mime-encoding)
					     (downcase url-current-mime-encoding))))

;;;	  (scratch-msg "\n before continuation buf=%s" (current-buffer))
	  (let ((x (w3-build-continuation))
		(url (url-view-url t)))
	    (when (and *daml-last-retrieval-status*
		       (= (car *daml-last-retrieval-status*) 200))
	      (while x (funcall (pop x))))
	    ;; ADDEDcode to run when done -- success or no (MB)
	    (daml-post-retrieval)
	    )
	  nil
;;;	  (scratch-msg "\nin w3-cback buf=%s" (current-buffer))
;      (if minibuffer-window
;	  (set-frame-selected-window (selected-frame) minibuffer-window))
	  ))))

;;; expose the temporary buffer and rename it
(defun daml-make-buffer-url-visible ()
  (let ((bufnam (when url-current-object
		  (url-generate-new-buffer-name
		   (file-name-nondirectory
		    (or (url-filename url-current-object)
			"Unknown-URL"))))))
    (cond ((null bufnam) nil)
	  ((string= bufnam "")
	   (setq bufnam (url-generate-new-buffer-name
			 (url-view-url t)))))
    (when (stringp bufnam) 
      (rename-buffer bufnam)
      ;; Make the URL show in list-buffers output
      (make-local-variable 'list-buffers-directory)
      (setq list-buffers-directory (url-view-url t))
      (set-buffer-modified-p nil)
      (buffer-enable-undo)
      )))



;;; force focus onto the buffer
(defun daml-set-window-buffer (buf)
  (when (and (bufferp buf) (buffer-live-p buf))
;    (push-window-configuration)
	(if in-xemacs-p (record-buffer buf))
    (set-window-buffer (if (eq (selected-window) (minibuffer-window))
			   (next-window (minibuffer-window))
			 (selected-window))
		       buf)
    buf))


(defun daml-post-retrieval ()
  (let* ((buf (cdr *daml-last-retrieval-status*))
	 (stat (car *daml-last-retrieval-status*))
	 (prevbuf (car *daml-last-position*))
					;	(prevbufpt (cdr *daml-last-position*))
	 (pos (position ?# *daml-last-retrieval-url*))
	 (uri (if pos (substring *daml-last-retrieval-url* 0 pos) 
		*daml-last-retrieval-url*))
	 (new-buf-name (file-name-nondirectory uri))
	 (target (if pos (substring *daml-last-retrieval-url* (1+ pos) nil)))
	 )
    (when buf
      (set-buffer buf)
      (setq stat (or stat 
		     (if (search-forward "<rdf:RDF" nil t) 200)))
      (beginning-of-buffer)
;      (dbg "post retrieval stat=%s" stat)
      (cond ((eq stat 200)
	     (set-buffer buf)
	     (message "Retrieval completed.")
	     (daml-make-buffer-url-visible)
	     (daml-mode)
	     (toggle-read-only 1)
;;;	     (beginning-of-buffer)
	     (daml-url-new-buffer uri buf) ;; store ref to new daml buffer
	     (if target (daml-find-def-in-buffer target nil)))
	    (t (switch-to-buffer prevbuf)
;;;	     (scratch-msg "buf=%s prev=%s\n" buf prevbuf)
	       (when (and buf (not (eq buf prevbuf)))
		 (kill-buffer buf)) ;; retrieval temp buffer
	       (message "Error: WWW returned status %S" stat))))
;;;    (scratch-msg "\nin post-retr buf= %S stat=%S\n" (current-buffer) *daml-last-retrieval-status*)
    ;; for last line of url-sentinel
    (daml-set-window-buffer buf)
;;;    (scratch-msg "\nend post-retr %s" (window-buffer (selected-window)))
    buf
    ))


;;;; THIS MODIFIES THE FUNCTION IN URL.EL
;;;; Needed to save the status of the return from disappearing
;;;;  this to save status on the buffer
(defun url-sentinel (proc string)
  (let* ((buf (if (processp proc) (process-buffer proc) proc))
	 (url-working-buffer (and buf (get-buffer buf)))
	 status
	 daml-retrievalp)

    (if (not url-working-buffer)
	(url-warn 'url (format "Process %s completed with no buffer!" proc))
      (save-excursion
	(set-buffer url-working-buffer)
	(remove-hook 'after-change-functions 'url-after-change-function)
;;;	(scratch-msg "in sent asynch=%s wb=%s" url-be-asynchronous url-working-buffer)

	(if url-be-asynchronous
	    (progn
	      (widen)
;;;	      (scratch-msg "in url-sent/async")
	      (cond
	       ((and (null proc) (not url-working-buffer)) nil)
	       ((url-mime-response-p)

		(setq status (url-parse-mime-headers))
		(when (boundp 'buffer-original-url) 
		  (setf daml-retrievalp t)
		  (setf *daml-last-retrieval-status*
		    (cons status (current-buffer))))
;;;		(scratch-msg "url-sent wb= %S status %S" (current-buffer) status)
		))
	      (if (not url-current-mime-type)
		  (setq url-current-mime-type (or
					       (mm-extension-to-mime
						(url-file-extension
						 (url-filename
						  url-current-object)))
					       "text/plain"))))))
      (if (member status '(401 301 302 303 204))
	  nil
	(funcall url-default-retrieval-proc (buffer-name url-working-buffer))))
    ;; if a daml-retrieval, force the current buffer to be the found one
    ;; (overrides a save-excursion and save-window-excursion in url-default-callback)
    (if (and daml-retrievalp *daml-last-retrieval-status*)
	(daml-set-window-buffer (cdr *daml-last-retrieval-status*))
    ;; FSF Emacs doesn't do this after calling a process-sentinel
	(set-buffer (window-buffer (selected-window))))))




;; clean out temp buffers created during retrievals (during debugging)
(defun delete-temp-url-buffers () (interactive)
  (dolist (buf (buffer-list))
    (when (string-match "\ \*URL-" (buffer-name buf))
      (kill-buffer buf))))
