;;; daml-summary.el --- a part of the simple DAML mode for Xemacs

;;; (C) 2001 BBN Technologies
;;; by Mark Burstein, with assistance from Ken Anderson and Richard Shapiro

;;; This package is designed to be used with XEmacs and Gnu Emacs



;;; DAML SUMMARY MODE and display fns - like a headers list in VM MODE

;;; see daml-display-summary-list
(defvar daml-summary-list nil "alist of references in summary-buffer")
(make-variable-buffer-local 'daml-summary-list)


;;; these vars should have the same values in the daml buffer and its summary buffer
(defvar daml-buffer-definition-list nil "List of DAML definitions culled from current buffer.")
(make-variable-buffer-local 'daml-buffer-definition-list)

(defvar daml-summary-buffer nil "buffer containing summary of current daml buffer")
(make-variable-buffer-local 'daml-summary-buffer)

(defvar daml-buffer nil "current daml content buffer buffer")
(make-variable-buffer-local 'daml-buffer)

(defvar daml-summary-config nil)
(make-variable-buffer-local 'daml-summary-config)

(defvar daml-summary-mode-hook nil)

;;;These function are basically the same as what vm uses to enable mouse highlighting
;;; in the vm summary window.  

(defun daml-fsfemacs-mouse-p ()
  (and (not in-xemacs-p)
       (fboundp 'set-mouse-position)))

(defun daml-xemacs-mouse-p ()
  (and in-xemacs-p
       (fboundp 'set-mouse-position)))

(defvar daml-popup-menu-on-mouse-3 nil) ;; not implemented

(defun daml-install-summary-mouse (map)
  (cond ((daml-xemacs-mouse-p)
	 (define-key map 'button2 'daml-mouse-button-2))
	((daml-fsfemacs-mouse-p)
	 (define-key map [mouse-2] 'daml-mouse-button-2)
	 (if daml-popup-menu-on-mouse-3
	     (progn
	       (define-key map [mouse-3] 'ignore)
	       (define-key map [down-mouse-3] 'daml-mouse-button-3))))))


(defvar daml-summary-mode-map 
    (let ((map (make-sparse-keymap)))
      (define-key map "n" 'daml-next-def-summary)
      (define-key map "p" 'daml-previous-def-summary)
      (define-key map " " 'daml-current-def-summary)
      (define-key map "s" 'daml-refresh-summary)
      (daml-install-summary-mouse map)
      map
      )
  "Keymap for VM Summary mode")


(defun daml-list-forms-buffer (&optional buf)
  (interactive)
  (let* ((curbuf (or (and buf (get-buffer buf))(current-buffer)))
	 (curpos (point))
	 (cbufname (buffer-name curbuf))
	 (sbufname (concat cbufname " Summary"))
	 )
    (save-excursion
      (set-buffer curbuf)
;      (message "Collecting definitions list...")
      (beginning-of-buffer)
      (when (and (re-search-forward "<rdf:RDF" nil t)
		 (re-search-forward ">" nil t))
	(let ((next (point))
	      (defs nil) 
	      def)
	  (setq next (cdr (daml-goto-tag-begin)))
	  (while (and next
		      (not (eobp))
		      (not (looking-at "</"))) ;; e.g. </rdf:RDF>
;	    (dbg (format "list loop at %s %s" (point)(bs 10)))

	    (cond ((looking-at "<\\(rdfs:comment\\|!--\\)")
;		   (dbg "skip comment ")
		   (setq next (and (daml-forward-tag)
				   (cdr (daml-goto-tag-begin))))
		   )
		  (t 
		   ;;; read tag and ID now moves past the def
		   (setq def (daml-read-tag-and-id))
;		   (dbg "Def: %s" def)
		   (push def defs)
		   (setq next (cdr (daml-goto-tag-begin)))
		   ))

	    )
;	  (message nil)
	  (goto-char curpos)
	  (setq daml-buffer-definition-list (reverse defs)))))))

;; return tag info as (begpt endpt tag id|about url)
(defun daml-read-tag-and-id ()
  (interactive)
  (let* ((begpt (point))
	 (endpt (daml-forward-tag))
	 (ebtagpt 0)
	 (idinfo (list endpt begpt)))
    (when (and begpt endpt)
      (goto-char begpt)
      (setq ebtagpt (daml-search-fwd ">"))
      (goto-char begpt)
      (when (and (not (eobp))
		 (looking-at "<[^/]")
		 (re-search-forward "<\\([^ \t\n>]+\\)" nil t))
	(let* ((tb (match-beginning 1))
	       (te (match-end 1))
	       (tag (buffer-substring tb te))
	       )
;;;	    (dbg "tag=%s ebtgpt=%s" tag ebtagpt)
	    (push tag idinfo)
	    (goto-char (match-end 0))
	    (when (re-search-forward 
		   "\\(\\(rdf:\\|\\)\\(ID\\|about\\)[ \t\n]*=\\)\"\\([^\"]*\\)\""
		   ebtagpt t)
	      (let* ((bid (match-beginning 1))
		     (eid (match-end 1))
		     (burl (match-beginning 4))
		     (eurl (match-end 4))
		     (idstr (buffer-substring bid eid))
		     (urlstr (buffer-substring burl eurl)))
		(push (list idstr urlstr) idinfo)
		;;; now look thru subtags for a subclass/subproperty
		(when (re-search-forward ">" nil t)
		  (unless (looking-back-at "/>")
		  (push (daml-read-parents) idinfo)))
		)))))
    
;    (goto-char begpt)
    (setq idinfo (reverse idinfo))
;    (dbg "res=%S" idinfo)
    idinfo
    ))

(defun daml-sub-id () 
  (when (looking-at 
   "<rdfs:sub\\(Property\\|Class\\)Of[^>]+rdf:resource[ ]*=[ ]*\"\\([^\"]*\\)")
;;;    (dbg "matched beg %s end %s" (match-beginning 2) (match-end 2))
    (buffer-substring (match-beginning 2) (match-end 2))))


(defun daml-read-parents ()
  (let ((done nil)(subs nil) (sub nil))
    (while (and (not done)
		(daml-goto-tag-begin))
;      (dbg "in readparents at %s\n" (bs 10))
      (cond ((looking-at "</")(setq done t) (daml-up-tag-end))
	    ((setq sub (daml-sub-id))
	     (push sub subs)
	     (daml-forward-tag)
	     (daml-goto-tag-begin))
	  (t (daml-forward-tag)
;	     (dbg "after ft %s" (bs 10))
	     (daml-goto-tag-begin)
	     )))
  subs))
	   

;; for testing
;(define-key daml-mode-map "\e," 'daml-read-tag-and-id)

(defun daml-display-summary-list (&optional recomputep daml-buf summary-buf)
  (let* ((dbuf (or (and daml-buf (get-buffer daml-buf)) daml-buffer))
	 (sbuf (or (and summary-buf (get-buffer summary-buf)) daml-summary-buffer))
	 (deflist (cond ((and (not recomputep) daml-buffer-definition-list))
			((consp recomputep) recomputep)
			(t (daml-list-forms-buffer dbuf))))
	 (summary-list nil)
	 (maxlen 0)
	 )
;    (scratch-msg "in display-list defs=%s sbuf=%s" deflist sbuf)
    (save-excursion
      (set-buffer sbuf)
      (setq buffer-read-only nil)
      (erase-buffer)
      (setq daml-buffer-definition-list deflist)
;      (scratch-msg "in display sum buflist=%s" deflist)
      ;; header line
      (let ((pt (point)))
	(insert (format "<!-- Definitions in %s (& parents)--> \n" (buffer-name dbuf)))
	(daml-mouse-set-mouse-track-highlight pt (point))
	(push (cons pt nil) summary-list)
	)
      ;; find out how long things are (for column creation)
      (dolist (elt deflist)
	(let ((len (length (third elt))))
	  (if (> len maxlen)(setq maxlen len))))
      (dolist (elt deflist)
	(let* ((pt (point)) 
	       (tag (third elt))
	       (id (first (fourth elt)))
	       (name (second (fourth elt)))
	       (parents (fifth elt))
	       (taglen (length tag))
	       (idlen (length id))
	       )
	  (push (cons pt elt) summary-list)
	  
	  (insert (format "%s %s%s%s %s    %s" 
			  tag (make-string (- maxlen taglen) ?\  ) 
			  (make-string (- 10 idlen) ?\ )
			  id
			  name
			  (if parents parents "")
			  ))
	  (daml-mouse-set-mouse-track-highlight pt (point))
	  (insert "\n")
	  ))
      (setq daml-summary-list summary-list)
      (font-lock-mode 1)
      (setq buffer-read-only t)
      )))


(defun daml-refresh-summary () 
  "Refresh the DAML Summary window's contents after the DAML file changed."
  (interactive)
  (set-window-configuration daml-summary-config)
  (daml-display-summary-list t))


;;; create an overlay for mouse highlighting
;;; if already had an overlay then move it. 
(defun daml-mouse-set-mouse-track-highlight (start end &optional overlay)
  (if (null overlay)
	(cond (in-xemacs-p
	       (let ((o (make-extent start end)))
		 (set-extent-property o 'start-open t)
		 (set-extent-property o 'priority 10)
		 (set-extent-property o 'highlight t)
		 o ))
	      (t
	       (let ((o (make-overlay start end)))
		 (overlay-put o 'mouse-face 'highlight)
		 o )))

    (cond (in-xemacs-p
	   (set-extent-endpoints overlay start end))
	  (t
	   (move-overlay overlay start end)))))



;;; other widnow
;;; get-buffer-window - window displaying  buffer
;;; window-height
;;; split-window


;;; starting from daml buffer, split window, save configuration
(defun daml-make-summary-config ()
  (interactive)
  (cond ((and (boundp 'daml-summary-buffer) daml-summary-buffer
	      (boundp 'daml-summary-config daml-summary-config))
	 (set-window-configuration daml-summary-config)
	 daml-summary-buffer
	 )
	(t 
	 (delete-other-windows) ;; one window to start
	 (let* ((daml-buf (current-buffer))
		(win (get-buffer-window daml-buf))
		(frame-ht (window-height))
		(num-forms (if (and (boundp 'daml-buffer-definition-list)
				      daml-buffer-definition-list)
			       (length daml-buffer-definition-list)
			     10))
		(sum-ht (if (< (/ frame-ht 2) (min num-forms 10))
			    (/ frame-ht 2)
			  (min (+ 3 num-forms) (/ frame-ht 4))))
		(lower-window (split-window win sum-ht))
		(summary-buffer
		 (get-buffer-create (format "%s Summary" (buffer-name))))
		)
	 ;; switch to the summary window, set buffer and resave vars
	   (set-window-buffer win summary-buffer)
;; save the configuration on both windows
	   (let ((config (current-window-configuration)))
	     (setq daml-summary-config config)
	     (setq daml-summary-buffer summary-buffer)
	     (setq daml-buffer daml-buf)
;	     (trace-msg "buffer=%s\n" (current-buffer))
	     (font-lock-mode 0)
	     (other-window 1)
	     ;; switch back to daml window
	     (setq daml-summary-config config)
	     (setq daml-summary-buffer summary-buffer) ;; in daml-buffer
	     (setq daml-buffer daml-buf)
	     summary-buffer
	     )))))
	

(defun daml-mouse-button-2 (event)
  (interactive "e")
  ;; go to where the event occurred
  (cond ((daml-xemacs-mouse-p)
	 (set-buffer (window-buffer (event-window event)))
	 (and (event-point event) (goto-char (event-point event))))
	((daml-fsfemacs-mouse-p)
	 (set-buffer (window-buffer (posn-window (event-start event))))
	 (goto-char (posn-point (event-start event)))))
  ;; now dispatch depending on where we are
  (mouse-set-point event)
  (beginning-of-line)
  (setq this-command 'daml-current-def-summary)
  (call-interactively 'daml-current-def-summary))



(defun daml-check-for-killed-summary ()
  (and (bufferp daml-summary-buffer) 
       (or (null (buffer-name daml-summary-buffer))
	   (not (buffer-live-p daml-summary-buffer)))
       (let ((mp daml-buffer-definition-list))
	 (setq daml-summary-buffer nil)
	 )))


(defun daml-next-def-summary () 
  (interactive)
  (forward-line 1)
  (end-of-line)
  (when (eobp) (forward-line -1))
  (daml-current-def-summary)
  )

(defun daml-previous-def-summary ()
  (interactive)
  (beginning-of-line)
  (unless (bobp)   (forward-line -1))
  (daml-current-def-summary)
  )


;;; to do this the robust way, we don't use the file positions, but do
;;; daml-find-def-in-buffer instead
(defun daml-current-def-summary ()
  (interactive)
  (beginning-of-line)
  (let* ((pt (point))
	 (rec (assoc pt daml-summary-list))
	 tag id name match-string)
    (when rec
      (setq tag (fourth rec))
      (setq id (car (fifth rec)))
      (setq name (second (fifth rec)))
      (setq match-string (format  "<%s[ \t\n]+%s[ \t\n]*\"%s\"" tag id name))
      )
    ;; make sure 2 windows showing, the other being the daml buffer
    (let ((cb (current-buffer))
	  (other (window-buffer (next-window (selected-window)))))
      (unless (eq other daml-buffer)
	(set-window-configuration daml-summary-config)))
    (save-excursion
      (other-window 1)
;      (set-buffer daml-summary-of-buffer)
;      (goto-char newpt)
      (let ((oldpt (point)))
	(beginning-of-buffer)
	(cond ((eq pt 1)) ;; on header line
	      ((and match-string (search-forward-regexp match-string nil t))
	       (beginning-of-line))
	      (t
	       (goto-char oldpt)
	       (message "Definition not found."))
	      ))
      (recenter 1)
      (other-window 1)
      )))


; old     '("\\(daml:\\|rdfs?:\\|\\)\\(Class\\|Ontology\\) +\\(rdf:\\|\\)\\(ID\\|about\\)= *\\([^ \n]*\\)"

(defvar daml-summary-font-lock-keywords
  (let ()
    (list 
     ;; black for the def parts of PROPERTY DEFINITION
     ;; and of TransitiveProperty UnambiguousProperty UniqueProperty
     (list
      (concat 
       "^\\(daml:\\|rdfs?:\\|\\)\\(Class\\|Ontology\\|\\([A-Za-z]*Property\\)\\)"
       " +\\(rdf:\\|\\)\\(ID\\|about\\)= *\\([^ \n]*\\)")
      '(1 daml-normal-face t)
      '(2 daml-keyword-face t) 
      '(4 daml-normal-face t)
      '(5 daml-keyword-face t) 
      '(6 (if (match-beginning 3) daml-property-face daml-class-face) t))
     '("\\([^:]+:\\|\\)\\(\\([A-Z]\\)?[^ ]*\\)[ ]+\\(rdf:\\|\\)\\(ID\\|about\\)=[ ]*\\([^ \n]*\\)"
       (1 daml-normal-face nil)
       (2 (if (match-beginning 3) daml-class-face daml-property-face) nil) 
       (4 daml-normal-face nil)
       (4 daml-keyword-face nil)
       (5 daml-string-face nil))
     ;; XML Comments: <!-- ... -->. - here the header.
     '("\\(<!--\\([^-]\\|-[^-]\\|--[^>]\\)*-->\\)"
       1 daml-comment-face t)
     )
    ))


(put 'daml-summary-mode 'font-lock-defaults '(daml-summary-font-lock-keywords nil nil))

(defun daml-summary-mode-internal ()
  "Mode for the summary window associated with DAML files. 
   mouse middle click on a line jumps the main window to that definition. 
Typing n or p moves next or previous line, and refocusses the main window.
Typing a space refocusses on the current line. 
Typing an 's' rebuilds the summary. "

  (setq mode-name "DAML Summary"
	major-mode 'daml-summary-mode
;	mode-line-format vm-mode-line-format
	;; must come after the setting of major-mode
;	mode-popup-menu (and vm-use-menus vm-popup-menu-on-mouse-3
;			     (vm-menu-support-possible-p)
;			     (vm-menu-mode-menu))

	buffer-read-only t
	truncate-lines t)
  ;; horizontal scrollbar off by default
  ;; user can turn it on in summary hook if desired.
  (and in-xemacs-p (featurep 'scrollbar)
       (set-specifier scrollbar-height (cons (current-buffer) 0)))
  (use-local-map daml-summary-mode-map)
  (cond	(in-xemacs-p
	 (put major-mode 'font-lock-keywords-case-fold-search nil))
	((string-lessp "19.28.89" emacs-version) ; Emacs 19.29 and later
	 (make-local-variable 'font-lock-defaults)
	 (setq font-lock-defaults '(daml-summary-font-lock-keywords nil nil)))

	;; this probably isnt working anyway:
	(t ; Emacs 19.28 and older
	 (make-local-variable 'font-lock-keywords-case-fold-search)
	 (make-local-variable 'font-lock-keywords)
	 (make-local-variable 'font-lock-no-comments)
	 (setq font-lock-keywords-case-fold-search nil) ; was t
	 (setq font-lock-keywords daml-summary-font-lock-keywords)
	 (setq font-lock-no-comments t)))

  (run-hooks 'daml-summary-mode-hook)
  )

;(fset 'vm-summary-mode 'vm-mode)
;(put 'vm-summary-mode 'mode-class 'special)



(defun daml-summarize (&optional display raise)
  "Summarize the contents of the current DAML buffer in a summary buffer."
  (interactive ) ;"p\np"
  (daml-check-for-killed-summary)
  (if (null daml-summary-buffer)
      (let ((dbuf (current-buffer))
	    (list (daml-list-forms-buffer))
	    (sbuf (daml-make-summary-config)))
	(save-excursion
	  (set-buffer sbuf)
	  (abbrev-mode 0) ;; turn off abbrev mode
	  (auto-fill-mode 0) ;; turn off autofill mode
	  (if (fboundp 'buffer-disable-undo)
	      (buffer-disable-undo (current-buffer))
	    ;; obfuscation to make the v19 compiler not whine
	    ;; about obsolete functions.
	    (let ((x 'buffer-flush-undo))
	      (funcall x (current-buffer))))
	  (daml-summary-mode-internal)
	  (daml-display-summary-list list dbuf sbuf)
	  )
	;; back in main buffer
	)
    (daml-refresh-summary)
    ))



