;;; daml-motion.el --- a part of the simple DAML mode for Xemacs

;;; (C) 2001 BBN Technologies
;;; by Mark Burstein, with assistance from Ken Anderson and Richard Shapiro

;;; This package is designed to be used with XEmacs and Gnu Emacs




;;; motion commands analogous to lisp structure movements
;;; support C-M-f, C-M-b - forward and back over tags (:: sexpressions)
;;; C-M-e (END), C-M-a (BEGINING) - end and beginning of enclosing tag expression
;;;   (using the same heuristic that tag on left margin (not indented) starts a definition)
;;; C-M-i - (IN) down an embedding level
;;; C-M-u, C-M-d - up and down definitions
;;;                and down goes just after it (or does nothing if no more levels).
;;; <TAB> - DAML-INDENT-LINE
;;; C-M-q - INDENT DEFINITION

;;; forward and backward may cross between embedding levels if this var is T
;;; otherwise, stop when moving forward over an end tag or backward over a 
;;; non-empty (that is, non self-terminating) begin tag.
(defvar *daml-move-cross-levels* nil "If T then fwd, back motions will cross embedding levels.")

(defvar *daml-definition-matcher* nil "If non-nil, then the definition match string")
(make-variable-buffer-local '*daml-definition-matcher*)

(defvar *daml-definition-indent* nil "If non-nil, then the definition indent column.")
(make-variable-buffer-local '*daml-definition-indent*)

(defvar *def-begin-pattern* "<[^!/?]")

(defun daml-definition-matcher ()
  (or *daml-definition-matcher*
      (let ((str *def-begin-pattern*))
	(dotimes (i (daml-definition-indent))
	  (setq str (concat " ?" str)))
	(setq *daml-definition-matcher* (concat "^" str)))))


(defun daml-definition-indent ()
  (or *daml-definition-indent*
      (let ((curpos (point)))
	(beginning-of-buffer)
	(when (and (re-search-forward "<rdf:RDF" nil t)
		   (re-search-forward ">" nil t)))
	(daml-search-fwd "<")
	(backward-char 1)
	(setq *daml-definition-indent* (current-column)))))

;  (daml-def-begin-string *daml-definition-indent*)

(defun daml-beginning-of-def-p ()
  (let ((curpos (point))
	ok)
    (backward-char (daml-definition-indent))
    (setq ok (looking-at (daml-definition-matcher)))
    (goto-char curpos)
    ok))

;;; ignore balanced dbl-quote strings and xml comments if ignore-comment-p
;;; for now don't worry about quoted dblquote inside strings
;;; number of dbl quote chars between begin of line and (point)
(defun dblqts-before (&optional pt)
  (let* ((orig-pos (point))
	(end (or pt orig-pos))
	(cnt 0))
    (beginning-of-line)
    (while (< (point) end)
      (if (char= (char-after) ?\") (incf cnt))
      (forward-char))
    (if (and pt (/= pt orig-pos)) (goto-char orig-pos))
    cnt))

;;; Determine if inside a quoted string. 
;;; Assumes strings on single line only for simplicity. 
;;; Therefore if odd number of quotes before 
;;; on the line, then inside one.
(defun inside-string-p ()
  (oddp (dblqts-before)))

(defvar daml-search-no-error t)

;;; search forward or return DAML-specific error provided
(defun search-forward-or-err (string &optional errstr)
  (let ((pt (point))
	(res (search-forward string nil daml-search-no-error)))
    (if res res
      (error (or errstr (format "Syntax Error: Missing %s after %s" string pt))))))
	
;;; search backward or return DAML-specific error provided
(defun search-backward-or-err (string &optional errstr)
  (let ((pt (point))
	(res (search-backward string nil daml-search-no-error)))
    (if res res
      (error (or errstr (format "Syntax Error: Missing %s before %s" string pt))))))

;;; look forward (backward) to find the closest angle bracket 
;;; find the nearest < or > in direction (bkward or not)
;;; if move is non-nil, then move if found char = move or move=T
;;; if move is a char, then signal an error if not the right one.
;;; return dotted pair of < or > and position of the char.
(defun closest-bracket-pos (&optional bkward move)
  (let* ((curpos (point))
	 (pos (if bkward
		  (search-backward-regexp "[><]" nil t)
		(when (search-forward-regexp "[><]" nil t)
		  (backward-char) ;; if going forward, move before the char
		  (point)) ;; and return the pos
		))
	 (chr (and pos (char-after pos))))
    (cond ((null chr) nil)
	  (t (unless (or (eq move t)(eq move chr))
	       (goto-char curpos))
	     (cons chr pos)))))


;;; search forward for a regexp, skipping quoted strings and balanced xml and daml/rdf comments
;;; skip quoted strings and xml comments. Must start outside an xml comment.
;;; like regular search, put point at the end of the expression when done. 
(defun daml-search-fwd (regexp &optional pt-to-beginning) 
  "Regexp search forward skipping quoted strings and XML, RDF comments."
  (interactive "s")
  (let ((found nil)
	(in-quote (inside-string-p)))
    (while (and (not found) (not (eobp)))
      (let ((char (char-after)))
	(cond ((eobp) nil)
	      (in-quote 
	       (when (search-forward-regexp "[^\\]\"" nil t)
		 (backward-char)
		 (setf in-quote nil)))
	      ((and (char= char ?\") (not (char= (char-before) ?\")))
	       (search-forward-regexp "[^\\]\"" nil t)
	       (backward-char))
	      ((looking-at "<!--")
	       (forward-char 4)
	       (search-forward-regexp "[^-]-->" nil t))
	      ((looking-at "<rdfs:comment>")
	       (search-forward-regexp "</rdfs:comment>" nil t))
	      ((looking-at regexp)
	       (unless pt-to-beginning (search-forward-regexp regexp));; to move point
	       (setq found (point)))))
      (unless found (forward-char)))
    found))
      

;;; search backward for a regexp, skipping quoted strings and balanced xml and daml comments
;;; skip quoted strings and xml comments. Must start outside an xml comment.
;;; start-prev is how many chars before (point) to start looking-at.
(defun daml-search-bkwd (regexp &optional start-prev)
  "Regexp search backward skipping quoted strings and XML, RDF comments."
  (interactive "s")
  (let ((curpos (point)) 
	(found (if (bobp) (error "Beginning of buffer.") nil)))
    ;; backup enough to match forward the first time (should be passed in unless exact match)
    (or start-prev (setf start-prev 1))
    (backward-char start-prev)
    (unwind-protect 
	(let ((in-quote (inside-string-p)))
	  (while (and (not found)(not (bobp)))
	    (let ((char (char-after)))
	      (cond ;; if in-quote ignore char
	       (in-quote (search-backward-or-err "\""))
	       ((looking-at "-->")
		(search-backward-or-err "<!--"))
	       ((looking-at "</rdfs:comment")
		(search-backward-or-err "<rdfs:comment" ))
	       ((looking-at regexp)(setq found (point)))
	       (t
		(backward-char))
	       )))
	    found)
      (unless found 
	(goto-char curpos) 
	found))
      ))
      

;;; read a token, starting at (point), ignoring < and > as well as spaces
;;; return a string, and position point after the end of string
;;; if next token is a string, return with "" around it
;;; optional args: POS is start position, MOVE means stay at end of token when done
;;; default is to return to position before fn was called, then return token
(defun daml-read-token (&optional pos move)
  (let* ((curpos (point))
	 (beg (or pos (point)))
	 (tok nil))
    (goto-char beg)
    (skip-chars-forward " \t\n<=>")
    (setq beg (point))
    (cond ((next-char-is ?\")
	   (forward-char)
	   (let ((str-end (search-forward "\"" nil t)))
	     (if str-end
		 (goto-char str-end)
	       (error "Syntax Error: Missing end dbl-quote?")
	       )))
	  (t 
	   (skip-chars-forward "-0-9A-Za-z:/_#&;@.")
	   ;;	   (dbg (format "%s" (point)))
	     ))
    (unless (eq beg (point)) (setf tok (buffer-substring beg (point))))
    (unless move (goto-char curpos))
    tok
    ))


;;; if inside a comment, skip to before start of it (or after end if MOVE='end)
;;; also go to beginning if AFTER the comment when MOVE='or-after
(defun daml-beginning-of-comment-if-inside (&optional move)
  (let ((curpos (point))
	curpos2 cpos com in-wd)
    (if (looking-at "[^ \t\n]")  ;; get to a separator
	(search-forward-regexp "[ \t\n]" nil t))
    (if (not (= curpos (point))) (setf curpos2 (point)))
    (setf cpos 
	  (search-backward-regexp "\\(</?rdfs:comment>?\\|<!--\\|-->\\)"
				  nil t))
    (setf in-wd (and curpos2 cpos 
		     (< (- curpos2 cpos)
			(- (match-end 1)(match-beginning 1)))))

    (cond (cpos 
	   (setf com (buffer-substring (match-beginning 1)(match-end 1)))
 ;     (dbg "cpos=%s" com)
	   (cond 
	    ((string-match "<rdfs:comment" com)
	     (goto-char cpos) ;; redundant, but the right position
	     (if (eq move 'end) 
		 (setq cpos (search-forward-or-err "</rdfs:comment>")))
	     cpos)
	    ((string-equal com "<!--")
	     (goto-char cpos)
	     (if (eq move 'end) (setq cpos (search-forward-or-err "-->")))
	     cpos)
	    ((and (string-equal com "-->") in-wd)
	     (setq cpos 
		   (if (eq move 'end) 
		       (search-forward-or-err "-->")
		     (search-backward-or-err "<!--"))))
	    ((and (string-match "</rdfs:comment" com) in-wd)
	     (setq cpos 
		   (if (eq move 'end)
		       (search-forward-or-err "</rdfs:comment>")
		       (search-backward-or-err "<rdfs:comment"))))
	    ((and (eq move 'or-after)
		  (string-match "</rdfs:comment" com))
	     (search-backward-or-err "<rdfs:comment"))
	    ((and (eq move 'or-after)
		  (string-match "-->" com))
	     (search-backward-or-err "<!--"))
	    (t (goto-char curpos)
	       nil)))
	  (t (goto-char curpos) nil))
    ))
	

;;; if inside a tag back up to begining.
;;; else go to next following <
;;; if no following < then return NIL

;;; if backup-if-outside is T, then go to prev > if between tags
;;; if move, return pair (chr . pos) where chr is first prior > (if backup)
;;; or < (if inside or after and (not backup))
;;; and pos is final buffer position
;;; else nil if not backup and no > found
;;; IF BACKING UP and OUTSIDE, then final pos is AFTER >
(defun daml-goto-tag-begin (&optional backup-if-outside)
  (interactive)
  (let* ((curpos (point))
	 pos
	 (etag (closest-bracket-pos t t))) ;; look BACKWARD for <> and move before it
;    (dbg "in goto tag begin (%s) etag %s" backup-if-outside etag)
    (cond ((null etag) 
	   (cond ((not backup-if-outside)
		 ;; if no previous. look for next pos or dont move
		  (setq etag (closest-bracket-pos nil ?<))  
		  (cond ((null etag) (goto-char curpos) nil)
			;; if find another < instead, then have an embedded tag problem.
		       ((char= (car etag) ?>)
			(message "Syntax Error: Missing > near position %s (looing at '%s')" 
				 (cdr etag)
				 (buffer-substring (cdr etag)(+ 10 (cdr etag))))
			nil)
		       ;; else we should be sitting at the next tag <
		       (t etag)))
		 (t ;; nowhere to backup to
		  nil)))

	  ;; if found current tag begin, and now looking at it, just return etag
	  ((char= (car etag) ?<) etag)
	  ;; if > is behind then back to < if backup 
	  ;; or else go forward to <
	  (t ; (char= (car etag) ?>) - must be this
	   (forward-char) ;; get outside the tag
	   (if backup-if-outside
	       (cons ?> (point))
	     ;; if not backing up, then find next tag beginning
	     (setq etag (closest-bracket-pos nil ?<))
;	     (dbg "next bracket pos etag %s" etag)
	     (if (char= (car etag) ?<) etag
	  ;; else back was > fwd was > and we're in trouble
	       (message "Syntax Error:  -couldn't find tag begin at position %s" (point))
	       (goto-char curpos) 
	       nil)))
     ;; should never get here
     (t 
	  (message "Warning -couldn't find tag begin at position %s" (point))
	  (goto-char curpos) nil))))

;(define-key daml-mode-map (keystroke '((control c) ?b)) 'daml-goto-tag-begin)

;;;;;;;;;;;;; DAML-MOVE-FORWARD, BACKWARD
;;; move forward n tags (essentially n '>'s), but at a level of nexting
;;; that is:
;;; if before or within a begin tag, go to end of the corresponding end tag
;;; (if empty end, just go after />)

;;;(maybe not do this)
;;; if before or within an end tag, go past the end tag. 

;;; if within a comment, go past the end of it. 
;;;   this includes (either <!-- comment --> or <comment> </comment>)
;;; 


;(search-forward-regexp "<\\(\w[\w:-_]*\\)"))

;;; DAML-FORWARD-TAG  (c-m-f)
;;; move to end of current tag or over a complete (balanced) tag pair
;;; prefix arg N is number of tag pairs to skip

;;; if in a tag, go to end of end tag
;;; if before a begin tag go to end of matching end
;;; if before an end, then dont move unless *daml-move-cross-levels*
;;; as this is a level change. 
;;; should return position if moved?
(defun daml-forward-tag (&optional n) 
  "Move forward to the end of the current tag, or over a balanced tag if between tags."
  (interactive "_p")
  (if (null n) (setq n 1))
  (let ((curpos (point)))
    (condition-case err
	(let ((cp2 (point)))
	  (daml-beginning-of-comment-if-inside) ;; else dont move
	  (while (> n 0)
	    (decf n)
	    ;; if inside a tag (begin or end), go to front of it, else next <
	    (when (daml-goto-tag-begin)  
	      (cond ((looking-at "<!--") 
		     (search-forward-or-err "-->"))
		    ((looking-at "<rdfs:comment>")
		     (search-forward-or-err "</rdfs:comment>"))
		    ;; at an end tag, just go after it 
		    ;; - UNLESS PROHIBITED BY *DAML-MOVE-CROSS-LEVELS*
		    ;; which is now normally true. 
		    ((looking-at "</")
		     (if *daml-move-cross-levels*
			 (search-forward-or-err ">")
		       (goto-char cp2);; just stay put where we were. 
		       nil))
		    ;; else should be a begin tag, need to find end
		    ((looking-at "<[?!]")
		     (daml-skip-xml-tag-group))

		    (t 
;		     (dbg "in fwd tag bf skip pt=%s\n"  (point))
		     (daml-skip-tag-group)))))
	      (if (eq (point) curpos) nil (point))
	      )
      (error ;; if an error, 
;       (dbg "error %s" err)
       (cond ((not daml-search-no-error) (error (format "%s" err)))
	     (t 
	      (goto-char curpos)
	      (message (format "%s" err))
;;;       (error (second err))
	      nil)))
      )))


;;; Tag begins after point. Find corresponding end tag and go after it
;;; Assumes we are not in a comment here.
(defun daml-skip-tag-group (&optional tag)
  (unless tag (setf tag (daml-read-token)))
  (let* ((curpos (point))
	 (tagbe (search-forward-or-err ">" ))
	 (tag-regexp (concat "</?" tag "[ \t\n>]")) ;; match <{/}tag{separator or >}
	 tage
	 )
;;;    (trace-msg "at char %s\n" (point))
    (cond ((null tagbe) (error "Missing > for tag %s" tag))
	  ((char= (char-before (1- (point))) ?/) ;; empty end tag
	   (point)) ;;  just stay after />
	  (t (daml-find-matching-tag-end tag-regexp)))))

	  
;;; skip to end of matching >  -- this is conveniently done by forward-list because <> are seen like ()
(defun daml-skip-xml-tag-group ()
  (forward-list))


;;; starting from front of a begin tag, find corresponding full end tag 
;;; regexp is the tag to search for

(defun daml-find-matching-tag-end (regexp)
  (let ((cnt 1) (matchpt t)) 
    (while (and (> cnt 0) matchpt)
      (setf matchpt (daml-search-fwd regexp t)) ;; set point at beginning of match
      ;; no match is an error (unbalanced tags)
      (cond ((null matchpt) (error "Couldn't find tag %s" regexp))
	    ((looking-at "</") ;; found end tag, go after it
	     (decf cnt)
	     (setf matchpt (search-forward-or-err ">"))
	     )
	     ;; a begin tag, go after it, and down a level unless empty end
	    ((setf matchpt (search-forward-or-err ">"))
	     (when matchpt
	       (unless (and (numberp matchpt)
			    (char= (char-before (1- matchpt)) ?/))
		 (incf cnt))))))
    matchpt))


;;; match to chars before point. e.g if abc^def where (point)=^ then 
;;; (looking-back-at "abc") is true
;;; (exact match for now).
(defun looking-back-at (chars)
  (let ((curpos (point))
	(len (length chars)))
    (when (and (plusp len)(<= len curpos))
      (backward-char len)
      (prog1 (looking-at chars)
	(goto-char curpos)))))


;;; DAML-BACKWARD-TAG (c-m-b)
;;; if within begin tag, go to beginning of the current begin tag
;;; if within end tag or after one, skip back to before corresponding begin tag
;;; Don't move back over a begin from directly after if
;;; and *daml-move-cross-levels* is NIL (handled in DAML-GOTO-TAG-BEGIN)
;;; if within or after comment, skip to before comment

;;; function returns new position or nil if no movement 
;;; (though I'm not totally sure I'm handling all errors correctly yet. )
(defun daml-backward-tag (&optional n)
  "If within a tag, move before it. If between tags at an embedding level, 
move prior to begin tag of surrounding tag layer."
  (interactive "_p")
  (if (null n) (setq n 1))
  (let ((curpos (point)) okpos)
    (condition-case err
	(progn 
	  (while (> n 0) 
	    (setq okpos (point)) ;; start of each iteration
	    (decf n)
	    (let ((empty-endp nil)
		  (newpos nil)
		  back) 
	      ;; if inside a tag (begin or end or comment), go to the <
	      ;; if after tag (daml-goto-tag-begin 'back) goes directly AFTER the preceding > (if any)
	      ;; daml-goto-tag-begin returns (char . pos) if moved where char is ?> or ?<
	      (when (setf back (daml-goto-tag-begin 'back))
;		(dbg "after tag-begin back= %s looking-at %s pt= %s" back (bs 5) (point))
		(when (and back (eq (car back) ?>))
		  ;; between tags, so backed up to just after a >
		  ;; jump to beginning of tag, unless jumping over a non-empty begin tag
		  ;; (that is, a begin not terminated by />) 
		  ;; which would mean changing levels (only do it if *DAML-MOVE-CROSS-LEVELS*)
		  (cond ((looking-back-at "-->")
			 (setq newpos (search-backward "<!--" nil t)))
			((looking-back-at "/>")
			 (setf empty-endp t)
;;			 (message "empty-end-tag")
			 (setq newpos (daml-search-bkwd "<")))
			;; else a regular tag or comment end
			(t (setq newpos (daml-search-bkwd "<" ))))
		  (setq back (if newpos (cons ?< newpos))))

;		(dbg "after tag-back looking at: %s emptyend? %s back=%s" (bs 10) empty-endp back)
	      ;;; should be at a tag beginning (looking at <)
		(cond ((null back) (error "No prior tag"))
		      ((looking-at "<!--")) ; at begin of comment
		      ;; in or after end tag of comment, jump to beginning
		      ((looking-at "</rdfs:comment>") 
		       (search-backward "<rdfs:comment" nil t))
		      ;; in or after begin tag of comment, jump before it
		      ((looking-at "<rdfs:comment>") nil)
		      ;; at end tag, find corresponding begin tag
		      ((looking-at "</") (daml-skip-back-tag-group))
		      ;; else some other begin tag. (looking-at "<[^/]")
		      ;; stay in front of it if an empty begin, 
		      ;; else go back to original location unless *daml-move-cross-levels*
;		      ((progn (dbg "past </ test") nil))
		      ((or empty-endp 
			   *daml-move-cross-levels*
			   (not newpos))
		       nil) ;; ok to go here if empty end or inside begin tag
		      ;; else head of a regular begin tag - don't go there and cross levels
		      (t ;; if started after the whole tag ;; back to iteration start point
		        (goto-char okpos))
		      ))))
	  ;; return a value if moved from original position
	  (if (eq curpos (point)) nil (point)) 
	  )
      (error 
       ;; on an error, single bkwd moves considered ok (when n > 1)
       (goto-char okpos)
       (message (format "Error %s"  err))
       (if (eq okpos curpos) nil okpos) 
;;;       (error (second err)))
       ))))

;;; End Tag begins after point. Find corresponding begin tag and go before it
;;; Assumes we are not in a comment here.

(defun daml-skip-back-tag-group (&optional tag)
  (unless tag (setf tag (daml-read-token)))
  ;; strip off the /
  (if (char= (elt tag 0) ?/)
      (setf tag (substring tag 1 nil)))
  (let* ((curpos (point))
	 (tag-regexp (concat "</?" tag "[ \t\n>]")) ;; match <{/}tag{separator or >}
	 )
    (daml-find-matching-tag-begin tag-regexp)))

(defun daml-in-empty-begin-tag-p ()
  (let ((pt (point)))
    (when (search-forward-or-err ">")
      (backward-char 2)
      (prog1 (looking-at "/")
	(goto-char pt)))))

;;; starting from front of a end tag, find corresponding prev begin tag 
;;; regexp is the tag to search for
(defun daml-find-matching-tag-begin (regexp)
  (let ((cnt 1) (matchpt t)) 
    (while (and (> cnt 0) matchpt)
      (setf matchpt (daml-search-bkwd regexp))
      ;; no match is an error (unbalanced tags)
      (cond ((null matchpt) (error "Couldn't find matching begin tag %s" regexp))
	    ((looking-at "</") ;; found another end tag, add a layer
	     (incf cnt)
	     )
	     ;; a matching begin tag, decrement unless empty end
	    (t 
	     (unless (daml-in-empty-begin-tag-p)
		 (decf cnt)))))
    matchpt))




;;; UP-TAG (c-m-u)
;;; move before the enclosing <TAG>
;;; if inside a tag group or its end tag, go before the enclosing begin tag
;;; if inside a begin tag, this is treated as same as before the begin tag, 
;;; so will move up a level from there. c-m-b will go to the tag begining only.
(defun daml-up-tag () 
  "Move up an embedding level to before the enclosing begin <TAG>."
  (interactive "_") 
  ;; make sure backward-tag doesn't cross levels, then go as far as it can.
  (let ((*daml-move-cross-levels* nil)
	(curpos (point))) ;; should be its default value anyway
    (while (daml-backward-tag));; find begining of this level
    (or (daml-search-bkwd "<[^/]")
	(progn
	  (goto-char curpos)
	  nil))))
    
;;; UP-TAG-END 
;;; move past the enclosing end </TAG>
(defun daml-up-tag-end ()
  "Move up an embedding level to past the enclosing end </TAG>."
  (interactive "_")
  ;; make sure backward-tag doesn't cross levels, then go as far as it can.
  (let ((*daml-move-cross-levels* nil)
	(curpos (point))) ;; should be its default value anyway
    (while (daml-forward-tag));; find end of this level
    (or (daml-search-fwd "</[^>]*>") ;; go after next end tag
	(progn
	  (goto-char curpos)
	  nil))))


;;; return pos and move after tag if tag currently looked at is a non-empty begin tag
(defun daml-move-after-begin-tag ()
  (let ((curpos (point)))
    (when (looking-at "<[^/]")
      (if (and (daml-search-fwd ">")
	       (not (looking-back-at "/>")))
	  (point)
	(goto-char curpos)
	nil))))

;;; INSIDE-TAG (c-m-i)
;;; find the first tag with content at current tag embedding level and move inside it.
;;; search forward at current tag level for a non-empty begin tag
;;; if before or inside a tag begin, go after it and stop.
;;; else stay put
(defun daml-inside-tag () 
  "Find the first tag with content at current tag embedding level and move inside it."
  (interactive "_") 
  (let ((*daml-move-cross-levels* nil) ;; should be its default value anyway
	(curpos (point))
	(downpos nil))
    (when (daml-goto-tag-begin)
      ;; now check if a non-empty begin, or else repeat by trying to go forward tag 
      (while (and (not (setq downpos (daml-move-after-begin-tag)))
		  (daml-forward-tag)))
      (if downpos downpos
	  (goto-char curpos)
	  nil)
      )))
	

(defun daml-prev-tag-def ()
  "Move to before the current definition, as approximated by an unindented begin tag.
If already at a definition start, move before the previous one, if any."
  (interactive "_") 
  (let ((curpos (point))
	(atbeg (daml-goto-tag-begin)))
    (cond ((null atbeg) ;; at or near eof, so no following open bracket
	   (if (re-search-backward "<" nil t)
	       (cond ((looking-at "</rdf:RDF>") (daml-backward-tag))
		     ((looking-at "</")
		      (daml-search-forward ">")(daml-backward-tag)))
	     (goto-char curpos)))
	  ((looking-at "<rdf:RDF>")(daml-backward-tag))
	  ;; moved up to next tag, so back up one if begin
	  ((and (< (point) curpos)
		(daml-beginning-of-def-p)))
	  ;; else moved to 
	  (t 
	   (if (= curpos (point))(daml-backward-tag))
	   (while (not (or (bobp) 
			   (looking-at "[ /t]*<rdf:RDF")
			   (daml-beginning-of-def-p)))
	     (if (looking-at "[ /t]*<[!?]")
		 (daml-backward-tag)
	       (daml-up-tag))
	     )))
    (point)
    ))

;; returns position of next tag if found one
(defun daml-next-tag-def () 
  "Move to before the next definition, as approximated by an unindented begin tag."
  (interactive "_")
  (let ((curpos (point))
	res atbegin)
    ;; get to next tag beginning
    (when (setq atbegin (daml-goto-tag-begin))
      (cond ((<= (point) curpos) 
	     (daml-forward-tag)
	     (setq atbegin (daml-goto-tag-begin)))))	    
    (cond ((daml-beginning-of-def-p)
	   (while (not (or (eobp)
			   (null atbegin) 
			   (looking-at "</rdf:RDF")
			   (daml-beginning-of-def-p)))
	     (daml-forward-tag)
	     (setq atbegin (daml-goto-tag-begin))
	     ))
	  (t ;;; inside a body, front of some tag or end tag
	   (while (not (or (eobp) 
			   (null atbegin)
			   (looking-at "</rdf:RDF")
			   (daml-beginning-of-def-p)))
	     (if (looking-at "</")
		 (daml-up-tag-end)
	       (daml-forward-tag))
	     (setq atbegin (daml-goto-tag-begin)))))
    (point)))

;;; find the first non-whitespace char on line and go there
(defun daml-non-white-line-pos ()
  (beginning-of-line)
  (let ((endline (progn (end-of-line) (point))) ;; to bound search-forward-regexp
	(startpos (progn (beginning-of-line) (point))))
    (if (search-forward-regexp "[^\ \t]" endline t)
	(backward-char)) ;;else stay at begining of line
    (point)
    ))

  
(defvar daml-indent-increment 2 "Number of spaces to indent an RDF subexpression.")

;;; INDENT-TAG-COLUMN
;;; Determine the correct column to indent to.
;;; if anything but an end tag on the line, then indent n in from the enclosing begin tag
;;; if an end tag on the line, indent to match the enclosing tag above.
(defun daml-indent-tag-column ()
  "Find column to indent to for a line of DAML code, with respect to the previous tag beginning."  
  (let* ((curpos (point))
	 (samelev t)
	 refcol)
;    (dbg (bs 6))
    (setq refcol
      (cond 
       ((looking-at "[ \t]*$") ;; clear to end of line - so a blank line
	(let ((beg (daml-goto-tag-begin 'back)))
	  ;; skip over preceding comments
	  (while (and beg
		      (or (and (looking-back-at "-->")	    
			       (daml-search-bkwd "<!--"))
			  (and (looking-back-at "</rdfs:comment>")
			       (re-search-backward "<rdfs:comment>" nil t))))
	    (setq beg (daml-goto-tag-begin 'back))
	    )
;	  (dbg "beg=%S pt=%s" beg (point))
	  (cond ((null beg) (beginning-of-line))
		((char= (car beg) ?>) ;; found a prior tag end
		  ;; if a comment, skip over it 
		 (cond
		  ((looking-back-at "/>")
		   (daml-search-bkwd "<")
		   (current-column))
		  (t 
		   (daml-search-bkwd "<")
		   (setq samelev (not (looking-at "<[^/]\\|rdf:RDF")))
		   (current-column))))
		(t ;; at the begining of current tag we're inside, so indent
		 (setq samelev nil)
		 (current-column)))))
       ((looking-at "<!--") 0)
       ((looking-at "</\\(\\(rdfs:\\|\\)comment\\)")
	(re-search-backward 
	 (format "<%s" 
		 (buffer-substring (match-beginning 1)(match-end 1)))
	 nil t)
	(current-column))
       ((looking-at "</") (daml-up-tag) (current-column))
       ((looking-at "<") 
	(cond ((daml-backward-tag) (current-column))
	      ((daml-up-tag)
	       (setq samelev
		 (if (looking-at "<rdf:RDF") t nil))
	       (current-column))
	      (t 0)))
       (t ;; in a tag
;	(dbg "in a tag pos=%s %s" (point) (buffer-substring (point) (+ 5 (point))))
	(setq samelev nil) (daml-goto-tag-begin) (current-column))))
    ;;; now we're ready to say the column
    (if samelev refcol (+ refcol daml-indent-increment))))


;;; INDENT-LINE
;;; if anything but an end tag on the line, then indent n in from the enclosing begin tag
;;; if an end tag on the line, indent to match the enclosing tag above.
(defun daml-indent-line ()
  "Indent a line of DAML code, with respect to the previous tag beginning."  
  (interactive)
  (let* ((textpos (daml-non-white-line-pos))
;	 (textcol (current-column))
	 (samelev t)
	 (refcol (daml-indent-tag-column)))
;    (dbg (bs 6))
    ;;; now we're ready to change the line spacing
    (goto-char textpos)
;    (dbg "refcol %s samelev %s" refcol samelev)
    (indent-line-to refcol)))
      
;;; determine if on a line with nothing but whitespace
(defun daml-empty-line-p ()
  (let ((curpos (point)))
    (beginning-of-line)
    (prog1 (looking-at "[ \t]*$")
      (goto-char curpos))))

;;; try to be smart about opening a line in a good place for a new tag 
(defun daml-open-new-line-for-insert ()
  (let ((curpos (point)))
    (cond ((looking-at "[ \t]*$") ;; near the end
	   (end-of-line)
	   (newline))
	  ((looking-at "[ \t]*<") ;; before a tag
	   (newline 2)
	   (daml-indent-line)
	   (previous-line 1)
;	   (daml-indent-line)
	   )
	  (t ;; in a tag?
	   (daml-goto-tag-begin)
	   (newline 2)
	   (daml-indent-line)
	   (previous-line 1)
	   ))))


(defun daml-newline ()
  "Do a newline at point, then indent for DAML readability."
  (interactive)
  (newline)
  (daml-indent-line))



;;; INDENT-EXPRESSION
(defun daml-indent-expression ()
  "Indent each line of the tag expression that begins on the current line."
  (interactive)
  (beginning-of-line)
  (let* ((curpos (point))
	 (textpos (daml-non-white-line-pos))
	 (numlines (count-lines curpos (progn (daml-forward-tag) (point)))))

    (goto-char curpos)
    (dotimes (i (1- numlines))
      (next-line 1)
      (daml-indent-line)
;      (dbg "looking at %s" (bs 10))
      )
    (goto-char curpos)
    (daml-non-white-line-pos)
    ))


;;; for testing
; (defun xx ()(interactive) (daml-search-bkwd "</?rdf:Property"))

