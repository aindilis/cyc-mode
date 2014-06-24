;;; daml-insert.el --- a part of the simple DAML mode for Xemacs

;;; (C) 2001 BBN Technologies
;;; by Mark Burstein, with assistance from Ken Anderson and Richard Shapiro

;;; This package is designed to be used with XEmacs and Gnu Emacs

;;; functionality for inserting new expressions into daml/rdf files
(defun empty-p (x)
  (or (null x) (and (stringp x)(string= x ""))))

;; Insert a tag with an attribute and value along with the closing tag.
(defun daml-insert0 (tag &optional attribute value no-end-tag)
  (unless (empty-p tag)
    (unless (daml-empty-line-p)
      (daml-open-new-line-for-insert)) ;; in daml-motion
    (daml-indent-line)
    ;; if no attribute and value then just do a <tag>
    (let ((et (if no-end-tag "/>" ">")))

    (insert (cond ((and (empty-p attribute) (empty-p value))
		   (format "<%s%s"  tag et))
		  ((and attribute value)
		   (format "<%s %s=\"%s\"%s"  tag attribute value et))))
  (unless no-end-tag
    (insert "\n")
    (daml-indent-line)
    (let ((p (point)))
      (insert (concat "\n</" tag ">"))
      (daml-indent-line)
      (goto-char p))))))


(defun daml-insert-class (name)
  (interactive "sClass Name: ")
  (daml-insert0 "daml:Class" "rdf:ID" name))

;; DER
(defun daml-insert-subClassOf (name)
  (interactive "sParent Class Name: ")
  (daml-insert0 "daml:subClassOf" "rdf:resource" name t))

;; DER
(defun daml-insert-subclass-restriction ()
  (interactive)
  (daml-insert0 "daml:subClassOf")
  (daml-insert0 "daml:Restriction"))

(defun daml-insert-toClass (name)
  (interactive "stoClass Name: ")
  (daml-insert0 "daml:toClass" "rdf:resource" name t))

(defun daml-insert-onProperty (name)
  (interactive "sonProperty Name: ")
  (daml-insert0 "daml:onProperty" "rdf:resource" name t))

(defun daml-insert-hasClass (name)
  (interactive "shasClass Name: ")
  (daml-insert0 "daml:hasClass" "rdf:resource" name t))

(defun daml-insert-hasValue (name)
  (interactive "shasValue Name: ")
  (daml-insert0 "daml:hasValue" "rdf:resource" name t))

(defun daml-insert-oneOf ()
  (interactive)
  (daml-insert0 "daml:oneOf" "rdf:parseType" "daml:collection" nil))

(defun daml-insert-unionOf ()
  (interactive)
  (daml-insert0 "daml:unionOf" "rdf:parseType" "daml:collection" nil))

(defun daml-insert-complementOf (name)
  (interactive "scomplementOf Name: ")
  (daml-insert0 "daml:complementOf" "rdf:resource" name t))

(defun daml-insert-intersectionOf ()
  (interactive)
  (daml-insert0 "daml:intersectionOf" "rdf:parseType" "daml:collection" nil))

(defun daml-insert-disjointUnionOf ()
  (interactive )
  (daml-insert0 "daml:disjointUnionOf" "rdf:parseType" "daml:collection" nil))

;;; property related inserts

(defun daml-insert-property (name)
  (interactive "sProperty Name: ")
  (daml-insert0 "rdf:Property" "rdf:ID" name))

;; DER
(defun daml-insert-object-property (name)
  (interactive "sObjectProperty Name: ")
  (daml-insert0 "daml:ObjectProperty" "rdf:ID" name))

;; DER
(defun daml-insert-datatype-property (name)
  (interactive "sDatatypeProperty Name: ")
  (daml-insert0 "daml:DatatypeProperty" "rdf:ID" name))


(defun daml-insert-subPropertyOf (name)
  (interactive "sParent Property Name: ")
  (daml-insert0 "daml:subPropertyOf" "rdf:resource" name t))

(defun daml-insert-domain (name)
  (interactive "sdomain Class Name: ")
  (daml-insert0 "daml:domain" "rdf:resource" name t))

(defun daml-insert-range (name)
  (interactive "srange Class Name: ")
  (daml-insert0 "daml:range" "rdf:resource" name t))

(defun daml-insert-range-restriction ()
  (interactive)
  (daml-insert0 "daml:range")
  (daml-insert0 "daml:Restriction") 
  )


(defun daml-insert-cardinality-restriction (str val)
  (let (pt
	(etag (daml-goto-tag-begin t)))
    (if (char= (car etag) ?>)(search-backward "<"))
    (when (looking-at "<daml:Restriction")
      (when (search-forward-regexp "/?>" nil nil)
	(goto-char (match-beginning 0))
	(insert (format " %s=\"%s\"" str val))
	))))
    
(defun daml-insert-cardinality (value)
  (interactive "sCardinality Value ")
  (daml-insert-cardinality-restriction "daml:cardinality" value))

(defun daml-insert-minCardinality (value)
  (interactive "sminCardinality Value: ")
  (daml-insert-cardinality-restriction "daml:minCardinality" value))

(defun daml-insert-maxCardinality (value)
  (interactive "smaxCardinality Value: ")
  (daml-insert-cardinality-restriction "daml:maxCardinality" value))



;; descriptions and other things

(defun daml-insert-description-id (name)
  (interactive "sDescription ID: ")
  (daml-insert0 name "rdf:Description" "rdf:ID"))

(defun daml-insert-description-about (name)
  (interactive "sDescription About(URL): ")
  (daml-insert0 "rdf:Description" "rdf:about" name))


;; DER
(defun daml-insert-comment ()
  (interactive)
  (daml-insert0 "rdfs:comment"))

(defun daml-insert-xml-comment ()
  (interactive)
  (unless (daml-empty-line-p)
    (daml-open-new-line-for-insert)) ;; in daml-motion
  (let (p)
    (insert "<!-- ") (setq p (point)) (insert " -->")
    (goto-char p)
    ))


(defun daml-insert-Class-about (name)
  (interactive "stoClass Name: ")
  (daml-insert0 "daml:Class" "rdf:about" name t))


;; DER
;; This assumes that the point is where the insertion is to take place.

(defun daml-insert-rdf-resource ()
  (interactive)
  (let (p
	(etag (daml-goto-tag-begin t)))
    (if (char= (car etag) ?>)
	(search-backward-regexp "/?>")
      (search-forward-regexp "/?>")
      )
    (goto-char (match-beginning 0))
    (insert " rdf:resource=\"") 
    (setq p (point))
    (insert "\"")
    (goto-char p)
    ))



(defun daml-insert-tag (name)
  (interactive "sTag Label:")
  (let ((res (read-input "(cr for none) rdf:resource=")))
    (if (empty-p res) (setq res nil))
;    (dbg "res=%s" res)
    (daml-insert0 name (if res "rdf:resource") res res)))

;;; if inside a begin tag, make do />
;;; else open a line and insert end corresponding to prev unclosed begin.
(defun daml-insert-end-tag ()
  (interactive)
  (let ((curpos (point))
	(beg (daml-goto-tag-begin t))
	tag
	)
;    (dbg "beg=%s point %s" beg (point))
    (cond ((char= (car beg) ?>) ;; we were after a tag
	   (when (and (daml-up-tag)
		      (looking-at "<\\([^/][^ \t\n>]*\\)"))
	     (setq tag (buffer-substring
			(match-beginning 1)
			(match-end 1)))
	     (goto-char curpos)
	     ;; unless at beginning of line or (daml-empty-line-p)
	     (unless (or (looking-at "^.*") (daml-empty-line-p))
	       (insert "\n"))
	     (insert (format "</%s>" tag))
	     ;; DER - only insert line if the tag is not at the end of the line.
	     (unless (looking-at "$")
	       (open-line 1))
	     (daml-indent-line)
	     ;; DER - I don't like being at the beginning of the tag I just inserted.
	     ;; This moves to the end of the tag.
	     (daml-search-fwd ">")(forward-char 1)
	     ))
	  ;; else, in a tag so found its beginning
	  ;;  if in an end tag, go past it and try again
	  ((looking-at "</")
	   (when (daml-search-fwd ">")
	     (forward-char 1)
	     (daml-insert-end-tag)))
	  ;; if a begin tag, see if it has
	  ;; a close bracket, and put slash before it
	  (t 
	   (forward-char 1) ;; past <
	     (setq curpos (point))
	     (daml-search-fwd "[<>]")
	     (backward-char 1)
	     (if (looking-at ">")
		 (insert "/")
	       (goto-char curpos)
	       (end-of-line)
	       (insert "/>"))))))
