;;; daml-fontify.el --- a part of the simple DAML mode for Xemacs

;;; (C) 2001 BBN Technologies
;;; by Mark Burstein, with assistance from Ken Anderson and Richard Shapiro

;;; This package is designed to be used with XEmacs and Gnu Emacs



;;; faces and font-lock match parameters for DAML/RDF code
;;; requires font-lock.el (normally loaded)

(defgroup daml-faces nil "faces used for DAML mode"  :group 'faces)

;;; GNU requires that the face vars be defined and point to themselves
(defvar daml-keyword-face 'daml-keyword-face 
  "Face to use for DAML keywords like Class, Property.")

(defface daml-keyword-face
    (if in-xemacs-p 
	'((((class color)) (:foreground "black"  :bold t))
	  (t (:foreground "gray" :bold t)))
      ;; in GNU, no bold, so just use color
      '((((class color))(:foreground "black"))))
  "Font Lock mode face used to highlight property names."
  :group 'daml-faces)


;;; used for things like rdf: and daml: 
;;; black, but not bold in xemacs, grey in GNU emacs to downplay them
;;; against the kwds.

(defvar daml-normal-face 'daml-normal-face "regular face")
(defface daml-normal-face
  (if in-xemacs-p     
      '((t (:foreground "black")))
    '((t (:foreground "grey"))))
  "Font Lock mode face used to highlight property names."
  :group 'daml-faces)

(defvar daml-string-face 'daml-string-face "string face")
(defface daml-string-face
    '((t (:foreground "green4")))
  "Font Lock mode face used to highlight strings."
  :group 'daml-faces)


(defvar daml-class-face 'daml-class-face
  "Face to use for DAML Class names in class definitions.")

;; same as function name face
(defface daml-class-face
  (if in-xemacs-p   
      '((((class color)) (:foreground "blue" :bold t))
	(t (:bold t)))
    ;; in GNU, just blue
    '((((class color)) (:foreground "blue"))))
  "Font Lock mode face used to highlight class names in class definitions."
  :group 'daml-faces)

(defvar daml-class-ref-face 'daml-class-ref-face
  "Face to use for DAML Class name references.")

(defface daml-class-ref-face
  '((((class color)) (:foreground "blue" ))
    (t (:bold t)))
  "Font Lock mode face used to highlight class refs."
  :group 'daml-faces)

(defvar daml-property-face 'daml-property-face
  "Face to use for DAML property names in property definitions.")

(defface daml-property-face
  (if in-xemacs-p  
     '((((class color)) (:foreground "darkviolet" :bold t))
       (t (:italic t)))
    ;; in gnu, just magenta
    '((((class color)) (:foreground "darkviolet"))))
     "Font Lock mode face used to highlight property names."
     :group 'daml-faces)

(defvar daml-property-ref-face 'daml-property-ref-face
  "Face to use for DAML property name references.")

(defface daml-property-ref-face
  '((((class color)) (:foreground "darkviolet" ))
    (t (:italic t)))
  "Font Lock mode face used to highlight property refs."
  :group 'daml-faces)

(defvar daml-comment-face 'daml-comment-face
  "Face to use for DAML comments.")

(defface daml-comment-face
  '((((class color) ) (:foreground "red" :italic t))
    (t (:foreground "DimGray" :italic t)))
  "Font Lock mode face used to highlight comments."
  :group 'daml-faces)


(defvar daml-other-face 'daml-other-face
  "Face to use for other keywords.")

(defface daml-other-face
  '((((class color)) (:foreground "peru")))
  "Font Lock mode face used to highlight other DAML keyword."
  :group 'daml-faces)

(defvar daml-tag-face 'daml-tag-face
  "Face to use for tags.")

(defface daml-tag-face
    '((((class color)) (:foreground "violetred" ))
      (t (:foreground "black")))
  "Font Lock mode face used to highlight other untyped tags."
  :group 'daml-faces)


(defvar daml-substitution-face 'daml-substitution-face "face to use for substitution strings")

(defface daml-substitution-face
    '((((class color)) (:foreground "orangered"))
      (t (:foreground "lightgrey")))
  "Face to use for DAML substitutions"
  :group 'daml-faces)



;;;================================================================
;;; these are the regexp matches for highlighting DAML 

(defvar daml-font-lock-keywords
  (let ()
    (list 
     ;; black for the def parts of PROPERTY DEFINITION
     ;; and of TransitiveProperty UnambiguousProperty UniqueProperty
     (list 
      (concat "<\\(\\|rdfs?:\\|daml:\\)\\(" 
	      "\\(\\(Object\\|Datatype\\|Transitive\\|Unambiguous\\|Unique\\|Constraint\\|\\)Property\\)"
	      "\\|\\(Class\\)\\)"
	      "\\s-+\\(rdf:\\|\\)\\(ID\\|about\\)\\s-*=\\s-*\""
	      "\\([^\"]*\\)\"")

      '(1 daml-normal-face t)
      '(2 daml-keyword-face t)      
      '(6 daml-normal-face t)
      '(7 daml-keyword-face t)
      (list 8 
	    '(if (match-beginning 5) 
		'daml-class-face ; blue
	      'daml-property-face) ; purple
	    t)
      )

     ;; still need these -  for closing tags and starts w/o attributes
     (list 
      (concat "</?\\(\\|rdfs?:\\|daml:\\)"
	      "\\(Class\\|"
	      "\\(Transitive\\|Unambiguous\\|Unique\\|"
	      "Constraint\\|Object\\|Datatype\\)?Property\\)" )

	   '(1 daml-normal-face t)
	   '(2 daml-keyword-face t))

;;;  rdfs:domain, range, subClassOf take Class as resources     
;;;  plus other daml keywords that take class URI as object 
;;;      toClass disjointWith sameClassAs  hasClass
;;; (by merging regexps, we are lax with rdfs: vs daml: 
;;; on the latter set)

     (list 
      (concat "<\\(\\|rdfs:\\|daml:\\)" ; 1
	      "\\(domain\\|range\\|\\(to\\|has\\)Class\\|"
	      "disjointWith\\|sameClassAs\\|"
	      "\\(subClass\\|complement\\)Of\\)" ; end 2, 5
	      "\\s-+\\(rdf:resource\\)\\s-*=\\s-*\"" ; 6
	      "[^\"#]*\\([^\"]*\\)\""
	      )
      '(1 daml-normal-face t)
      '(2 daml-keyword-face t)
      '(5 daml-normal-face t)
      '(6 daml-class-ref-face t t)
      )

     (list 
      (concat "<\\(\\|daml:\\)" ; 1
	      "\\(hasValue\\)"
	      )
      '(1 daml-normal-face t)
      '(2 daml-keyword-face t)
      )
     (list 
      (concat "<\\(\\|rdfs:\\|daml:\\)"
	      "\\(inverseOf\\|subPropertyOf\\|samePropertyAs\\|onProperty\\)"
	      "\\s-+\\(rdf:resource\\)\\s-*=\\s-*\""
	      "[^\"#]*\\([^\"]*\\)\"")
      '(1 daml-normal-face t)
      '(2 daml-keyword-face t)
      '(3 daml-normal-face t)
      '(4 daml-property-ref-face t)
      )

     ;; ID or about
     '("\\(rdf:\\|\\)\\(about\\|ID\\)[ \t]*=" 
       (1 daml-normal-face t)
       (2 daml-keyword-face t))

     ;;; rdf:Description, type, value with reference or id
     '("</?\\(rdf:\\)\\(Description\\|type\\|value\\)"
       (1 daml-normal-face t)
       (2 daml-keyword-face t))

     ;;; again, for closing
     (list (concat "<\\(\\|/\\)\\(daml:\\|rdfs:\\)"
		   "\\(seeAlso\\|domain\\|range\\|inverseOf\\|"
		   "sub\\(Class\\|Property\\)Of\\|"
		   "\\(\\|to\\|has\\)Class\\|"
		   "disjointWith\\|sameClassAs\\)")
       '(1 daml-keyword-face t)
       '(2 daml-normal-face t)
       '(3 daml-keyword-face t))

     '("<!DOCTYPE" 0 daml-keyword-face t)
;;; in DOCTYPE preface
     '("<\\(!ENTITY\\)[ \t]+\\([^ \t]+\\)[ \t]+\"\\([^\"]*\\)" 
       (1 daml-keyword-face t)
       (2 daml-substitution-face t)
       (3 daml-string-face t) 
       )
     
     ;;  daml keywords that usually stand alone or have general range:
     ;;      Restriction intersectionOF disjointUnionOf equivalentTo oneOf 
     ;;      unionOf complementOf inverseOf Disjoint
     ;; and also [min|max]CardinalityQ?  or cardinality
     (list (concat "</?\\(\\|daml:\\)"
		   "\\(Restriction\\|Disjoint\\|intersectionOf\\|"
		   "disjointUnionOf\\|equivalentTo\\|"
		   "oneOf\\|unionOf\\|complementOf\\|inverseOf\\)")
	   '(1 daml-normal-face t)
	   '(2 daml-keyword-face t)
	   )
     '("\\(rdf:\\)\\(parseType\\)\\s-*=\\s-*\"\\([^\"]*\\)\""
       (1 daml-normal-face t)(2 daml-other-face t)(3 daml-other-face t))

     '("\\(daml:\\)\\(\\(\\|minC\\|maxC\\|c\\)ardinality\\(\\|Q\\)\\)"
       (1 daml-normal-face t) (2 daml-other-face t))

     ;; Large-scale structure keywords 
     ;; for now, just rdf:RDF, daml:Ontology, daml:imports
     '("</?\\(rdf:\\|daml:\\|\\)\\(RDF\\|Ontology\\|versionInfo\\|imports\\)[^>]*>?"
       (1 daml-normal-face t)
       (2 daml-keyword-face t))

     ;; Any tag, general rule, just after bold/italic stuff.
     ;; FOR NOW USE RULE THAT CHAR AFTER : is Capitalized if class, else property

     '("<\\(rdfs:\\)\\(label\\)[^>]*>\\([^<]*\\)</\\(rdfs:\\)\\(label\\)>" 
       (1 daml-normal-face t)
       (2 daml-keyword-face t)
       (3 daml-string-face t)
       (4 daml-normal-face t)
       (5 daml-keyword-face t)
       )

     '("[ \"<>]\\(\\(http\\|file\\|ftp\\|mailto\\):[^ \t\n#>\"]*\\)" 1 daml-tag-face t)

     '("</?\\([A-Za-z0-9_]+:\\|\\)\\([a-z][^ \t\n:>]*[ \t\n>]\\)" 
       (1 daml-normal-face nil)
       (2 daml-property-ref-face nil))

     '("</?\\([A-Za-z0-9_]+:\\|\\)\\([A-Z][^ \t\n:>]*[ \t\n>]\\)" 
       (1 daml-normal-face nil) 
       (2 daml-class-ref-face nil) )

     ;; any other tag
     '("<\\([^!? ][^ \t>]+\\)" 1 daml-tag-face nil) 

     ;; strings are mostly URIs, so just use this for now, except for the above
     '("=[ \t\n]*\"\\([^\"]+\\)\"" 1 daml-string-face nil) ;; was override= t

     ;; within rdf tag, 
     '("\\(xmlns:?\\)\\([^ \t=]*[ \t]*=\\)" 
       (1 daml-normal-face t)
       (2 daml-keyword-face t))
     ;; in definitions
     '("rdf:\\(\\(ID\\|about\\)[ \t]*=\\)" (1 daml-keyword-face t))

     ;; slashes in end tags and empty end tags
     '("<\\(/\\)" 1 daml-keyword-face t)
     '("\\(/\\)>" 1 daml-keyword-face t)
     

     ;; XML Comments: <!-- ... -->. They traditionally override anything else.
     ;; It's complicated 'cause we won't allow "-->" inside a comment, and
     ;; font-lock colours the *longest* possible match of the regexp.

     '("\\(<!--\\([^-]\\|-[^-]\\|--[^>]\\)*-->\\)"
       1 daml-comment-face t)

     ;; things like <?xml ..?> show up as comments
     '("<[?][^?>]*[?]>"    0 daml-comment-face t)

     ;; RDFS Comments - higlight the tag as we can't cross lines very well.
     '("<\\(\\|rdfs:\\)comment>.*\\(\\|^.*\\)*</\\(\\|rdfs:\\)comment>"
       0 daml-comment-face t) ;; red thru line

     '("</?\\(\\|rdfs:\\)comment>"
       0 daml-comment-face t) ;; if on different lines

     '("&[^;]+;" 0 daml-substitution-face t)

     ;; HTML special characters
     '("&[^;\n]*;" 0 daml-string-face nil)


;;; END OF LIST ELTS
     ))
    "Additional expressions to highlight in DAML mode.")



(put 'daml-mode 'font-lock-defaults '(daml-font-lock-keywords nil nil))

(defun re-font-lock () (interactive) (font-lock-mode 0)(font-lock-mode 1))
