;; cyc-api functions

(defun constant-p (object)                                                                             
  "Return T iff the argument is a CycL constant                                                               
Single value returned satisfies BOOLEANP."
  )

(defun constant-external-id (constant)                                                                 
  "Return the external id of CONSTANT.                                                                        
CONSTANT must satisfy CONSTANT-P.                                                                          
Single value returned satisfies CONSTANT-EXTERNAL-ID-P or is NIL."
  )

(defun constant-internal-id (constant)                                                                 
  "Return the internal id of CONSTANT.                                                                        
CONSTANT must satisfy CONSTANT-P.                                                                          
Single value returned satisfies CONSTANT-INTERNAL-ID-P or is NIL."
  )

(defun constant-name (constant)                                                                        
  "Return the name of CONSTANT or :unnamed.                                                                   
CONSTANT must satisfy CONSTANT-P."
  )

(defun find-constant (name)                                                                            
  "Return the constant with NAME, or NIL if not present.                                                      
NAME must satisfy STRINGP.                                                                                 
Single value returned satisfies CONSTANT-P or is NIL."
  )

(defun find-constant-by-external-id (external-id)                                                      
  "Return the constant with EXTERNAL-ID, or NIL if not present.                                               
EXTERNAL-ID must satisfy CONSTANT-EXTERNAL-ID-P.                                                           
Single value returned satisfies CONSTANT-P or is NIL."
  )

(defun find-constant-by-internal-id (id)                                                               
  "Return the constant with internal ID, or NIL if not present.                                               
ID must satisfy CONSTANT-INTERNAL-ID-P.                                                                    
Single value returned satisfies CONSTANT-P or is NIL."
  )

(defun create-constant (name &optional external-id)                                                    
  "Return a new constant named NAME. Use EXTERNAL-ID if non-null, else create a new ID.                       
Single value returned satisfies CONSTANT-P."
  )

(defun find-or-create-constant (name &optional external-id)                                            
  "Return the constant with NAME if it exists, otherwise create it with EXTERNAL-ID.                          
Also, if it exists but has a null id, install EXTERNAL-ID on the constant.                                 
NAME must satisfy STRINGP.                                                                                 
Single value returned satisfies CONSTANT-P."
  )

(defun rename-constant (constant new-name)                                                             
  "Rename CONSTANT to have NEW-NAME as its name. The constant is returned.                                    
CONSTANT must satisfy CONSTANT-P.                                                                          
NEW-NAME must satisfy STRINGP.                                                                             
Single value returned satisfies CONSTANT-P."
  )

(defun uniquify-constant-name (name)                                                                   
  "Return a unique, currently unused constant name based on NAME, which must be a valid constant name.        
NAME must satisfy STRINGP.                                                                                 
Single value returned satisfies STRINGP."
  )

(defun remove-constant (constant)                                                                      
  "Remove constant from the KB.                                                                               
CONSTANT must satisfy CONSTANT-P.                                                                          
Single value returned satisfies NULL."
  )

(defmacro do-constants ((var &optional (message "mapping Cyc constants")) &body body)                        
    "Iterate over all HL constant datastructures, executing BODY within the scope of VAR.                       
VAR is bound to each constant in turn.                                                                     
MESSAGE is a progress message string."
    )

(defun constant-count ()                                                                               
  "Return the total number of constants.                                                                      
Single value returned satisfies INTEGERP."
  )

(defun valid-constant-name-char (char)                                                                 
  "Return T iff CHAR is a character which is allowed in a valid constant name.                                
CHAR must satisfy CHARACTERP.                                                                              
Single value returned satisfies BOOLEANP."
  )

(defun valid-constant-name (string)                                                                    
  "Return T iff STRING is a valid name for a constant.                                                        
Single value returned satisfies BOOLEANP."
  )

(defun constant-complete-exact (string &optional start end)                                            
  "Return a valid constant whose name exactly matches STRING.                                                 
Optionally the START and END character positions can be                                                    
specified, such that the STRING matches characters between the START and                                   
END range. If no constant exists, return NIL.                                                              
STRING must satisfy STRINGP.                                                                               
START must satisfy FIXNUMP."
  )

(defun constant-complete (prefix &optional case-sensitive? exact-length? start end)                    
  "Return all valid constants with PREFIX as a prefix of their name                                           
When CASE-SENSITIVE? is non-nil, the comparison is done in a case-sensitive fashion.                       
When EXACT-LENGTH? is non-nil, the prefix must be the entire string                                        
Optionally the START and END character positions can be                                                    
specified, such that the PREFIX matches characters between the START and                                   
END range. If no constant exists, return NIL.                                                              
PREFIX must satisfy STRINGP.                                                                               
CASE-SENSITIVE? must satisfy BOOLEANP.                                                                     
EXACT-LENGTH? must satisfy BOOLEANP.                                                                       
START must satisfy FIXNUMP."
  )

(defun constant-apropos (substring &optional case-sensitive? start end)                                
  "Return all valid constants with SUBSTRING somewhere in their name                                          
When CASE-SENSITIVE? is non-nil, the comparison is done in a case-sensitive fashion.                       
Optionally the START and END character positions can be                                                    
specified, such that the SUBSTRING matches characters between the START and                                
END range. If no constant exists, return NIL.                                                              
SUBSTRING must satisfy STRINGP.                                                                            
CASE-SENSITIVE? must satisfy BOOLEANP.                                                                     
START must satisfy FIXNUMP."
  )

(defun nart-p (object)                                                                                 
  "Return T iff OBJECT is a datastructure implementing a non-atomic reified term (NART).                      
Single value returned satisfies BOOLEANP."
  )

(defun nart-id (nart)                                                                                  
  "Return the id of this NART.                                                                                
NART must satisfy NART-P.                                                                                  
Single value returned satisfies INTEGERP."
  )

(defun find-nart-by-id (id)                                                                            
  "Return the NART with ID, or NIL if not present.                                                            
ID must satisfy INTEGERP.                                                                                  
Single value returned satisfies NART-P or is NIL."
  )
  
(defun nart-hl-formula (nart)                                                                          
  "Return the hl formula of this NART.                                                                        
NART must satisfy NART-P.                                                                                  
Single value returned satisfies CONSP or is NIL."
  )

(defun nart-el-formula (nart)                                                                          
  "Return the el formula of this NART.                                                                        
NART must satisfy NART-P.                                                                                  
Single value returned satisfies CONSP or is NIL."
  )

(defun remove-nart (nart)                                                                              
  "Remove NART from the KB.                                                                                   
NART must satisfy NART-P.                                                                                  
Single value returned satisfies NULL."
  )

(defun nart-count ()                                                                                   
  "Return the total number of NARTs.                                                                          
Single value returned satisfies INTEGERP."
  )

(defmacro do-narts ((var &optional (message "mapping Cyc NARTs")) &body body)                                
    "Iterate over all HL NART datastructures, executing BODY within the scope of VAR.                           
VAR is bound to the NART.                                                                                  
MESSAGE is a progress message string."
    )

(defun fort-p (object)                                                                                 
  "Return T iff OBJECT is a first order reified term (FORT).                                                  
Single value returned satisfies BOOLEANP."
  )

(defun el-fort-p (object)                                                                              
  "Returns t iff OBJECT is a fort or an EL formula.                                                           
Single value returned satisfies BOOLEANP."
  )

(defun fort-el-formula (fort)                                                                          
  "Return the EL formula for any FORT.                                                                        
FORT must satisfy FORT-P.                                                                                  
Single value returned satisfies CONSP or is NIL."
  )

(defun remove-fort (fort)                                                                              
  "Remove FORT from the KB.                                                                                   
FORT must satisfy FORT-P.                                                                                  
Single value returned satisfies NULL."
  )

(defun fort-count ()                                                                                   
  "Return the total number of FORTs.                                                                          
Single value returned satisfies INTEGERP."
  )

(defmacro do-forts ((var &optional (message "mapping Cyc FORTs")) &body body)                                
    "Iterate over all HL FORT datastructures, executing BODY within the scope of VAR.                           
VAR is bound to the FORT.                                                                                  
MESSAGE is a progress message string."
    )

(defun assertion-p (object)                                                                            
  "Return T iff OBJECT is an HL assertion                                                                     
Single value returned satisfies BOOLEANP."
  )

(defun assertion-id (assertion)                                                                        
  "Return the id of this ASSERTION.                                                                           
ASSERTION must satisfy ASSERTION-P.                                                                        
Single value returned satisfies INTEGERP."
  )

(defun assertion-cnf (assertion)                                                                       
  "Return the cnf of ASSERTION.                                                                               
@note If you know the assertion is a gaf,                                                                  
consider using gaf-formula instead,                                                                        
if you do not explicitly need a CNF.                                                                       
ASSERTION must satisfy ASSERTION-P.                                                                        
Single value returned satisfies CNF-P."
  )

(defun assertion-formula (assertion)                                                                   
  "Return a formula for ASSERTION.                                                                            
ASSERTION must satisfy ASSERTION-P.                                                                        
Single value returned satisfies EL-FORMULA-P."
  )

(defun assertion-ist-formula (assertion)                                                               
  "Return a formula in #$ist format for ASSERTION.                                                            
ASSERTION must satisfy ASSERTION-P.                                                                        
Single value returned satisfies EL-FORMULA-P."
  )

(defun assertion-mentions-term (assertion term)                                                        
  "Return T iff ASSERTION's formula or mt contains TERM.                                                      
If assertion is a meta-assertion, recurse down sub-assertions.                                             
By convention, negated gafs do not necessarily mention the term #$not.                                     
ASSERTION must satisfy ASSERTION-P.                                                                        
TERM must satisfy HL-TERM-P.                                                                               
Single value returned satisfies BOOLEANP."
  )

(defun assertion-mt (assertion)                                                                        
  "Return the mt of ASSERTION.                                                                                
ASSERTION must satisfy ASSERTION-P.                                                                        
Single value returned satisfies FORT-P."
  )

(defun assertion-direction (assertion)                                                                 
  "Return the direction of ASSERTION (either :backward, :forward or :code).                                   
ASSERTION must satisfy ASSERTION-P.                                                                        
Single value returned satisfies DIRECTION-P."
  )

(defun forward-assertion? (assertion)                                                                  
  "Predicate returns T iff ASSERTION's direction is :FORWARD.                                                 
Single value returned satisfies BOOLEANP."
  )

(defun backward-assertion? (assertion)                                                                 
  "Predicate returns T iff ASSERTION's direction is :BACKWARD.                                                
Single value returned satisfies BOOLEANP."
  )

(defun direction-p (object)                                                                            
  "Return T iff OBJECT is a valid assertion inference direction                                               
:backward :forward or :code.                                                                               
Single value returned satisfies BOOLEANP."
  )

(defun code-assertion? (assertion)                                                                     
  "Predicate returns T iff ASSERTION's direction is :CODE.                                                    
Single value returned satisfies BOOLEANP."
  )

(defun assertion-truth (assertion)                                                                     
  "Return the current truth of ASSERTION -- either :true :false or :unknown.                                  
ASSERTION must satisfy ASSERTION-P.                                                                        
Single value returned satisfies TRUTH-P."
  )

(defun assertion-has-truth (assertion truth)                                                           
  "Return T iff ASSERTION's current truth is TRUTH.                                                           
ASSERTION must satisfy ASSERTION-P.                                                                        
TRUTH must satisfy TRUTH-P.                                                                                
Single value returned satisfies BOOLEANP."
  )

(defun truth-p (object)                                                                                
  "Returns T iff OBJECT is a valid CycL truth                                                                 
:true :false or :unknown."
  )

(defun assertion-strength (assertion)                                                                  
  "Return the current argumentation strength of ASSERTION -- either :monotonic :default or :unknown.          
ASSERTION must satisfy ASSERTION-P.                                                                        
Single value returned satisfies EL-STRENGTH-P."
  )

(defun assertion-has-meta-assertions? (assertion)                                                      
  "Return T iff ASSERTION has some meta-assertions.                                                           
ASSERTION must satisfy ASSERTION-P.                                                                        
Single value returned satisfies BOOLEANP."
  )

(defun find-assertion-by-id (id)                                                                       
  "Return the assertion with ID, or NIL if not present.                                                       
ID must satisfy INTEGERP.                                                                                  
Single value returned satisfies ASSERTION-P or is NIL."
  )

(defun find-assertion (cnf mt)                                                                         
  "Find the assertion in MT with CNF. Return NIL if not present.                                              
CNF must satisfy CNF-P.                                                                                    
MT must satisfy FORT-P.                                                                                    
Single value returned satisfies ASSERTION-P or is NIL."
  )

(defun find-assertion-any-mt (cnf)                                                                     
  "Find any assertion in any mt with CNF. Return NIL if none are present.                                     
CNF must satisfy CNF-P.                                                                                    
Single value returned satisfies ASSERTION-P or is NIL."
  )

(defun find-all-assertions (cnf)                                                                       
  "Return all assertions that have CNF or NIL if there aren't any.                                            
CNF must satisfy CNF-P.                                                                                    
Single value returned satisfies (LIST ASSERTION-P) or is NIL."
  )

(defun find-gaf (gaf mt)                                                                               
  "Find the assertion in MT with GAF as its formula. Return NIL if not present.                               
GAF must satisfy EL-FORMULA-P.                                                                             
MT must satisfy FORT-P.                                                                                    
Single value returned satisfies ASSERTION-P or is NIL."
  )

(defun find-gaf-any-mt (gaf)                                                                           
  "Find any assertion in any mt with GAF as its formula. Return NIL if not present.                           
GAF must satisfy EL-FORMULA-P.                                                                             
Single value returned satisfies ASSERTION-P or is NIL."
  )

(defun find-all-gafs (gaf)                                                                             
  "Return all assertions of GAF or NIL if there aren't any.                                                   
GAF must satisfy EL-FORMULA-P.                                                                             
Single value returned satisfies (LIST ASSERTION-P) or is NIL."
  )

(defun get-asserted-argument (assertion)                                                               
  "Return the asserted argument for ASSERTION, or nil if none present.                                        
ASSERTION must satisfy ASSERTION-P.                                                                        
Single value returned satisfies ASSERTED-ARGUMENT-P or is NIL."
  )

(defun asserted-assertion? (assertion)                                                                 
  "Return non-nil IFF assertion has an asserted argument.                                                     
ASSERTION must satisfy ASSERTION-P.                                                                        
Single value returned satisfies BOOLEANP."
  )

(defun deduced-assertion? (assertion)                                                                  
  "Return non-nil IFF assertion has some deduced argument                                                     
ASSERTION must satisfy ASSERTION-P.                                                                        
Single value returned satisfies BOOLEANP."
  )

(defun assertion-el-ist-formula (assertion)                                                            
  "Return the el formula in #$ist format for ASSERTION.                                                       
ASSERTION must satisfy ASSERTION-P.                                                                        
Single value returned satisfies CONSP."
  )

(defun assertion-has-dependents-p (assertion)                                                          
  "Return non-nil IFF assertion has dependents.                                                               
ASSERTION must satisfy ASSERTION-P.                                                                        
Single value returned satisfies BOOLEANP."
  )

(defmacro do-assertions ((var &optional (message "mapping Cyc assertions")) &body body)                      
    "Iterate over all HL assertion datastructures, executing BODY within the scope of VAR.                      
VAR is bound to the assertion.                                                                             
MESSAGE is a progress message string."
    )

(defun assertion-count ()                                                                              
  "Return the total number of assertions.                                                                     
Single value returned satisfies INTEGERP."
  )

(defun negated? (form)                                                                                 
  "Assuming FORM is a valid CycL formula, return T IFF it is negated.                                         
FORM must satisfy EL-FORMULA-P.                                                                            
Single value returned satisfies BOOLEANP."
  )

(defun negate (form)                                                                                   
  "Assuming FORM is a valid CycL formula, return a negated version of it.                                     
FORM must satisfy EL-FORMULA-P.                                                                            
Single value returned satisfies EL-FORMULA-P."
  )

(defun clause-p (object)                                                                               
  "Returns T iff OBJECT is either a CNF or DNF clause.                                                        
Single value returned satisfies BOOLEANP."
  )

(defun sense-p (object)                                                                                
  "Return T iff OBJECT is a valid CycL literal sense                                                          
:neg or :pos.                                                                                              
Single value returned satisfies BOOLEANP."
  )

(defun make-clause (neg-lits pos-lits)                                                                 
  "Construct a clause from NEG-LITS and POS-LITS, each of which are lists of literals.                        
NEG-LITS must satisfy LISTP.                                                                               
POS-LITS must satisfy LISTP.                                                                               
Single value returned satisfies CLAUSE-P."
  )

(defun neg-lits (clause)                                                                               
  "Return the neg-lits of CLAUSE.                                                                             
CLAUSE must satisfy CLAUSE-P.                                                                              
Single value returned satisfies LISTP."
  )

(defun pos-lits (clause)                                                                               
  "Return the pos-lits of CLAUSE.                                                                             
CLAUSE must satisfy CLAUSE-P.                                                                              
Single value returned satisfies LISTP."
  )

(defun clause-equal (clause1 clause2)                                                                  
  "Return T iff CLAUSE1 and CLAUSE2 are both equivalent clauses.                                              
Single value returned satisfies BOOLEANP."
  )

(defun empty-clause ()                                                                                 
  "Return the empty clause.                                                                                   
Single value returned satisfies CLAUSE-P."
  )

(defun empty-clause? (clause)                                                                          
  "Return T iff CLAUSE is empty.                                                                              
CLAUSE must satisfy CLAUSE-P.                                                                              
Single value returned satisfies BOOLEANP."
  )

(defun clause-literal (clause sense num)                                                               
  "Return literal in CLAUSE specified by SENSE and NUM.                                                       
SENSE must be either :pos or :neg.                                                                         
CLAUSE must satisfy CLAUSE-P.                                                                              
SENSE must satisfy SENSE-P.                                                                                
NUM must satisfy INTEGERP."
  )

(defun clause-without-literal (clause sense num)                                                       
  "Return a new clause which is CLAUSE without the literal specified by SENSE and NUM.                        
SENSE must be either :pos or :neg.                                                                         
CLAUSE must satisfy CLAUSE-P.                                                                              
SENSE must satisfy SENSE-P.                                                                                
NUM must satisfy INTEGERP.                                                                                 
Single value returned satisfies CLAUSE-P."
  )

(defun ground-clause-p (clause)                                                                        
  "Return T iff CLAUSE is a ground clause.                                                                    
Single value returned satisfies BOOLEANP."
  )

(defun cnf-p (object)                                                                                  
  "Returns T iff OBJECT is a canonicalized CycL formula in conjunctive normal form.                           
Single value returned satisfies BOOLEANP."
  )

(defun gaf-cnf? (cnf)                                                                                  
  "Return T iff CNF is a cnf representation of a gaf formula.                                                 
Single value returned satisfies BOOLEANP."
  )

(defun cnf-formula (cnf &optional truth)                                                               
  "Return a readable formula of CNF                                                                           
TRUTH only gets looked at for ground, single pos lit cnfs                                                  
in which case TRUTH gives the truth of the gaf.                                                            
CNF must satisfy CNF-P.                                                                                    
TRUTH must satisfy TRUTH-P.                                                                                
Single value returned satisfies EL-FORMULA-P."
  )

(defun cnf-formula-from-clauses (cnf-clauses)                                                          
  "Return a readable formula from a list of CNF-CLAUSES.                                                      
CNF-CLAUSES must satisfy LISTP.                                                                            
Single value returned satisfies EL-FORMULA-P."
  )

(defun dnf-formula (dnf)                                                                               
  "Return a readable formula of DNF.                                                                          
Single value returned satisfies EL-FORMULA-P."
  )

(defun dnf-formula-from-clauses (dnf-clauses)                                                          
  "Return a readable formula from a list of DNF-CLAUSES.                                                      
DNF-CLAUSES must satisfy LISTP.                                                                            
Single value returned satisfies EL-FORMULA-P."
  )

(defun atomic-clause-p (clause)                                                                        
  "Return T iff CLAUSE is an atomic clause.                                                                   
Single value returned satisfies BOOLEANP."
  )

(defun hl-term-p (obj)                                                                                 
  "Returns T if the OBJ is a valid CycL HL term.                                                              
Single value returned satisfies BOOLEANP."
  )

(defun hl-module-p (object)                                                                            
  "Return T iff OBJECT is an HL module.                                                                       
Single value returned satisfies BOOLEANP."
  )

(defun argument-p (object)                                                                             
  "Return T iff OBJECT is an HL argument structure.                                                           
Single value returned satisfies BOOLEANP."
  )

(defun asserted-argument-p (object)                                                                    
  "Return T iff OBJECT is an HL asserted argument structure.                                                  
Single value returned satisfies BOOLEANP."
  )

(defun argument-equal (argument1 argument2)                                                            
  "Return T iff ARGUMENT1 and ARGUMENT2 are equivalent arguments.                                             
Single value returned satisfies BOOLEANP."
  )

(defun argument-truth (argument)                                                                       
  "Return the truth of ARGUMENT.                                                                              
ARGUMENT must satisfy ARGUMENT-P."
  )

(defun argument-strength (argument)                                                                    
  "Return the strength of ARGUMENT.                                                                           
ARGUMENT must satisfy ARGUMENT-P."
  )

(defun deduction-p (object)                                                                            
  "Return T iff OBJECT is a CycL deduction.                                                                   
Single value returned satisfies BOOLEANP."
  )

(defun deduction-id (deduction)                                                                        
  "Return the id of DEDUCTION.                                                                                
DEDUCTION must satisfy DEDUCTION-P.                                                                        
Single value returned satisfies INTEGERP."
  )

(defun find-deduction-by-id (id)                                                                       
  "Return the deduction with ID, or NIL if not present.                                                       
ID must satisfy INTEGERP.                                                                                  
Single value returned satisfies DEDUCTION-P or is NIL."
  )

(defun deduction-assertion (deduction)                                                                 
  "Return the assertion for which DEDUCTION is a deduction.                                                   
DEDUCTION must satisfy DEDUCTION-P.                                                                        
Single value returned satisfies ASSERTION-P."
  )

(defun deduction-count ()                                                                              
  "Return the total number of deductions.                                                                     
Single value returned satisfies INTEGERP."
  )

(defmacro do-deductions ((var &optional (message "mapping Cyc deductions")) &body body)                      
    "Iterate over all HL deduction datastructures, executing BODY within the scope of VAR.                      
VAR is a deduction.                                                                                        
MESSAGE is a progress message string."
    )

(defun support-p (object)                                                                              
  "Return T iff OBJECT can be a support in an argument.                                                       
Single value returned satisfies BOOLEANP."
  )

(defun support-module (support)                                                                        
  "Return the module of SUPPORT.                                                                              
SUPPORT must satisfy SUPPORT-P.                                                                            
Single value returned satisfies HL-MODULE-P."
  )

(defun support-mt (support)                                                                            
  "Return the microtheory of SUPPORT.                                                                         
SUPPORT must satisfy SUPPORT-P.                                                                            
Single value returned satisfies FORT-P."
  )

(defun support-truth (support)                                                                         
  "Return the truth of SUPPORT.                                                                               
SUPPORT must satisfy SUPPORT-P.                                                                            
Single value returned satisfies TRUTH-P."
  )

(defun support-strength (support)                                                                      
  "Return the strength of SUPPORT.                                                                            
SUPPORT must satisfy SUPPORT-P.                                                                            
Single value returned satisfies EL-STRENGTH-P."
  )

(defun support-sentence (support)                                                                      
  "Return the sentence of SUPPORT.                                                                            
SUPPORT must satisfy SUPPORT-P.                                                                            
Single value returned satisfies CONSP."
  )

(defun hl-support-p (object)                                                                           
  "Does OBJECT represent an HL support?                                                                       
Single value returned satisfies BOOLEANP."
  )

(defun make-hl-support (hl-module sentence &optional mt tv)                                            
  "Construct a new HL support.                                                                                
HL-MODULE must satisfy HL-MODULE-P.                                                                        
SENTENCE must satisfy POSSIBLY-SENTENCE-P.                                                                 
MT must satisfy FORT-P.                                                                                    
TV must satisfy TV-P.                                                                                      
Single value returned satisfies HL-SUPPORT-P."
  )

(defun el-var? (object)                                                                                
  "Return T iff OBJECT is a symbol which can be interpreted as an EL variable.                                
Single value returned satisfies BOOLEANP."
  )

(defun variable-p (object)                                                                             
  "Return T iff OBJECT is an HL variable.                                                                     
Single value returned satisfies BOOLEANP."
  )

(defun find-variable-by-id (id)                                                                        
  "Return the HL variable with ID, or NIL if not present.                                                     
ID must satisfy INTEGERP.                                                                                  
Single value returned satisfies VARIABLE-P or is NIL."
  )

(defun variable-id (variable)                                                                          
  "Return id of HL variable VARIABLE.                                                                         
VARIABLE must satisfy VARIABLE-P.                                                                          
Single value returned satisfies INTEGERP."
  )

(defun default-el-var-for-hl-var (variable)                                                            
  "Return a readable EL var from HL var VARIABLE.                                                             
VARIABLE must satisfy VARIABLE-P."
  )

(defun fully-bound-p (object)                                                                          
  "Return T iff OBJECT contains no HL variables, and therefore is fully bound.                                
Single value returned satisfies BOOLEANP."
  )

(defun variable-count ()                                                                               
  "Return the total number of HL variables.                                                                   
Single value returned satisfies INTEGERP."
  )

(defun indexed-term-p (object)                                                                         
  "Returns T iff OBJECT is an indexed CycL term, i.e. a fort or an assertion.                                 
Single value returned satisfies BOOLEANP."
  )

(defun remove-term-indices (term)                                                                      
  "Remove all assertions about TERM from the KB. Return the TERM.                                             
Single value returned satisfies INDEXED-TERM-P."
  )

(defun key-exception-rule-index (rule &optional mt)                                                    
  "Return a list of the keys to the next index level below RULE MT.                                           
Single value returned satisfies LISTP."
  )

(defun key-function-rule-index (func &optional mt)                                                     
  "Return a list of the keys to the next index level below FUNC MT.                                           
Single value returned satisfies LISTP."
  )

(defun key-gaf-arg-index (term &optional argnum pred)                                                  
  "Return a list of the keys to the next index level below TERM ARGNUM PRED.                                  
Single value returned satisfies LISTP."
  )

(defun key-genl-mt-rule-index (col &optional sense mt)                                                 
  "Return a list of the keys to the next index level below COL SENSE MT.                                      
Single value returned satisfies LISTP."
  )

(defun key-genls-rule-index (col &optional sense mt)                                                   
  "Return a list of the keys to the next index level below COL SENSE MT.                                      
Single value returned satisfies LISTP."
  )

(defun key-isa-rule-index (col &optional sense mt)                                                     
  "Return a list of the keys to the next index level below COL SENSE MT.                                      
Single value returned satisfies LISTP."
  )

(defun key-nart-arg-index (term &optional argnum func)                                                 
  "Return a list of the keys to the next index level below TERM ARGNUM FUNC.                                  
Single value returned satisfies LISTP."
  )

(defun key-predicate-extent-index (pred)                                                               
  "Return a list of the keys to the next predicate-extent index level below PRED.                             
Single value returned satisfies LISTP."
  )

(defun key-predicate-rule-index (pred &optional sense mt)                                              
  "Return a list of the keys to the next index level below PRED SENSE MT.                                     
Single value returned satisfies LISTP."
  )

(defun num-exception-rule-index (rule &optional mt direction)                                          
  "Return the raw assertion count at RULE MT DIRECTION.                                                       
Single value returned satisfies INTEGERP."
  )

(defun num-function-extent-index (func)                                                                
  "Return the function extent of FUNC.                                                                        
Single value returned satisfies INTEGERP."
  )

(defun num-function-rule-index (func &optional mt direction)                                           
  "Return the raw assertion count at FUNC MT DIRECTION.                                                       
Single value returned satisfies INTEGERP."
  )

(defun num-gaf-arg-index (term &optional argnum pred mt)                                               
  "Return the number of gafs indexed off of TERM ARGNUM PRED MT.                                              
Single value returned satisfies INTEGERP."
  )

(defun num-genl-mt-rule-index (col &optional sense mt direction)                                       
  "Return the raw assertion count at COL SENSE MT DIRECTION.                                                  
Single value returned satisfies INTEGERP."
  )

(defun num-genls-rule-index (col &optional sense mt direction)                                         
  "Return the raw assertion count at COL SENSE MT DIRECTION.                                                  
Single value returned satisfies INTEGERP."
  )

(defun num-index (term)                                                                                
  "The total number of assertions indexed from TERM.                                                          
Single value returned satisfies INTEGERP."
  )

(defun num-isa-rule-index (col &optional sense mt direction)                                           
  "Return the raw assertion count at COL SENSE MT DIRECTION.                                                  
Single value returned satisfies INTEGERP."
  )

(defun num-mt-index (term)                                                                             
  "Return the number of assertions at the mt index for TERM.                                                  
Single value returned satisfies INTEGERP."
  )

(defun num-nart-arg-index (term &optional argnum func)                                                 
  "Return the number of #$termOfUnit gafs indexed off of TERM ARGNUM FUNC.                                    
Single value returned satisfies INTEGERP."
  )

(defun num-other-index (term)                                                                          
  "Return the number of assertions at the other index for TERM.                                               
Single value returned satisfies INTEGERP."
  )

(defun num-predicate-extent-index (pred &optional mt)                                                  
  "Return the assertion count at PRED MT.                                                                     
Single value returned satisfies INTEGERP."
  )

(defun num-predicate-rule-index (pred &optional sense mt direction)                                    
  "Return the raw assertion count at PRED SENSE MT DIRECTION.                                                 
Single value returned satisfies INTEGERP."
  )

(defun relevant-num-function-extent-index (func)                                                       
  "Compute the function extent at relevant mts under FUNC.                                                    
This will be the entire function extent if #$BaseKB is relevant,                                           
and zero otherwise.                                                                                        
Single value returned satisfies INTEGERP."
  )

(defun relevant-num-gaf-arg-index (term &optional argnum pred)                                         
  "Return the assertion count at relevant mts under TERM ARGNUM PRED.                                         
Single value returned satisfies INTEGERP."
  )

(defun relevant-num-nart-arg-index (term &optional argnum func)                                        
  "Compute the assertion count at relevant mts under TERM ARGNUM FUNC.                                        
This will be the entire count extent if #$BaseKB is relevant,                                              
and zero otherwise.                                                                                        
Single value returned satisfies INTEGERP."
  )

(defun relevant-num-predicate-extent-index (pred)                                                      
  "Compute the assertion count at relevant mts under PRED.                                                    
Single value returned satisfies INTEGERP."
  )

(defmacro with-mt (mt &body body)                                                                            
    "MT and all its genl mts are relevant within BODY."
    )

(defmacro with-all-mts (&body body)                                                                          
    "All mts are relevant within BODY."
    )

(defmacro with-just-mt (mt &body body)                                                                       
    "Only MT is relevant within BODY (no genl mts)."
    )

(defmacro with-mt-list (mt-list &body body)                                                                  
    "Each mt in the list MT-LIST is relevant within BODY (no genl mts)."
    )

(defmacro with-any-mt (&body body)                                                                           
    "Any mt can be used for relevance for a particular inference within &BODY."
    )

(defmacro with-genl-mts (mt &body body)                                                                      
    "MT and all its genl mts are relevant within BODY."
    )

(defmacro map-mts ((var) &body body)                                                                         
    "Iterate over all microtheories, binding VAR to an mt and executing BODY."
    )

(defun map-term (function term)                                                                        
  "Apply FUNCTION to each assertion indexed from TERM.                                                        
- (defun must satisfy FUNCTION-SPEC-P.                                                                     
Single value returned satisfies NULL."
  )

(defun map-term-selective (function term test &optional truth)                                         
  "Apply FUNCTION to each assertion indexed from TERM with TRUTH that passes TEST.                            
If TRUTH is nil, all assertions are mapped.                                                                
- (defun must satisfy FUNCTION-SPEC-P.                                                                     
TEST must satisfy FUNCTION-SPEC-P.                                                                         
Single value returned satisfies NULL."
  )

(defun map-term-gafs (function term &optional truth)                                                   
  "Apply FUNCTION to every gaf indexed from TERM.                                                             
If TRUTH is nil, all assertions are mapped.                                                                
- (defun must satisfy FUNCTION-SPEC-P.                                                                     
Single value returned satisfies NULL."
  )

(defun map-mt-contents (function term &optional truth gafs-only)                                       
  "Apply FUNCTION to each assertion with TRUTH in MT TERM.                                                    
If TRUTH is nil, all assertions are mapped.                                                                
If GAFS-ONLY, then only gafs are mapped.                                                                   
- (defun must satisfy FUNCTION-SPEC-P.                                                                     
Single value returned satisfies NULL."
  )

(defun map-mt-index (function mt &optional truth gafs-only)                                            
  "Apply FUNCTION to each assertion with TRUTH at mt index MT.                                                
If TRUTH is nil, all assertions are mapped.                                                                
If GAFS-ONLY, then only gafs are mapped.                                                                   
- (defun must satisfy FUNCTION-SPEC-P.                                                                     
Single value returned satisfies NULL."
  )

(defun map-other-index (function term &optional truth gafs-only)                                       
  "Apply FUNCTION to each assertion with TRUTH at other index TERM.                                           
If TRUTH is nil, all assertions are mapped.                                                                
If GAFS-ONLY, then only gafs are mapped.                                                                   
- (defun must satisfy FUNCTION-SPEC-P.                                                                     
Single value returned satisfies NULL."
  )

(defun gather-index (term &optional remove-duplicates?)                                                
  "Return a list of all mt-relevant assertions indexed via TERM.                                              
If REMOVE-DUPLICATES? is non-nil, assertions are guaranteed to only be listed once.                        
Single value returned is a list of elements satisfying ASSERTION-P."
  )

(defun gather-index-in-any-mt (term &optional remove-duplicates?)                                      
  "Return a list of all assertions indexed via TERM.                                                          
If REMOVE-DUPLICATES? is non-nil, assertions are guaranteed to only be listed once.                        
Single value returned is a list of elements satisfying ASSERTION-P."
  )

(defun gather-exception-rule-index (rule &optional mt direction)                                       
  "Return a list of all non-gaf assertions (rules) such that:                                                 
a) it has a positive literal of the form (abnormal <whatever> RULE)                                        
b) if MT is non-nil, then MT must be its microtheory                                                       
c) if DIRECTION is non-nil, then DIRECTION must be its direction.                                          
Single value returned is a list of elements satisfying ASSERTION-P."
  )

(defun gather-function-extent-index (func)                                                             
  "Return a list of all #$termOfUnit assertions such that:                                                    
FUNC is the functor of the naut arg2.                                                                      
Single value returned is a list of elements satisfying ASSERTION-P."
  )

(defun gather-function-rule-index (func &optional mt direction)                                        
  "Return a list of all non-gaf assertions (rules) such that:                                                 
a) it has a negative literal of the form (termOfUnit <whatever> (FUNC . <whatever>))                       
b) if MT is non-nil, then MT must be its microtheory                                                       
c) if DIRECTION is non-nil, then DIRECTION must be its direction.                                          
Single value returned is a list of elements satisfying ASSERTION-P."
  )

(defun gather-gaf-arg-index (term argnum &optional pred mt truth)                                      
  "Return a list of all gaf assertions such that:                                                             
a) TERM is its ARGNUMth argument                                                                           
b) if TRUTH is non-nil, then TRUTH is its truth value                                                      
c) if PRED is non-nil, then PRED must be its predicate                                                     
d) if MT is non-nil, then MT must be its microtheory (and PRED must be non-nil).                           
ARGNUM must satisfy POSITIVE-INTEGER-P.                                                                    
Single value returned is a list of elements satisfying ASSERTION-P."
  )

(defun gather-genl-mt-rule-index (genl-mt sense &optional rule-mt direction)                           
  "Return a list of all non-gaf assertions (rules) such that:                                                 
a) if SENSE is :pos, it has a positive literal of the form (genlMt <whatever> GENL-MT)                     
b) if SENSE is :neg, it has a negative literal of the form (genlMt <whatever> GENL-MT)                     
c) if RULE-MT is non-nil, then RULE-MT must be its microtheory                                             
d) if DIRECTION is non-nil, then DIRECTION must be its direction.                                          
SENSE must satisfy SENSE-P.                                                                                
Single value returned is a list of elements satisfying ASSERTION-P."
  )

(defun gather-genls-rule-index (collection sense &optional mt direction)                               
  "Return a list of all non-gaf assertions (rules) such that:                                                 
a) if SENSE is :pos, it has a positive literal of the form (genls <whatever> COLLECTION)                   
b) if SENSE is :neg, it has a negative literal of the form (genls <whatever> COLLECTION)                   
c) if MT is non-nil, then MT must be its microtheory                                                       
d) if DIRECTION is non-nil, then DIRECTION must be its direction.                                          
SENSE must satisfy SENSE-P.                                                                                
Single value returned is a list of elements satisfying ASSERTION-P."
  )

(defun gather-isa-rule-index (collection sense &optional mt direction)                                 
  "Return a list of all non-gaf assertions (rules) such that:                                                 
a) if SENSE is :pos, it has a positive literal of the form (isa <whatever> COLLECTION)                     
b) if SENSE is :neg, it has a negative literal of the form (isa <whatever> COLLECTION)                     
c) if MT is non-nil, then MT must be its microtheory                                                       
d) if DIRECTION is non-nil, then DIRECTION must be its direction.                                          
SENSE must satisfy SENSE-P.                                                                                
Single value returned is a list of elements satisfying ASSERTION-P."
  )

(defun gather-mt-index (term)                                                                          
  "Return a list of all assertions such that TERM is its microtheory.                                         
Single value returned is a list of elements satisfying ASSERTION-P."
  )

(defun gather-nart-arg-index (term argnum &optional func)                                              
  "Return a list of all #$termOfUnit assertions with a naut arg2 such that:                                   
a) TERM is its ARGNUMth argument                                                                           
b) if FUNC is non-nil, then FUNC must be its functor                                                       
ARGNUM must satisfy POSITIVE-INTEGER-P.                                                                    
Single value returned is a list of elements satisfying ASSERTION-P."
  )

(defun gather-other-index (term)                                                                       
  "Return a list of other assertions mentioning TERM but not indexed in any other more useful manner.         
Single value returned is a list of elements satisfying ASSERTION-P."
  )

(defun gather-predicate-extent-index (pred &optional mt truth)                                         
  "Return a list of all gaf assertions such that:                                                             
a) PRED is its predicate                                                                                   
b) if TRUTH is non-nil, then TRUTH is its truth value                                                      
c) if MT is non-nil, then MT must be its microtheory.                                                      
Single value returned is a list of elements satisfying ASSERTION-P."
  )

(defun gather-predicate-rule-index (pred sense &optional mt direction)                                 
  "Return a list of all non-gaf assertions (rules) such that:                                                 
a) if SENSE is :pos, it has PRED as a predicate in a positive literal                                      
b) if SENSE is :neg, it has PRED as a predicate in a negative literal                                      
c) if MT is non-nil, then MT must be its microtheory                                                       
d) if DIRECTION is non-nil, then DIRECTION must be its direction.                                          
SENSE must satisfy SENSE-P.                                                                                
Single value returned is a list of elements satisfying ASSERTION-P."
  )

(defun gather-term-assertions (term &optional mt)                                                      
  "Return a list of all mt-relevant assertions of TERM.                                                       
Single value returned is a list of elements satisfying ASSERTION-P."
  )

(defun fpred-value (term pred &optional index-arg gather-arg truth)                                    
  "Find the first gaf assertion such that:                                                                    
- (a) the assertion is in a relevant microtheory (relevance is established outside)                          
- (b) if TRUTH is non-nil, the assertion has TRUTH as its truth value                                        
- (c) PRED is the predicate used.                                                                            
- (d) TERM is the term in the INDEX-ARG position.                                                            
Return the term in the GATHER-ARG position if such an assertion exists.                                    
Otherwise, return NIL.                                                                                     
TERM must satisfy INDEXED-TERM-P.                                                                          
PRED must satisfy FORT-P.                                                                                  
INDEX-ARG must satisfy INTEGERP.                                                                           
GATHER-ARG must satisfy INTEGERP.                                                                          
TRUTH must satisfy TRUTH-P.                                                                                
Single value returned satisfies HL-TERM-P or is NIL."
  )

(defun fpred-value-in-mt (term pred mt &optional index-arg gather-arg truth)                           
  "Find the first gaf assertion such that:                                                                    
- (a) the assertion is in microtheory MT                                                                     
- (b) if TRUTH is non-nil, the assertion has TRUTH as its truth value                                        
- (c) PRED is the predicate used.                                                                            
- (d) TERM is the term in the INDEX-ARG position.                                                            
Return the term in the GATHER-ARG position if such an assertion exists.                                    
Otherwise, return NIL.                                                                                     
TERM must satisfy INDEXED-TERM-P.                                                                          
PRED must satisfy FORT-P.                                                                                  
MT must satisfy HLMT-P.                                                                                    
INDEX-ARG must satisfy INTEGERP.                                                                           
GATHER-ARG must satisfy INTEGERP.                                                                          
TRUTH must satisfy TRUTH-P.                                                                                
Single value returned satisfies HL-TERM-P or is NIL."
  )

(defun fpred-value-in-mts (term pred mts &optional index-arg gather-arg truth)                         
  "Find the first gaf assertion such that:                                                                    
- (a) the assertion is in one of the microtheories in the list MTS                                           
- (b) if TRUTH is non-nil, the assertion has TRUTH as its truth value                                        
- (c) PRED is the predicate used.                                                                            
- (d) TERM is the term in the INDEX-ARG position.                                                            
Return the term in the GATHER-ARG position if such an assertion exists.                                    
Otherwise, return NIL.                                                                                     
TERM must satisfy INDEXED-TERM-P.                                                                          
PRED must satisfy FORT-P.                                                                                  
MTS must satisfy LISTP.                                                                                    
INDEX-ARG must satisfy INTEGERP.                                                                           
GATHER-ARG must satisfy INTEGERP.                                                                          
TRUTH must satisfy TRUTH-P.                                                                                
Single value returned satisfies HL-TERM-P or is NIL."
  )

(defun fpred-value-in-any-mt (term pred &optional index-arg gather-arg truth)                          
  "Find the first gaf assertion such that:                                                                    
- (a) the assertion is allowed to be in any microtheory                                                      
- (b) if TRUTH is non-nil, the assertion has TRUTH as its truth value                                        
- (c) PRED is the predicate used.                                                                            
- (d) TERM is the term in the INDEX-ARG position.                                                            
Return the term in the GATHER-ARG position if such an assertion exists.                                    
Otherwise, return NIL.                                                                                     
TERM must satisfy INDEXED-TERM-P.                                                                          
PRED must satisfy FORT-P.                                                                                  
INDEX-ARG must satisfy INTEGERP.                                                                           
GATHER-ARG must satisfy INTEGERP.                                                                          
TRUTH must satisfy TRUTH-P.                                                                                
Single value returned satisfies HL-TERM-P or is NIL."
  )

(defun fpred-value-in-relevant-mts (term pred &optional mt index-arg gather-arg truth)                 
  "If MT is NIL, behaves like FPRED-VALUE. Otherwise, behaves like FPRED-VALUE-IN-MT.                         
TERM must satisfy INDEXED-TERM-P.                                                                          
PRED must satisfy FORT-P.                                                                                  
INDEX-ARG must satisfy INTEGERP.                                                                           
GATHER-ARG must satisfy INTEGERP.                                                                          
TRUTH must satisfy TRUTH-P.                                                                                
Single value returned satisfies HL-TERM-P or is NIL."
  )

(defun pred-values (term pred &optional index-arg gather-arg truth)                                    
  "Find all gaf assertions such that:                                                                         
- (a) the assertion is in a relevant microtheory (relevance is established outside)                          
- (b) if TRUTH is non-nil, the assertion has TRUTH as its truth value                                        
- (c) PRED is the predicate used.                                                                            
- (d) TERM is the term in the INDEX-ARG position.                                                            
Return a list of the terms in the GATHER-ARG position of all such assertions.                              
TERM must satisfy INDEXED-TERM-P.                                                                          
PRED must satisfy FORT-P.                                                                                  
INDEX-ARG must satisfy INTEGERP.                                                                           
GATHER-ARG must satisfy INTEGERP.                                                                          
TRUTH must satisfy TRUTH-P.                                                                                
Single value returned is a list of elements satisfying HL-TERM-P."
  )

(defun pred-values-in-mt (term pred mt &optional index-arg gather-arg truth)                           
  "Find all gaf assertions such that:                                                                         
- (a) the assertion is in microtheory MT                                                                     
- (b) if TRUTH is non-nil, the assertion has TRUTH as its truth value                                        
- (c) PRED is the predicate used.                                                                            
- (d) TERM is the term in the INDEX-ARG position.                                                            
Return a list of the terms in the GATHER-ARG position of all such assertions.                              
TERM must satisfy INDEXED-TERM-P.                                                                          
PRED must satisfy FORT-P.                                                                                  
MT must satisfy HLMT-P.                                                                                    
INDEX-ARG must satisfy INTEGERP.                                                                           
GATHER-ARG must satisfy INTEGERP.                                                                          
TRUTH must satisfy TRUTH-P.                                                                                
Single value returned is a list of elements satisfying HL-TERM-P."
  )

(defun pred-values-in-mts (term pred mts &optional index-arg gather-arg truth)                         
  "Find all gaf assertions such that:                                                                         
- (a) the assertion is in one of the microtheories in the list MTS                                           
- (b) if TRUTH is non-nil, the assertion has TRUTH as its truth value                                        
- (c) PRED is the predicate used.                                                                            
- (d) TERM is the term in the INDEX-ARG position.                                                            
Return a list of the terms in the GATHER-ARG position of all such assertions.                              
TERM must satisfy INDEXED-TERM-P.                                                                          
PRED must satisfy FORT-P.                                                                                  
MTS must satisfy LISTP.                                                                                    
INDEX-ARG must satisfy INTEGERP.                                                                           
GATHER-ARG must satisfy INTEGERP.                                                                          
TRUTH must satisfy TRUTH-P.                                                                                
Single value returned is a list of elements satisfying HL-TERM-P."
  )

(defun pred-values-in-any-mt (term pred &optional index-arg gather-arg truth)                          
  "Find all gaf assertions such that:                                                                         
- (a) the assertion is allowed to be in any microtheory                                                      
- (b) if TRUTH is non-nil, the assertion has TRUTH as its truth value                                        
- (c) PRED is the predicate used.                                                                            
- (d) TERM is the term in the INDEX-ARG position.                                                            
Return a list of the terms in the GATHER-ARG position of all such assertions.                              
TERM must satisfy INDEXED-TERM-P.                                                                          
PRED must satisfy FORT-P.                                                                                  
INDEX-ARG must satisfy INTEGERP.                                                                           
GATHER-ARG must satisfy INTEGERP.                                                                          
TRUTH must satisfy TRUTH-P.                                                                                
Single value returned is a list of elements satisfying HL-TERM-P."
  )

(defun pred-values-in-relevant-mts (term pred &optional mt index-arg gather-arg truth)                 
  "If MT is NIL, behaves like PRED-VALUES. Otherwise, behaves like PRED-VALUES-IN-MT                          
TERM must satisfy INDEXED-TERM-P.                                                                          
PRED must satisfy FORT-P.                                                                                  
INDEX-ARG must satisfy INTEGERP.                                                                           
GATHER-ARG must satisfy INTEGERP.                                                                          
TRUTH must satisfy TRUTH-P.                                                                                
Single value returned is a list of elements satisfying HL-TERM-P."
  )

(defun pred-refs (pred &optional gather-arg truth)                                                     
  "Find all gaf assertions such that:                                                                         
- (a) the assertion is in a relevant microtheory (relevance is established outside)                          
- (b) if TRUTH is non-nil, the assertion has TRUTH as its truth value                                        
- (c) PRED is the predicate used.                                                                            
Return a list of the terms in the GATHER-ARG position of all such assertions.                              
PRED must satisfy FORT-P.                                                                                  
GATHER-ARG must satisfy INTEGERP.                                                                          
TRUTH must satisfy TRUTH-P.                                                                                
Single value returned is a list of elements satisfying HL-TERM-P."
  )

(defun pred-refs-in-mt (pred mt &optional gather-arg truth)                                            
  "Find all gaf assertions such that:                                                                         
- (a) the assertion is in microtheory MT                                                                     
- (b) if TRUTH is non-nil, the assertion has TRUTH as its truth value                                        
- (c) PRED is the predicate used.                                                                            
Return a list of the terms in the GATHER-ARG position of all such assertions.                              
PRED must satisfy FORT-P.                                                                                  
MT must satisfy HLMT-P.                                                                                    
GATHER-ARG must satisfy INTEGERP.                                                                          
TRUTH must satisfy TRUTH-P.                                                                                
Single value returned is a list of elements satisfying HL-TERM-P."
  )

(defun pred-refs-in-mts (pred mts &optional gather-arg truth)                                          
  "Find all gaf assertions such that:                                                                         
- (a) the assertion is in one of the microtheories in the list MTS                                           
- (b) if TRUTH is non-nil, the assertion has TRUTH as its truth value                                        
- (c) PRED is the predicate used.                                                                            
Return a list of the terms in the GATHER-ARG position of all such assertions.                              
PRED must satisfy FORT-P.                                                                                  
MTS must satisfy LISTP.                                                                                    
GATHER-ARG must satisfy INTEGERP.                                                                          
TRUTH must satisfy TRUTH-P.                                                                                
Single value returned is a list of elements satisfying HL-TERM-P."
  )

(defun pred-refs-in-any-mt (pred &optional gather-arg truth)                                           
  "Find all gaf assertions such that:                                                                         
- (a) the assertion is allowed to be in any microtheory                                                      
- (b) if TRUTH is non-nil, the assertion has TRUTH as its truth value                                        
- (c) PRED is the predicate used.                                                                            
Return a list of the terms in the GATHER-ARG position of all such assertions.                              
PRED must satisfy FORT-P.                                                                                  
GATHER-ARG must satisfy INTEGERP.                                                                          
TRUTH must satisfy TRUTH-P.                                                                                
Single value returned is a list of elements satisfying HL-TERM-P."
  )

(defun pred-refs-in-relevant-mts (pred &optional mt gather-arg truth)                                  
  "If MT is NIL, behaves like PRED-REFS. Otherwise, behaves like PRED-REFS-IN-MT                              
PRED must satisfy FORT-P.                                                                                  
GATHER-ARG must satisfy INTEGERP.                                                                          
TRUTH must satisfy TRUTH-P.                                                                                
Single value returned is a list of elements satisfying HL-TERM-P."
  )

(defun some-pred-value (term pred &optional index-arg truth)                                           
  "Find the first gaf assertion such that:                                                                    
- (a) the assertion is in a relevant microtheory (relevance is established outside)                          
- (b) if TRUTH is non-nil, the assertion has TRUTH as its truth value                                        
- (c) PRED is the predicate used.                                                                            
- (d) TERM is the term in the INDEX-ARG position.                                                            
Return T if such an assertion exists, otherwise return NIL.                                                
TERM must satisfy INDEXED-TERM-P.                                                                          
PRED must satisfy FORT-P.                                                                                  
INDEX-ARG must satisfy INTEGERP.                                                                           
TRUTH must satisfy TRUTH-P.                                                                                
Single value returned satisfies BOOLEANP."
  )

(defun some-pred-value-in-mt (term pred mt &optional index-arg truth)                                  
  "Find the first gaf assertion such that:                                                                    
- (a) the assertion is in microtheory MT                                                                     
- (b) if TRUTH is non-nil, the assertion has TRUTH as its truth value                                        
- (c) PRED is the predicate used.                                                                            
- (d) TERM is the term in the INDEX-ARG position.                                                            
Return T if such an assertion exists, otherwise return NIL.                                                
TERM must satisfy INDEXED-TERM-P.                                                                          
PRED must satisfy FORT-P.                                                                                  
MT must satisfy HLMT-P.                                                                                    
INDEX-ARG must satisfy INTEGERP.                                                                           
TRUTH must satisfy TRUTH-P.                                                                                
Single value returned satisfies BOOLEANP."
  )

(defun some-pred-value-in-mts (term pred mts &optional index-arg truth)                                
  "Find the first gaf assertion such that:                                                                    
- (a) the assertion is in one of the microtheories in the list MTS                                           
- (b) if TRUTH is non-nil, the assertion has TRUTH as its truth value                                        
- (c) PRED is the predicate used.                                                                            
- (d) TERM is the term in the INDEX-ARG position.                                                            
Return T if such an assertion exists, otherwise return NIL.                                                
TERM must satisfy INDEXED-TERM-P.                                                                          
PRED must satisfy FORT-P.                                                                                  
MTS must satisfy LISTP.                                                                                    
INDEX-ARG must satisfy INTEGERP.                                                                           
TRUTH must satisfy TRUTH-P.                                                                                
Single value returned satisfies BOOLEANP."
  )

(defun some-pred-value-in-any-mt (term pred &optional index-arg truth)                                 
  "Find the first gaf assertion such that:                                                                    
- (a) the assertion is allowed to be in any microtheory                                                      
- (b) if TRUTH is non-nil, the assertion has TRUTH as its truth value                                        
- (c) PRED is the predicate used.                                                                            
- (d) TERM is the term in the INDEX-ARG position.                                                            
Return T if such an assertion exists, otherwise return NIL.                                                
TERM must satisfy INDEXED-TERM-P.                                                                          
PRED must satisfy FORT-P.                                                                                  
INDEX-ARG must satisfy INTEGERP.                                                                           
TRUTH must satisfy TRUTH-P.                                                                                
Single value returned satisfies BOOLEANP."
  )

(defun some-pred-value-in-relevant-mts (term pred &optional mt index-arg truth)                        
  "If MT is NIL, behaves like SOME-PRED-VALUE. Otherwise, behaves like SOME-PRED-VALUE-IN-MT                  
TERM must satisfy INDEXED-TERM-P.                                                                          
PRED must satisfy FORT-P.                                                                                  
INDEX-ARG must satisfy INTEGERP.                                                                           
TRUTH must satisfy TRUTH-P.                                                                                
Single value returned satisfies BOOLEANP."
  )

(defun pred-u-v-holds (pred u v &optional u-arg v-arg truth)                                           
  "Find the first gaf assertion such that:                                                                    
- (a) the assertion is in a relevant microtheory (relevance is established outside)                          
- (b) if TRUTH is non-nil, the assertion has TRUTH as its truth value                                        
- (c) PRED is the predicate used.                                                                            
- (d) U is the term in the U-ARG position.                                                                   
- (e) V is the term in the V-ARG position.                                                                   
Return T if such an assertion exists, otherwise return NIL.                                                
PRED must satisfy FORT-P.                                                                                  
U must satisfy INDEXED-TERM-P.                                                                             
V must satisfy HL-TERM-P.                                                                                  
U-ARG must satisfy INTEGERP.                                                                               
V-ARG must satisfy INTEGERP.                                                                               
TRUTH must satisfy TRUTH-P.                                                                                
Single value returned satisfies BOOLEANP."
  )

(defun pred-u-v-holds-in-mt (pred u v mt &optional u-arg v-arg truth)                                  
  "Find the first gaf assertion such that:                                                                    
- (a) the assertion is in microtheory MT                                                                     
- (b) if TRUTH is non-nil, the assertion has TRUTH as its truth value                                        
- (c) PRED is the predicate used.                                                                            
- (d) U is the term in the U-ARG position.                                                                   
- (e) V is the term in the V-ARG position.                                                                   
Return T if such an assertion exists, otherwise return NIL.                                                
PRED must satisfy FORT-P.                                                                                  
U must satisfy INDEXED-TERM-P.                                                                             
V must satisfy HL-TERM-P.                                                                                  
MT must satisfy HLMT-P.                                                                                    
U-ARG must satisfy INTEGERP.                                                                               
V-ARG must satisfy INTEGERP.                                                                               
TRUTH must satisfy TRUTH-P.                                                                                
Single value returned satisfies BOOLEANP."
  )

(defun pred-u-v-holds-in-mts (pred u v mts &optional u-arg v-arg truth)                                
  "Find the first gaf assertion such that:                                                                    
- (a) the assertion is in one of the microtheories in the list MTS                                           
- (b) if TRUTH is non-nil, the assertion has TRUTH as its truth value                                        
- (c) PRED is the predicate used.                                                                            
- (d) U is the term in the U-ARG position.                                                                   
- (e) V is the term in the V-ARG position.                                                                   
Return T if such an assertion exists, otherwise return NIL.                                                
PRED must satisfy FORT-P.                                                                                  
U must satisfy INDEXED-TERM-P.                                                                             
V must satisfy HL-TERM-P.                                                                                  
MTS must satisfy LISTP.                                                                                    
U-ARG must satisfy INTEGERP.                                                                               
V-ARG must satisfy INTEGERP.                                                                               
TRUTH must satisfy TRUTH-P.                                                                                
Single value returned satisfies BOOLEANP."
  )

(defun pred-u-v-holds-in-any-mt (pred u v &optional u-arg v-arg truth)                                 
  "Find the first gaf assertion such that:                                                                    
- (a) the assertion is allowed to be in any microtheory                                                      
- (b) if TRUTH is non-nil, the assertion has TRUTH as its truth value                                        
- (c) PRED is the predicate used.                                                                            
- (d) U is the term in the U-ARG position.                                                                   
- (e) V is the term in the V-ARG position.                                                                   
Return T if such an assertion exists, otherwise return NIL.                                                
PRED must satisfy FORT-P.                                                                                  
U must satisfy INDEXED-TERM-P.                                                                             
V must satisfy HL-TERM-P.                                                                                  
U-ARG must satisfy INTEGERP.                                                                               
V-ARG must satisfy INTEGERP.                                                                               
TRUTH must satisfy TRUTH-P.                                                                                
Single value returned satisfies BOOLEANP."
  )

(defun pred-u-v-holds-in-relevant-mts (pred u v &optional mt u-arg v-arg truth)                        
  "If MT is NIL, behaves like PRED-U-V-HOLDS. Otherwise, behaves like PRED-U-V-HOLDS-IN-MT                    
PRED must satisfy FORT-P.                                                                                  
U must satisfy INDEXED-TERM-P.                                                                             
V must satisfy HL-TERM-P.                                                                                  
U-ARG must satisfy INTEGERP.                                                                               
V-ARG must satisfy INTEGERP.                                                                               
TRUTH must satisfy TRUTH-P.                                                                                
Single value returned satisfies BOOLEANP."
  )

(defun pred-value-tuples (term pred index-arg gather-args &optional truth)                             
  "Find all gaf assertions such that:                                                                         
- (a) the assertion is in a relevant microtheory (relevance is established outside)                          
- (b) if TRUTH is non-nil, the assertion has TRUTH as its truth value                                        
- (c) PRED is the predicate used.                                                                            
- (d) TERM is the term in the INDEX-ARG position.                                                            
Return a list of tuples formed from the GATHER-ARGS positions of all such assertions.                      
TERM must satisfy INDEXED-TERM-P.                                                                          
PRED must satisfy FORT-P.                                                                                  
INDEX-ARG must satisfy INTEGERP.                                                                           
GATHER-ARGS must satisfy LISTP.                                                                            
TRUTH must satisfy TRUTH-P.                                                                                
Single value returned is a list of elements satisfying LISTP."
  )

(defun pred-value-tuples-in-mt (term pred index-arg gather-args mt &optional truth)                    
  "Find all gaf assertions such that:                                                                         
- (a) the assertion is microtheory MT                                                                        
- (b) if TRUTH is non-nil, the assertion has TRUTH as its truth value                                        
- (c) PRED is the predicate used.                                                                            
- (d) TERM is the term in the INDEX-ARG position.                                                            
Return a list of tuples formed from the GATHER-ARGS positions of all such assertions.                      
TERM must satisfy INDEXED-TERM-P.                                                                          
PRED must satisfy FORT-P.                                                                                  
INDEX-ARG must satisfy INTEGERP.                                                                           
GATHER-ARGS must satisfy LISTP.                                                                            
MT must satisfy HLMT-P.                                                                                    
TRUTH must satisfy TRUTH-P.                                                                                
Single value returned is a list of elements satisfying LISTP."
  )

(defun pred-value-tuples-in-mts (term pred index-arg gather-args mts &optional truth)                  
  "Find all gaf assertions such that:                                                                         
- (a) the assertion is in one of the microtheories in the list MTS                                           
- (b) if TRUTH is non-nil, the assertion has TRUTH as its truth value                                        
- (c) PRED is the predicate used.                                                                            
- (d) TERM is the term in the INDEX-ARG position.                                                            
Return a list of tuples formed from the GATHER-ARGS positions of all such assertions.                      
TERM must satisfy INDEXED-TERM-P.                                                                          
PRED must satisfy FORT-P.                                                                                  
INDEX-ARG must satisfy INTEGERP.                                                                           
GATHER-ARGS must satisfy LISTP.                                                                            
MTS must satisfy LISTP.                                                                                    
TRUTH must satisfy TRUTH-P.                                                                                
Single value returned is a list of elements satisfying LISTP."
  )

(defun pred-value-tuples-in-any-mt (term pred index-arg gather-args &optional truth)                   
  "Find all gaf assertions such that:                                                                         
- (a) the assertion is allowed to be from any microtheory                                                    
- (b) if TRUTH is non-nil, the assertion has TRUTH as its truth value                                        
- (c) PRED is the predicate used.                                                                            
- (d) TERM is the term in the INDEX-ARG position.                                                            
Return a list of tuples formed from the GATHER-ARGS positions of all such assertions.                      
TERM must satisfy INDEXED-TERM-P.                                                                          
PRED must satisfy FORT-P.                                                                                  
INDEX-ARG must satisfy INTEGERP.                                                                           
GATHER-ARGS must satisfy LISTP.                                                                            
TRUTH must satisfy TRUTH-P.                                                                                
Single value returned is a list of elements satisfying LISTP."
  )

(defun pred-value-tuples-in-relevant-mts (term pred index-arg gather-args &optional mt truth)          
  "If MT is NIL, behaves like PRED-VALUE-TUPLES. Otherwise, behaves like PRED-VALUE-TUPLES-IN-MT              
TERM must satisfy INDEXED-TERM-P.                                                                          
PRED must satisfy FORT-P.                                                                                  
INDEX-ARG must satisfy INTEGERP.                                                                           
GATHER-ARGS must satisfy LISTP.                                                                            
TRUTH must satisfy TRUTH-P.                                                                                
Single value returned is a list of elements satisfying LISTP."
  )

(defun reflexive-predicate? (predicate &optional mt)                                                   
  "Return T iff PREDICATE is a reflexive predicate.                                                           
Single value returned satisfies BOOLEANP."
  )

(defun irreflexive-predicate? (predicate &optional mt)                                                 
  "Return T iff PREDICATE is an irreflexive predicate.                                                        
Single value returned satisfies BOOLEANP."
  )

(defun symmetric-predicate? (predicate &optional mt)                                                   
  "Return T iff PREDICATE is a symmetric predicate.                                                           
Single value returned satisfies BOOLEANP."
  )

(defun asymmetric-predicate? (predicate &optional mt)                                                  
  "Return T iff PREDICATE is an asymmetric predicate.                                                         
Single value returned satisfies BOOLEANP."
  )

(defun anti-symmetric-predicate? (predicate &optional mt)                                              
  "Return T iff PREDICATE is an anti-symmetric predicate.                                                     
Single value returned satisfies BOOLEANP."
  )

(defun transitive-predicate? (predicate &optional mt)                                                  
  "Return T iff PREDICATE is a transitive predicate.                                                          
Single value returned satisfies BOOLEANP."
  )

(defun relation? (relation &optional mt)                                                               
  "Return T iff RELATION is a relationship.                                                                   
Single value returned satisfies BOOLEANP."
  )

(defun commutative-relation? (relation &optional mt)                                                   
  "Return T iff RELATION is a commutative relation.                                                           
Single value returned satisfies BOOLEANP."
  )

(defun commutative-function? (function &optional mt)                                                   
  "Return T iff FUNCTION is a commutative function.                                                           
Single value returned satisfies BOOLEANP."
  )

(defun binary-predicate? (predicate &optional mt)                                                      
  "Return T iff PREDICATE is a predicate of arity 2.                                                          
Single value returned satisfies BOOLEANP."
  )

(defun argn-isa (relation argnum &optional mt)                                                         
  "Returns a list of the local isa constraints applied to the ARGNUMth argument of                            
RELATION (#$argsIsa conjoins with #$arg1Isa et al).                                                        
ARGNUM must satisfy INTEGERP."
  )

(defun min-argn-isa (relation n &optional mt)                                                          
  "Returns a list of the most specific local isa-constraints applicable                                       
to argument N of RELATION.                                                                                 
RELATION must satisfy INDEXED-TERM-P.                                                                      
N must satisfy INTEGERP.                                                                                   
Single value returned is a list of elements satisfying INDEXED-TERM-P."
  )

(defun argn-isa-of (collection argnum &optional mt)                                                    
  "Returns the relations for which COLLECTION is a                                                            
local isa constraint applied to argument ARGNUM.                                                           
ARGNUM must satisfy INTEGERP.                                                                              
Single value returned is a list of elements satisfying INDEXED-TERM-P."
  )

(defun argn-genl (relation argnum &optional mt)                                                        
  "Returns the local genl constraints applied to the ARGNUMth argument of RELATION.                           
ARGNUM must satisfy INTEGERP.                                                                              
Single value returned is a list of elements satisfying INDEXED-TERM-P."
  )

(defun min-argn-genl (relation n &optional mt)                                                         
  "Return a list of the most specific local genl constraints applicable                                       
to the argument N of RELATION.                                                                             
N must satisfy INTEGERP.                                                                                   
Single value returned is a list of elements satisfying FORT-P."
  )

(defun argn-genl-of (collection argnum &optional mt)                                                   
  "Returns a list of the predicates for which COLLECTION is a                                                 
local genl constraint applied to the Nth argument.                                                         
ARGNUM must satisfy INTEGERP.                                                                              
Single value returned is a list of elements satisfying FORT-P."
  )

(defun defining-defns (col &optional mt)                                                               
  "Return a list of the local defining (necessary and sufficient definitions) of collection COL.              
Single value returned is a list of elements satisfying FORT-P."
  )

(defun necessary-defns (col &optional mt)                                                              
  "Return a list of the local necessary definitions of collection COL.                                        
Single value returned is a list of elements satisfying FORT-P."
  )

(defun sufficient-defns (col &optional mt)                                                             
  "Return a list of the local sufficient definitions of collection COL.                                       
Single value returned is a list of elements satisfying FORT-P."
  )

(defun all-sufficient-defns (col &optional mt)                                                         
  "Return a list of all sufficient definitions of collection COL.                                             
Single value returned is a list of elements satisfying FORT-P."
  )

(defun arity (relation &optional mt)                                                                   
  "Return the arity for relation constant RELATION."
  )

(defun result-isa (functor &optional mt)                                                               
  "Return a list of the collections that include as instances                                                 
the results of non-predicate function constant FUNCTOR.                                                    
Single value returned is a list of elements satisfying FORT-P."
  )

(defun creator (fort &optional mt)                                                                     
  "Identify the cyclist who created FORT.                                                                     
FORT must satisfy FORT-P.                                                                                  
MT must satisfy HLMT-P.                                                                                    
Single value returned satisfies FORT-P."
  )

(defun creation-time (fort &optional mt)                                                               
  "Identify when FORT was created.                                                                            
FORT must satisfy FORT-P.                                                                                  
MT must satisfy HLMT-P.                                                                                    
Single value returned satisfies INTEGERP."
  )

(defun comment (fort &optional mt)                                                                     
  "Return the comment string for FORT.                                                                        
FORT must satisfy FORT-P.                                                                                  
Single value returned satisfies STRINGP."
  )

(defun asserted-by (assertion)                                                                         
  "Returns the cyclist who asserted ASSERTION.                                                                
ASSERTION must satisfy ASSERTION-P."
  )

(defun asserted-when (assertion)                                                                       
  "Returns the date when ASSERTION was asserted.                                                              
ASSERTION must satisfy ASSERTION-P.                                                                        
Single value returned satisfies INTEGERP."
  )

(defun reviewer (fort &optional mt)                                                                    
  "Identify the cyclist who reviewed FORT.                                                                    
FORT must satisfy FORT-P.                                                                                  
MT must satisfy HLMT-P.                                                                                    
Single value returned satisfies FORT-P."
  )

(defun all-term-assertions (term &optional remove-duplicates?)                                         
  "Return a list of all the assertions indexed via the indexed term TERM.                                     
TERM must satisfy INDEXED-TERM-P.                                                                          
Single value returned is a list of elements satisfying ASSERTION-P."
  )

(defun isa-relevant-assertions (term &optional mt)                                                     
  "Return a list of all (e.g., inheritance) rules relevant to TERM                                            
by virtue of the collections of which it is an instance.                                                   
Single value returned is a list of elements satisfying ASSERTION-P."
  )

(defun assertions-mentioning-terms (term-list &optional include-meta-assertions?)                      
  "Return a list of assertions that mention every term in TERM-LIST.                                          
Single value returned is a list of elements satisfying ASSERTION-P."
  )

(defun primitive-collection? (fort &optional mt)                                                       
  "Return T iff FORT is a collection for which no classical (necessary and sufficient)                        
definition is known.                                                                                       
Single value returned satisfies BOOLEANP."
  )

(defun preds-for-pair (fort-1 fort-2 &optional mt mode)                                                
  "Return a list of the predicates relevant to relating FORT-1 and FORT-2.                                    
MODE may be :figurative (instances of collections),                                                        
:literal (constants), or                                                                                   
:mixed (referents).                                                                                        
FORT-1 must satisfy FORT-P.                                                                                
FORT-2 must satisfy FORT-P.                                                                                
Single value returned is a list of elements satisfying FORT-P."
  )

(defun collections-coextensional? (col-1 col-2 &optional mt)                                           
  "Are COL-1 and COL-2 coextensional?                                                                         
COL-1 must satisfy EL-FORT-P.                                                                              
COL-2 must satisfy EL-FORT-P.                                                                              
Single value returned satisfies BOOLEANP."
  )

(defun collections-disjoint? (col-1 col-2 &optional mt)                                                
  "Are collections COL-1 and COL-2 disjoint?                                                                  
- (uses only sbhl graphs: their extensions are not searched                                                  
nor are their necessary conditions analyzed)                                                               
COL-1 must satisfy EL-FORT-P.                                                                              
COL-2 must satisfy EL-FORT-P.                                                                              
Single value returned satisfies BOOLEANP."
  )

(defun collections-intersect? (col-1 col-2 &optional mt)                                               
  "Do collections COL-1 and COL-2 intersect?                                                                  
- (uses only sbhl graphs: their extensions are not searched                                                  
nor are their sufficient conditions analyzed)                                                              
COL-1 must satisfy EL-FORT-P.                                                                              
COL-2 must satisfy EL-FORT-P.                                                                              
Single value returned satisfies BOOLEANP."
  )

(defun count-all-instances (collection &optional mt tv)                                                
  "Counts the number of instances in the collection and then returns the count.                               
COLLECTION must satisfy EL-FORT-P.                                                                         
Single value returned is a list of elements satisfying FORT-P."
  )

(defun el-strength-p (object)                                                                          
  "Return T iff OBJECT is a valid CycL assertion strength                                                     
:default or :monotonic.                                                                                    
Single value returned satisfies BOOLEANP."
  )

(defun el-to-hl (formula &optional mt)                                                                 
  "Translate el expression FORMULA into its equivalent canonical hl expressions                               
FORMULA must satisfy EL-FORMULA-P."
  )

(defun el-to-hl-query (formula &optional mt)                                                           
  "Translate el query FORMULA into its equivalent hl expressions                                              
FORMULA must satisfy EL-FORMULA-P."
  )

(defun assertion-el-formula (assertion)                                                                
  "Return the el formula for ASSERTION                                                                        
ASSERTION must satisfy ASSERTION-P.                                                                        
Single value returned satisfies LISTP."
  )

(defun el-wff-syntax? (formula &optional mt)                                                           
  "Is FORMULA well-formed wrt syntax?                                                                         
Single value returned satisfies BOOLEANP."
  )

(defun el-wff-syntax+arity? (formula &optional mt)                                                     
  "Is FORMULA well-formed wrt syntax and arity?                                                               
Single value returned satisfies BOOLEANP."
  )

(defun ground? (expression &optional var?)                                                             
  "Returns whether EXPRESSION is free of any variables?                                                       
Single value returned satisfies BOOLEANP."
  )

(defun el-formula-ok? (formula &optional mt)                                                           
  "Is FORMULA a well-formed el formula?                                                                       
FORMULA must satisfy LISTP.                                                                                
Single value returned satisfies BOOLEANP."
  )

(defun el-query-ok? (formula &optional mt)                                                             
  "Is FORMULA a well-formed el query?                                                                         
FORMULA must satisfy LISTP.                                                                                
Single value returned satisfies BOOLEANP."
  )

(defun diagnose-el-formula (formula &optional mt io-mode)                                              
  "Identify how el formula FORMULA fails syntactic or semantic constraints                                    
FORMULA must satisfy LISTP."
  )

(defun min-isa (term &optional mt tv)                                                                  
  "Returns most-specific collections that include TERM (inexpensive)                                          
TERM must satisfy HL-TERM-P.                                                                               
Single value returned is a list of elements satisfying FORT-P."
  )

(defun max-not-isa (term &optional mt tv)                                                              
  "Returns most-general collections that do not include TERM (expensive)                                      
TERM must satisfy HL-TERM-P.                                                                               
Single value returned is a list of elements satisfying FORT-P."
  )

(defun all-isa (term &optional mt tv)                                                                  
  "Returns all collections that include TERM (inexpensive)                                                    
TERM must satisfy HL-TERM-P.                                                                               
Single value returned is a list of elements satisfying FORT-P."
  )

(defun union-all-isa (terms &optional mt tv)                                                           
  "Returns all collections that include any term in TERMS (inexpensive)                                       
TERMS must satisfy LISTP.                                                                                  
Single value returned is a list of elements satisfying FORT-P."
  )

(defun all-not-isa (term &optional mt tv)                                                              
  "Returns all collections that do not include TERM (expensive)                                               
TERM must satisfy HL-TERM-P.                                                                               
Single value returned is a list of elements satisfying FORT-P."
  )

(defun all-instances (col &optional mt tv)                                                             
  "Returns all instances of COLLECTION (expensive)                                                            
COL must satisfy EL-FORT-P.                                                                                
Single value returned is a list of elements satisfying FORT-P."
  )

(defun map-all-isa (fn term &optional mt tv)                                                           
  "Apply FUNCTION to every all-isa of TERM                                                                    
- (FUNCTION must not effect the current sbhl search state)                                                   
FN must satisfy FUNCTION-SPEC-P.                                                                           
TERM must satisfy HL-TERM-P."
  )

(defun any-wrt-all-isa (function term &optional mt tv)                                                 
  "Return the first encountered non-nil result of applying FUNCTION to the all-isa of TERM                    
- (FUNCTION may not effect the current sbhl search state)                                                    
- (defun must satisfy FUNCTION-SPEC-P.                                                                     
TERM must satisfy HL-TERM-P."
  )

(defun map-all-instances (fn col &optional mt tv)                                                      
  "Apply FUNCTION to each unique instance of all specs of COLLECTION.                                         
FN must satisfy FUNCTION-SPEC-P.                                                                           
COL must satisfy EL-FORT-P."
  )

(defun map-instances (function term &optional mt tv)                                                   
  "apply FUNCTION to every (least general) #$isa of TERM                                                      
- (defun must satisfy FUNCTION-SPEC-P.                                                                     
TERM must satisfy EL-FORT-P."
  )

(defun isa? (term collection &optional mt tv)                                                          
  "Returns whether TERM is an instance of COLLECTION                                                          
COLLECTION must satisfy EL-FORT-P.                                                                         
Single value returned satisfies BOOLEANP."
  )

(defun isa-any? (term collections &optional mt tv)                                                     
  "Returns whether TERM is an instance of any collection in COLLECTIONS                                       
TERM must satisfy HL-TERM-P.                                                                               
COLLECTIONS must satisfy LISTP.                                                                            
Single value returned satisfies BOOLEANP."
  )

(defun any-isa-any? (terms collections &optional mt tv)                                                
  "@return booleanp; whether any term in TERMS is an instance of any collection in COLLECTIONS                
TERMS must satisfy LISTP.                                                                                  
COLLECTIONS must satisfy LISTP.                                                                            
Single value returned satisfies BOOLEANP."
  )

(defun not-isa? (term collection &optional mt tv)                                                      
  "@return boolenap; whether TERM is known to not be an instance of COLLECTION                                
TERM must satisfy HL-TERM-P.                                                                               
COLLECTION must satisfy EL-FORT-P.                                                                         
Single value returned satisfies BOOLEANP."
  )

(defun instances? (collection &optional mt tv)                                                         
  "Returns whether COLLECTION has any direct instances                                                        
COLLECTION must satisfy EL-FORT-P.                                                                         
Single value returned satisfies BOOLEANP."
  )

(defun instances (col &optional mt tv)                                                                 
  "Returns the asserted instances of COL                                                                      
COL must satisfy EL-FORT-P.                                                                                
Single value returned is a list of elements satisfying FORT-P."
  )

(defun isa-siblings (term &optional mt tv)                                                             
  "Returns the direct isas of those collections of which TERM is a direct instance                            
TERM must satisfy HL-TERM-P.                                                                               
Single value returned is a list of elements satisfying FORT-P."
  )

(defun max-floor-mts-of-isa-paths (term collection &optional tv)                                       
  "Returns in what (most-genl) mts TERM is an instance of COLLECTION                                          
TERM must satisfy HL-TERM-P.                                                                               
COLLECTION must satisfy EL-FORT-P."
  )

(defun why-isa? (term collection &optional mt tv behavior)                                             
  "Returns justification of (isa TERM COLLECTION)                                                             
TERM must satisfy HL-TERM-P.                                                                               
COLLECTION must satisfy EL-FORT-P.                                                                         
Single value returned satisfies LISTP."
  )

(defun why-not-isa? (term collection &optional mt tv behavior)                                         
  "Returns justification of (not (isa TERM COLLECTION))                                                       
TERM must satisfy HL-TERM-P.                                                                               
COLLECTION must satisfy EL-FORT-P.                                                                         
Single value returned satisfies LISTP."
  )

(defun all-instances-among (col terms &optional mt tv)                                                 
  "Returns those elements of TERMS that include COL as an all-isa                                             
COL must satisfy HL-TERM-P.                                                                                
TERMS must satisfy LISTP.                                                                                  
Single value returned is a list of elements satisfying FORT-P."
  )

(defun all-isa-among (term collections &optional mt tv)                                                
  "Returns those elements of COLLECTIONS that include TERM as an all-instance                                 
TERM must satisfy HL-TERM-P.                                                                               
COLLECTIONS must satisfy LISTP.                                                                            
Single value returned is a list of elements satisfying FORT-P."
  )

(defun all-isas-wrt (term isa &optional mt tv)                                                         
  "Returns all isa of term TERM that are also instances of collection ISA (ascending transitive               
closure; inexpensive)                                                                                      
TERM must satisfy EL-FORT-P.                                                                               
ISA must satisfy EL-FORT-P.                                                                                
Single value returned is a list of elements satisfying FORT-P."
  )

(defun instance-siblings (term &optional mt tv)                                                        
  "Returns the direct instances of those collections having direct isa TERM                                   
TERM must satisfy EL-FORT-P.                                                                               
Single value returned is a list of elements satisfying FORT-P."
  )

(defun max-instances (col &optional mt tv)                                                             
  "Returns the maximal among the asserted instances of COL                                                    
COL must satisfy EL-FORT-P.                                                                                
Single value returned is a list of elements satisfying FORT-P."
  )

(defun min-not-instances (col &optional mt tv)                                                         
  "Returns the most-specific negated instances of collection COL                                              
COL must satisfy EL-FORT-P.                                                                                
Single value returned is a list of elements satisfying FORT-P."
  )

(defun not-isa-among (term collections &optional mt tv)                                                
  "Returns those elements of COLLECTIONS that do NOT include TERM                                             
TERM must satisfy HL-TERM-P.                                                                               
COLLECTIONS must satisfy LISTP.                                                                            
Single value returned is a list of elements satisfying FORT-P."
  )

(defun union-all-instances (cols &optional mt tv)                                                      
  "Returns set of all instances of each collection in COLS (expensive)                                        
COLS must satisfy LISTP.                                                                                   
Single value returned is a list of elements satisfying FORT-P."
  )

(defun min-genls (col &optional mt tv)                                                                 
  "Returns the most-specific genls of collection COL                                                          
Single value returned is a list of elements satisfying FORT-P."
  )

(defun max-not-genls (col &optional mt tv)                                                             
  "Returns the least-specific negated genls of collection COL                                                 
COL must satisfy EL-FORT-P.                                                                                
Single value returned is a list of elements satisfying FORT-P."
  )

(defun max-specs (col &optional mt tv)                                                                 
  "Returns the least-specific specs of collection COL                                                         
COL must satisfy EL-FORT-P.                                                                                
Single value returned is a list of elements satisfying FORT-P."
  )

(defun min-not-specs (col &optional mt tv)                                                             
  "Returns the most-specific negated specs of collection COL                                                  
COL must satisfy EL-FORT-P.                                                                                
Single value returned is a list of elements satisfying FORT-P."
  )

(defun genl-siblings (col &optional mt tv)                                                             
  "Returns the direct genls of those direct spec collections of COL                                           
COL must satisfy EL-FORT-P.                                                                                
Single value returned is a list of elements satisfying FORT-P."
  )

(defun spec-siblings (col &optional mt tv)                                                             
  "Returns the direct specs of those direct genls collections of COL                                          
COL must satisfy EL-FORT-P.                                                                                
Single value returned is a list of elements satisfying FORT-P."
  )

(defun all-genls (col &optional mt tv)                                                                 
  "Returns all genls of collection COL                                                                        
- (ascending transitive closure; inexpensive)                                                                
COL must satisfy EL-FORT-P.                                                                                
Single value returned is a list of elements satisfying FORT-P."
  )

(defun all-genls-wrt (spec genl &optional mt tv)                                                       
  "Returns all genls of collection SPEC that are also specs of collection GENL (ascending transitive          
closure; inexpensive)                                                                                      
SPEC must satisfy EL-FORT-P.                                                                               
GENL must satisfy EL-FORT-P.                                                                               
Single value returned is a list of elements satisfying FORT-P."
  )

(defun union-all-genls (cols &optional mt tv)                                                          
  "Returns all genls of each collection in COLs                                                               
COLS must satisfy LISTP.                                                                                   
Single value returned is a list of elements satisfying FORT-P."
  )

(defun all-genls-if (function col &optional mt tv)                                                     
  "Returns all genls of collection COL that satisfy FUNCTION                                                  
- (FUNCTION must not effect sbhl search state)                                                               
- (defun must satisfy FUNCTION-SPEC-P.                                                                     
COL must satisfy EL-FORT-P.                                                                                
Single value returned is a list of elements satisfying FORT-P."
  )

(defun all-not-genls (col &optional mt tv)                                                             
  "Returns all negated genls of collection COL                                                                
- (descending transitive closure; expensive)                                                                 
COL must satisfy EL-FORT-P.                                                                                
Single value returned is a list of elements satisfying FORT-P."
  )

(defun all-specs (col &optional mt tv)                                                                 
  "Returns all specs of collection COL                                                                        
- (descending transitive closure; expensive)                                                                 
COL must satisfy EL-FORT-P.                                                                                
Single value returned is a list of elements satisfying FORT-P."
  )

(defun all-specs-if (function col &optional mt tv)                                                     
  "Returns all genls of collection COL that satisfy FUNCTION                                                  
- (FUNCTION must not effect sbhl search state)                                                               
- (defun must satisfy FUNCTION-SPEC-P.                                                                     
COL must satisfy EL-FORT-P.                                                                                
Single value returned is a list of elements satisfying FORT-P."
  )

(defun all-dependent-specs (col &optional mt tv)                                                       
  "Returns all specs s of COL s.t. every path connecting                                                      
s to any genl of COL must pass through COL                                                                 
COL must satisfy EL-FORT-P.                                                                                
Single value returned is a list of elements satisfying FORT-P."
  )

(defun union-all-specs (cols &optional mt tv)                                                          
  "Returns all specs of each collection in COLs                                                               
COLS must satisfy LISTP.                                                                                   
Single value returned is a list of elements satisfying FORT-P."
  )

(defun genl? (spec genl &optional mt tv)                                                               
  "Returns whether (#$genls SPEC GENL) can be inferred.                                                       
- (ascending transitive search; inexpensive)                                                                 
SPEC must satisfy EL-FORT-P.                                                                               
GENL must satisfy EL-FORT-P.                                                                               
Single value returned satisfies BOOLEANP."
  )

(defun spec? (genl spec &optional mt tv)                                                               
  "Returns whether (#$genls SPEC GENL) can be inferred.                                                       
- (ascending transitive search; inexpensive)                                                                 
GENL must satisfy EL-FORT-P.                                                                               
SPEC must satisfy EL-FORT-P.                                                                               
Single value returned satisfies BOOLEANP."
  )

(defun any-genl? (spec genls &optional mt tv)                                                          
  "(any-genl? spec genls) is t iff (genl? spec genl) for some genl in genls                                   
- (ascending transitive search; inexpensive)                                                                 
SPEC must satisfy EL-FORT-P.                                                                               
GENLS must satisfy LISTP.                                                                                  
Single value returned satisfies BOOLEANP."
  )

(defun all-genl? (spec genls &optional mt tv)                                                          
  "Returns T iff (genl? spec genl) for every genl in GENLS                                                    
- (ascending transitive search; inexpensive)                                                                 
SPEC must satisfy EL-FORT-P.                                                                               
GENLS must satisfy LISTP.                                                                                  
Single value returned satisfies BOOLEANP."
  )

(defun any-spec? (genl specs &optional mt tv)                                                          
  "Returns T iff (spec? genl spec) for some spec in SPECS                                                     
GENL must satisfy EL-FORT-P.                                                                               
SPECS must satisfy LISTP.                                                                                  
Single value returned satisfies BOOLEANP."
  )

(defun all-spec? (genl specs &optional mt tv)                                                          
  "Returns T iff (spec? genl spec) for every spec in SPECS                                                    
GENL must satisfy EL-FORT-P.                                                                               
SPECS must satisfy LISTP.                                                                                  
Single value returned satisfies BOOLEANP."
  )

(defun any-genl-any? (specs genls &optional mt tv)                                                     
  "Return T iff (genl? spec genl mt) for any spec in SPECS, genl in GENLS                                     
SPECS must satisfy LISTP.                                                                                  
GENLS must satisfy LISTP.                                                                                  
Single value returned satisfies BOOLEANP."
  )

(defun any-genl-all? (specs genls &optional mt tv)                                                     
  "Return T iff (genl? spec genl mt) for any spec in SPECS and all genl in GENLS                              
SPECS must satisfy LISTP.                                                                                  
GENLS must satisfy LISTP.                                                                                  
Single value returned satisfies BOOLEANP."
  )

(defun not-genl? (col not-genl &optional mt tv)                                                        
  "Return whether collection NOT-GENL is not a genl of COL.                                                   
COL must satisfy EL-FORT-P.                                                                                
NOT-GENL must satisfy EL-FORT-P.                                                                           
Single value returned satisfies BOOLEANP."
  )

(defun any-not-genl? (col not-genls &optional mt tv)                                                   
  "Returns whether any collection in NOT-GENLS is not a genl of COL.                                          
COL must satisfy EL-FORT-P.                                                                                
NOT-GENLS must satisfy LISTP.                                                                              
Single value returned satisfies BOOLEANP."
  )

(defun collection-leaves (col &optional mt tv)                                                         
  "Returns the minimally-general (the most specific) among all-specs of COL                                   
COL must satisfy EL-FORT-P.                                                                                
Single value returned is a list of elements satisfying FORT-P."
  )

(defun min-cols (cols &optional mt tv)                                                                 
  "Returns the minimally-general (the most specific) among reified collections COLS                           
COLS must satisfy LISTP.                                                                                   
Single value returned is a list of elements satisfying FORT-P."
  )

(defun max-cols (cols &optional mt tv)                                                                 
  "Returns the most-general among reified collections COLS                                                    
COLS must satisfy LISTP.                                                                                   
Single value returned is a list of elements satisfying FORT-P."
  )

(defun min-ceiling-cols (cols &optional candidates mt tv)                                              
  "Returns the most specific common generalizations among reified collections COLS                            
- (if CANDIDATES is non-nil, then result is a subset of CANDIDATES)                                          
COLS must satisfy LISTP.                                                                                   
Single value returned is a list of elements satisfying FORT-P."
  )

(defun max-floor-cols (cols &optional candidates mt tv)                                                
  "Returns the most general common specializations among reified collections COLS                             
- (if CANDIDATES is non-nil, then result is a subset of CANDIDATES)                                          
COLS must satisfy LISTP.                                                                                   
Single value returned is a list of elements satisfying FORT-P."
  )

(defun any-disjoint-collection-pair (cols &optional mt)                                                
  "Returns a pair of disjoint elements of COLS (if any exist)                                                 
COLS must satisfy LISTP.                                                                                   
Single value returned is a list of elements satisfying FORT-P."
  )

(defun any-disjoint-collection-pair? (cols &optional mt)                                               
  "Are any two collections in COLS disjoint?                                                                  
COLS must satisfy LISTP.                                                                                   
Single value returned satisfies BOOLEANP."
  )

(defun why-collections-disjoint? (c1 c2 &optional mt)                                                  
  "Justification of (disjointWith C1 C2)                                                                      
C1 must satisfy EL-FORT-P.                                                                                 
C2 must satisfy EL-FORT-P.                                                                                 
Single value returned satisfies LISTP."
  )

(defun any-genl-isa (col isa &optional mt tv)                                                          
  "Return some genl of COL that isa instance of ISA (if any such genl exists)                                 
COL must satisfy EL-FORT-P.                                                                                
ISA must satisfy EL-FORT-P.                                                                                
Single value returned satisfies FORT-P."
  )

(defun lighter-col (col-a col-b)                                                                       
  "Return COL-B iff it has fewer specs than COL-A, else return COL-A                                          
COL-A must satisfy EL-FORT-P.                                                                              
COL-B must satisfy EL-FORT-P.                                                                              
Single value returned satisfies FORT-P."
  )

(defun shallower-col (col-a col-b)                                                                     
  "Return COL-B iff it has fewer genls than COL-A, else return COL-A                                          
COL-A must satisfy EL-FORT-P.                                                                              
COL-B must satisfy EL-FORT-P.                                                                              
Single value returned satisfies FORT-P."
  )

(defun max-floor-mts-of-genls-paths (spec genl &optional tv)                                           
  "@return listp; Returns in what (most-genl) mts GENL is a genls of SPEC                                     
SPEC must satisfy EL-FORT-P.                                                                               
GENL must satisfy EL-FORT-P."
  )

(defun why-not-assert-genls? (spec genl &optional mt)                                                  
  "Justification of why asserting (genls SPEC GENL) is not consistent                                         
SPEC must satisfy EL-FORT-P.                                                                               
GENL must satisfy EL-FORT-P.                                                                               
Single value returned satisfies LISTP."
  )

(defun map-all-genls (fn col &optional mt tv)                                                          
  "Applies FN to every (all) genls of COL                                                                     
- (FN must not effect the current sbhl space)                                                                
FN must satisfy FUNCTION-SPEC-P.                                                                           
COL must satisfy EL-FORT-P."
  )

(defun any-all-genls (fn col &optional mt tv)                                                          
  "Return a non-nil result of applying FN to some all-genl of COL                                             
- (FN must not effect the current sbhl space)                                                                
FN must satisfy FUNCTION-SPEC-P.                                                                           
COL must satisfy EL-FORT-P."
  )

(defun map-all-specs (fn col &optional mt tv)                                                          
  "Applies FN to every (all) specs of COL                                                                     
- (FN must not effect the current sbhl space)                                                                
FN must satisfy FUNCTION-SPEC-P.                                                                           
COL must satisfy EL-FORT-P."
  )

(defun any-all-specs (fn col &optional mt tv)                                                          
  "Return a non-nil result of applying FN to some all-spec of COL                                             
- (FN must not effect the current sbhl space)                                                                
FN must satisfy FUNCTION-SPEC-P.                                                                           
COL must satisfy EL-FORT-P."
  )

(defun all-genls-among (col candidates &optional mt tv)                                                
  "Returns those genls of COL that are included among CANDIDATEs                                              
COL must satisfy EL-FORT-P.                                                                                
CANDIDATES must satisfy LISTP.                                                                             
Single value returned is a list of elements satisfying FORT-P."
  )

(defun all-not-spec? (col not-specs &optional mt tv)                                                   
  "Return whether every collection in NOT-SPECS is not a spec of COL.                                         
COL must satisfy EL-FORT-P.                                                                                
NOT-SPECS must satisfy LISTP.                                                                              
Single value returned satisfies BOOLEANP."
  )

(defun all-not-specs (col &optional mt tv)                                                             
  "Returns all negated specs of collection COL                                                                
- (ascending transitive closure; inexpensive)                                                                
COL must satisfy EL-FORT-P.                                                                                
Single value returned is a list of elements satisfying FORT-P."
  )

(defun all-spec-any? (specs genls &optional mt tv)                                                     
  "Return T iff for each spec in SPECS there is some genl in GENLS s.t. (genl? spec genl mt)                  
SPECS must satisfy LISTP.                                                                                  
GENLS must satisfy LISTP.                                                                                  
Single value returned satisfies BOOLEANP."
  )

(defun all-specs-among (col candidates &optional mt tv)                                                
  "Returns those specs of COL that are included among CANDIDATEs                                              
COL must satisfy EL-FORT-P.                                                                                
CANDIDATES must satisfy LISTP.                                                                             
Single value returned is a list of elements satisfying FORT-P."
  )

(defun spec-siblings (col &optional mt tv)                                                             
  "Returns the direct specs of those direct genls collections of COL                                          
COL must satisfy EL-FORT-P.                                                                                
Single value returned is a list of elements satisfying FORT-P."
  )

(defun why-genl? (spec genl &optional mt tv behavior)                                                  
  "Justification of (genls SPEC GENL)                                                                         
SPEC must satisfy EL-FORT-P.                                                                               
GENL must satisfy EL-FORT-P.                                                                               
Single value returned satisfies LISTP."
  )

(defun why-not-genl? (spec genl &optional mt tv behavior)                                              
  "Justification of (not (genls SPEC GENL))                                                                   
SPEC must satisfy EL-FORT-P.                                                                               
GENL must satisfy EL-FORT-P.                                                                               
Single value returned satisfies LISTP."
  )

(defun min-genl-predicates (pred &optional mt tv)                                                      
  "Returns the most-specific local genlPreds of PRED                                                          
PRED must satisfy FORT-P.                                                                                  
Single value returned is a list of elements satisfying FORT-P."
  )

(defun min-genl-inverses (pred &optional mt tv)                                                        
  "Returns the most-specific local genlInverses of PRED                                                       
PRED must satisfy FORT-P.                                                                                  
Single value returned is a list of elements satisfying FORT-P."
  )

(defun max-not-genl-predicates (pred &optional mt tv)                                                  
  "Returns the most-general local negated genlPreds of PRED                                                   
PRED must satisfy FORT-P.                                                                                  
Single value returned is a list of elements satisfying FORT-P."
  )

(defun max-not-genl-inverses (pred &optional mt tv)                                                    
  "Returns the most-general local negated genlPreds of PRED                                                   
PRED must satisfy FORT-P.                                                                                  
Single value returned is a list of elements satisfying FORT-P."
  )

(defun max-spec-predicates (pred &optional mt tv)                                                      
  "Returns the most-general specPreds of PRED                                                                 
PRED must satisfy FORT-P.                                                                                  
Single value returned is a list of elements satisfying FORT-P."
  )

(defun max-spec-inverses (pred &optional mt tv)                                                        
  "Returns the most-general specInverses of PRED                                                              
PRED must satisfy FORT-P.                                                                                  
Single value returned is a list of elements satisfying FORT-P."
  )

(defun min-not-spec-predicates (pred &optional mt tv)                                                  
  "Returns the most-specific negated specPreds of PRED                                                        
PRED must satisfy FORT-P.                                                                                  
Single value returned is a list of elements satisfying FORT-P."
  )

(defun min-not-spec-inverses (pred &optional mt tv)                                                    
  "Returns the most-specific negated specPreds of PRED                                                        
PRED must satisfy FORT-P.                                                                                  
Single value returned is a list of elements satisfying FORT-P."
  )

(defun genl-predicate-siblings (pred &optional mt tv)                                                  
  "Returns the direct #$genlPreds of those predicates having direct spec-preds PRED                           
PRED must satisfy FORT-P.                                                                                  
Single value returned is a list of elements satisfying FORT-P."
  )

(defun genl-inverse-siblings (pred &optional mt tv)                                                    
  "Returns the direct #$genlInverse of those predicates having direct spec-inverses PRED                      
PRED must satisfy FORT-P.                                                                                  
Single value returned is a list of elements satisfying FORT-P."
  )

(defun spec-predicate-siblings (pred &optional mt tv)                                                  
  "Returns the direct spec-preds of those collections having direct #$genlPreds PRED                          
PRED must satisfy FORT-P.                                                                                  
Single value returned is a list of elements satisfying FORT-P."
  )

(defun spec-inverse-siblings (pred &optional mt tv)                                                    
  "Returns the direct spec-inverses of those collections having direct #$genlInverse PRED                     
PRED must satisfy FORT-P.                                                                                  
Single value returned is a list of elements satisfying FORT-P."
  )

(defun all-genl-predicates (pred &optional mt tv)                                                      
  "Returns all genlPreds of predicate PRED                                                                    
- (ascending transitive closure; inexpensive)                                                                
PRED must satisfy FORT-P.                                                                                  
Single value returned is a list of elements satisfying FORT-P."
  )

(defun all-genl-inverses (pred &optional mt tv)                                                        
  "Returns all genlPreds of predicate PRED                                                                    
- (ascending transitive closure; inexpensive)                                                                
PRED must satisfy FORT-P.                                                                                  
Single value returned is a list of elements satisfying FORT-P."
  )

(defun all-not-genl-predicates (pred &optional mt tv)                                                  
  "Returns all negated genlPreds of predicate PRED                                                            
- (descending transitive closure; expensive)                                                                 
PRED must satisfy FORT-P.                                                                                  
Single value returned is a list of elements satisfying FORT-P."
  )

(defun all-not-genl-inverses (pred &optional mt tv)                                                    
  "Returns all negated genlPreds of predicate PRED                                                            
- (descending transitive closure; expensive)                                                                 
PRED must satisfy FORT-P.                                                                                  
Single value returned is a list of elements satisfying FORT-P."
  )

(defun all-spec-predicates (pred &optional mt tv)                                                      
  "Returns all predicates having PRED as a genlPred                                                           
- (descending transitive closure; expensive)                                                                 
PRED must satisfy FORT-P.                                                                                  
Single value returned is a list of elements satisfying FORT-P."
  )

(defun all-spec-inverses (pred &optional mt tv)                                                        
  "Returns all predicates having PRED as a genlInverse                                                        
- (descending transitive closure; expensive)                                                                 
PRED must satisfy FORT-P.                                                                                  
Single value returned is a list of elements satisfying FORT-P."
  )

(defun all-spec-preds-wrt-type (pred col arg &optional mt tv)                                          
  "Returns those all-spec-preds of PRED for which instances                                                   
of COL are legal in arguments in position ARG                                                              
PRED must satisfy FORT-P.                                                                                  
COL must satisfy FORT-P.                                                                                   
ARG must satisfy INTEGERP.                                                                                 
Single value returned is a list of elements satisfying FORT-P."
  )

(defun all-spec-preds-wrt-arg (pred fort arg &optional mt tv)                                          
  "Returns those all-spec-preds of PRED for which FORT                                                        
is legal as arguments in position ARG                                                                      
PRED must satisfy FORT-P.                                                                                  
FORT must satisfy FORT-P.                                                                                  
ARG must satisfy INTEGERP.                                                                                 
Single value returned is a list of elements satisfying FORT-P."
  )

(defun all-not-spec-predicates (pred &optional mt tv)                                                  
  "Returns all negated specPreds of predicate PRED                                                            
- (ascending transitive closure; inexpensive)                                                                
PRED must satisfy FORT-P.                                                                                  
Single value returned is a list of elements satisfying FORT-P."
  )

(defun all-not-spec-inverses (pred &optional mt tv)                                                    
  "Returns all predicates having PRED as a negated genlInverse                                                
- (ascending transitive closure; inexpensive)                                                                
PRED must satisfy FORT-P.                                                                                  
Single value returned is a list of elements satisfying FORT-P."
  )

(defun spec-predicate? (genl spec &optional mt tv)                                                     
  "Is GENL a genlPred of SPEC?                                                                                
- (ascending transitive search; inexpensive)                                                                 
GENL must satisfy FORT-P.                                                                                  
SPEC must satisfy FORT-P.                                                                                  
Single value returned satisfies BOOLEANP."
  )

(defun genl-predicate? (spec genl &optional mt tv)                                                     
  "Is GENL a genlPred of SPEC?                                                                                
- (ascending transitive search; inexpensive)                                                                 
SPEC must satisfy FORT-P.                                                                                  
GENL must satisfy FORT-P.                                                                                  
Single value returned satisfies BOOLEANP."
  )

(defun genl-inverse? (spec genl &optional mt tv)                                                       
  "Is GENL a genlInverse of SPEC?                                                                             
- (ascending transitive search; inexpensive)                                                                 
SPEC must satisfy FORT-P.                                                                                  
GENL must satisfy FORT-P.                                                                                  
Single value returned satisfies BOOLEANP."
  )

(defun spec-inverse? (genl spec &optional mt tv)                                                       
  "Is GENL a genlInverse of SPEC?                                                                             
- (ascending transitive search; inexpensive)                                                                 
SPEC must satisfy FORT-P.                                                                                  
GENL must satisfy FORT-P.                                                                                  
Single value returned satisfies BOOLEANP."
  )

(defun any-genl-predicate? (spec genls &optional mt tv)                                                
  "Returns T iff (genl-predicate? SPEC GENL) for some genl in GENLS                                           
- (ascending transitive search; inexpensive)                                                                 
SPEC must satisfy FORT-P.                                                                                  
GENLS must satisfy LISTP.                                                                                  
Single value returned satisfies BOOLEANP."
  )

(defun not-genl-predicate? (spec not-genl &optional mt tv)                                             
  "Is NOT-GENL knwon to be not a genlPred of SPEC?                                                            
- (descending transitive search; expensive)                                                                  
SPEC must satisfy FORT-P.                                                                                  
NOT-GENL must satisfy FORT-P.                                                                              
Single value returned satisfies BOOLEANP."
  )

(defun not-genl-inverse? (spec not-genl &optional mt tv)                                               
  "Is NOT-GENL a negated genlInverse of SPEC?                                                                 
- (descending transitive search; expensive)                                                                  
SPEC must satisfy FORT-P.                                                                                  
NOT-GENL must satisfy FORT-P.                                                                              
Single value returned satisfies BOOLEANP."
  )

(defun any-not-genl-predicate? (pred not-genls &optional mt tv)                                        
  "Is any predicate in NOT-GENLS not a genlPred of PRED?                                                      
- (descending transitive search; expensive)                                                                  
PRED must satisfy FORT-P.                                                                                  
NOT-GENLS must satisfy LISTP.                                                                              
Single value returned satisfies BOOLEANP."
  )

(defun intersecting-predicates? (pred1 pred2 &optional mt)                                             
  "Does the extension of PRED1 include some tuple in the extension of PRED2?                                  
PRED1 must satisfy FORT-P.                                                                                 
PRED2 must satisfy FORT-P.                                                                                 
Single value returned satisfies BOOLEANP."
  )

(defun why-genl-predicate? (spec genl &optional mt tv behavior)                                        
  "A justification of (genlPreds SPEC GENL)                                                                   
SPEC must satisfy FORT-P.                                                                                  
GENL must satisfy FORT-P.                                                                                  
Single value returned satisfies LISTP."
  )

(defun why-not-genl-predicate? (spec genl &optional mt tv behavior)                                    
  "A justification of (not (genlPreds SPEC GENL))                                                             
SPEC must satisfy FORT-P.                                                                                  
GENL must satisfy FORT-P.                                                                                  
Single value returned satisfies LISTP."
  )

(defun why-genl-inverse? (pred genl-inverse &optional mt tv behavior)                                  
  "A justification of (genlInverse PRED GENL-INVERSE)                                                         
PRED must satisfy FORT-P.                                                                                  
GENL-INVERSE must satisfy FORT-P.                                                                          
Single value returned satisfies LISTP."
  )

(defun why-not-genl-inverse? (spec genl &optional mt tv behavior)                                      
  "A justification of (not (genlInverse SPEC GENL)                                                            
SPEC must satisfy FORT-P.                                                                                  
GENL must satisfy FORT-P.                                                                                  
Single value returned satisfies LISTP."
  )

(defun min-predicates (preds &optional mt tv)                                                          
  "Returns the most-specific predicates in PREDS                                                              
PREDS must satisfy LISTP.                                                                                  
Single value returned is a list of elements satisfying FORT-P."
  )

(defun max-predicates (preds &optional mt tv)                                                          
  "Returns the most-specific predicates in PREDS                                                              
PREDS must satisfy LISTP.                                                                                  
Single value returned is a list of elements satisfying FORT-P."
  )

(defun min-ceiling-predicates (preds &optional candidates mt tv)                                       
  "Returns the most-specific common generalizations (ceilings) of PREDS                                       
PREDS must satisfy LISTP.                                                                                  
Single value returned is a list of elements satisfying FORT-P."
  )

(defun max-floor-predicates (preds &optional candidates mt tv)                                         
  "Returns the most-general common specializations (floors) of PREDS                                          
PREDS must satisfy LISTP.                                                                                  
Single value returned is a list of elements satisfying FORT-P."
  )

(defun max-floor-mts-of-genl-predicate-paths (spec genl &optional tv)                                  
  "@return listp; In what (most-genl) mts is GENL a genlPred of SPEC?                                         
SPEC must satisfy FORT-P.                                                                                  
GENL must satisfy FORT-P."
  )

(defun max-floor-mts-of-genl-inverse-paths (spec genl-inverse &optional tv)                            
  "In what (most-genl) mts is GENL-INVERSE a genlInverse of SPEC?                                             
SPEC must satisfy FORT-P.                                                                                  
GENL-INVERSE must satisfy FORT-P."
  )

(defun map-all-genl-preds (pred fn &optional mt tv)                                                    
  "Apply FN to each genlPred of PRED                                                                          
PRED must satisfy FORT-P.                                                                                  
FN must satisfy FUNCTION-SPEC-P."
  )

(defun some-all-genl-preds (pred fn &optional mt tv)                                                   
  "Apply FN to each genlPred of PRED until FN returns a non-nil result                                        
PRED must satisfy FORT-P.                                                                                  
FN must satisfy FUNCTION-SPEC-P."
  )

(defun map-all-spec-preds (pred fn &optional mt tv)                                                    
  "Apply FN to each genlPred of PRED                                                                          
PRED must satisfy FORT-P.                                                                                  
FN must satisfy FUNCTION-SPEC-P."
  )

(defun some-all-spec-preds (pred fn &optional mt tv)                                                   
  "Apply FN to each genlPred of PRED until FN returns a non-nil result                                        
PRED must satisfy FORT-P.                                                                                  
FN must satisfy FUNCTION-SPEC-P."
  )

(defun all-genl-preds-among (pred candidates &optional mt tv)                                          
  "Returns those genlPreds of PRED that are included among CANDIDATEs                                         
PRED must satisfy EL-FORT-P.                                                                               
CANDIDATES must satisfy LISTP.                                                                             
Single value returned is a list of elements satisfying FORT-P."
  )

(defun genl-predicates (pred &optional mt tv)                                                          
  "Returns the local genlPreds of PRED                                                                        
PRED must satisfy FORT-P.                                                                                  
Single value returned is a list of elements satisfying FORT-P."
  )

(defun genl-inverses (pred &optional mt tv)                                                            
  "Returns the local genlInverses of PRED                                                                     
PRED must satisfy FORT-P.                                                                                  
Single value returned is a list of elements satisfying FORT-P."
  )

(defun not-genl-inverses (pred &optional mt tv)                                                        
  "Returns the local negated genlPreds of PRED                                                                
PRED must satisfy FORT-P.                                                                                  
Single value returned is a list of elements satisfying FORT-P."
  )

(defun not-genl-predicates (pred &optional mt tv)                                                      
  "Returns the local negated genlPreds of PRED                                                                
PRED must satisfy FORT-P.                                                                                  
Single value returned is a list of elements satisfying FORT-P."
  )

(defun not-spec-inverses (pred &optional mt tv)                                                        
  "Returns the most-specific negated specPreds of PRED                                                        
PRED must satisfy FORT-P.                                                                                  
Single value returned is a list of elements satisfying FORT-P."
  )

(defun not-spec-predicates (pred &optional mt tv)                                                      
  "Returns the negated specPreds of PRED                                                                      
PRED must satisfy FORT-P.                                                                                  
Single value returned is a list of elements satisfying FORT-P."
  )

(defun some-all-genl-inverses (pred fn &optional mt tv)                                                
  "Apply FN to each genlPred of PRED until FN returns a non-nil result                                        
PRED must satisfy FORT-P.                                                                                  
FN must satisfy FUNCTION-SPEC-P."
  )

(defun spec-inverses (pred &optional mt tv)                                                            
  "Returns the specInverses of PRED                                                                           
PRED must satisfy FORT-P.                                                                                  
Single value returned is a list of elements satisfying FORT-P."
  )

(defun spec-predicates (pred &optional mt tv)                                                          
  "Returns the specPreds of PRED                                                                              
PRED must satisfy FORT-P.                                                                                  
Single value returned is a list of elements satisfying FORT-P."
  )

(defun union-all-genl-inverses (preds &optional mt tv)                                                 
  "Returns all genl-inverses of each collection in Preds                                                      
PREDS must satisfy LISTP.                                                                                  
Single value returned is a list of elements satisfying FORT-P."
  )

(defun union-all-genl-predicates (preds &optional mt tv)                                               
  "Returns all genl-predicates of each collection in Preds                                                    
PREDS must satisfy LISTP.                                                                                  
Single value returned is a list of elements satisfying FORT-P."
  )

(defun union-all-spec-inverses (preds &optional mt tv)                                                 
  "Returns all specs of each collection in Preds                                                              
PREDS must satisfy LISTP.                                                                                  
Single value returned is a list of elements satisfying FORT-P."
  )

(defun union-all-spec-predicates (preds &optional mt tv)                                               
  "Returns all spec-predicates of each collection in Preds                                                    
PREDS must satisfy LISTP.                                                                                  
Single value returned is a list of elements satisfying FORT-P."
  )

(defun gt-all-accessible (predicate fort &optional mt)                                                 
  "Returns all superiors and all inferiors of FORT via PREDICATE                                              
PREDICATE must satisfy FORT-P.                                                                             
FORT must satisfy FORT-P.                                                                                  
Single value returned is a list of elements satisfying FORT-P."
  )

(defun gt-all-dependent-inferiors (predicate fort &optional mt)                                        
  "Returns all inferiors i of FORT s.t. every path connecting i to                                            
any superior of FORT must pass through FORT                                                                
PREDICATE must satisfy FORT-P.                                                                             
FORT must satisfy FORT-P.                                                                                  
Single value returned is a list of elements satisfying FORT-P."
  )

(defun gt-all-inferiors (predicate fort &optional mt)                                                  
  "Returns all inferiors of FORT via PREDICATE                                                                
PREDICATE must satisfy FORT-P.                                                                             
FORT must satisfy FORT-P.                                                                                  
Single value returned is a list of elements satisfying FORT-P."
  )

(defun gt-all-superiors (predicate fort &optional mt)                                                  
  "Returns all superiors of FORT via PREDICATE                                                                
PREDICATE must satisfy FORT-P.                                                                             
FORT must satisfy FORT-P.                                                                                  
Single value returned is a list of elements satisfying FORT-P."
  )

(defun gt-any-superior-path (predicate inferior superior &optional mt)                                 
  "Returns list of nodes connecting INFERIOR with SUPERIOR                                                    
PREDICATE must satisfy FORT-P.                                                                             
INFERIOR must satisfy FORT-P.                                                                              
SUPERIOR must satisfy FORT-P.                                                                              
Single value returned is a list of elements satisfying FORT-P."
  )

(defun gt-co-inferiors (predicate fort &optional mt)                                                   
  "Returns sibling direct-inferiors of direct-superiors of FORT via PREDICATE, excluding FORT itself          
PREDICATE must satisfy FORT-P.                                                                             
FORT must satisfy FORT-P.                                                                                  
Single value returned is a list of elements satisfying FORT-P."
  )

(defun gt-co-superiors (predicate fort &optional mt)                                                   
  "Returns sibling direct-superiors of direct-inferiors of FORT via PREDICATE, excluding FORT itself          
PREDICATE must satisfy FORT-P.                                                                             
FORT must satisfy FORT-P.                                                                                  
Single value returned is a list of elements satisfying FORT-P."
  )

(defun gt-completes-cycle? (predicate fort1 fort2 &optional mt)                                        
  "Returns whether a transitive path connect FORT2 to FORT1,                                                  
or whether a transitive inverse path connect FORT1 to FORT2?                                               
PREDICATE must satisfy FORT-P.                                                                             
FORT1 must satisfy FORT-P.                                                                                 
FORT2 must satisfy FORT-P.                                                                                 
Single value returned satisfies BOOLEANP."
  )

(defun gt-compose-fn-all-inferiors (predicate fort fn &optional combine-fn mt)                         
  "Apply fn to each inferior of FORT;                                                                         
fn takes a fort as its only arg, and                                                                       
it must not effect the search status of each fort it visits                                                
PREDICATE must satisfy FORT-P.                                                                             
FORT must satisfy FORT-P.                                                                                  
FN must satisfy FUNCTION-SPEC-P."
  )

(defun gt-compose-fn-all-superiors (predicate fort fn &optional combine-fn mt)                         
  "Apply fn to each superior of FORT;                                                                         
fn takes a fort as its only arg, and must not effect the search status of each                             
fort it visits                                                                                             
PREDICATE must satisfy FORT-P.                                                                             
FORT must satisfy FORT-P.                                                                                  
FN must satisfy FUNCTION-SPEC-P."
  )

(defun gt-compose-pred-all-inferiors (predicate fort compose-pred &optional compose-index-arg          
						"compose-gather-arg mt)                                                                                     
Returns all nodes accessible by COMPOSE-PRED from each inferior of FORT along                              
transitive PREDICATE                                                                                       
PREDICATE must satisfy FORT-P.                                                                             
FORT must satisfy FORT-P.                                                                                  
COMPOSE-PRED must satisfy PREDICATE-IN-ANY-MT?.                                                            
Single value returned is a list of elements satisfying FORT-P."
						)

  )
(defun gt-compose-pred-all-superiors (predicate fort compose-pred &optional compose-index-arg          
						)
  "compose-gather-arg mt)                                                                                     
Returns all nodes accessible by COMPOSE-PRED from each superior of FORT along                              
transitive PREDICATE                                                                                       
PREDICATE must satisfy FORT-P.                                                                             
FORT must satisfy FORT-P.                                                                                  
COMPOSE-PRED must satisfy PREDICATE-IN-ANY-MT?.                                                            
Single value returned is a list of elements satisfying FORT-P."
  )

(defun gt-cycles? (predicate fort &optional mt)                                                        
  "Returns whether FORT is accessible from itself by one or more PREDICATE gafs?                              
PREDICATE must satisfy FORT-P.                                                                             
FORT must satisfy FORT-P.                                                                                  
Single value returned satisfies BOOLEANP."
  )

(defun gt-has-inferior? (predicate superior inferior &optional mt)                                     
  "Returns whether fort SUPERIOR is hierarchically higher                                                     
- (wrt transitive PREDICATE) to fort INFERIOR?                                                               
PREDICATE must satisfy FORT-P.                                                                             
SUPERIOR must satisfy FORT-P.                                                                              
INFERIOR must satisfy FORT-P.                                                                              
Single value returned satisfies BOOLEANP."
  )

(defun gt-has-superior? (predicate inferior superior &optional mt)                                     
  "Returns whetherfort INFERIOR is hierarchically lower (wrt transitive PREDICATE)                            
to fort SUPERIOR?                                                                                          
PREDICATE must satisfy FORT-P.                                                                             
INFERIOR must satisfy FORT-P.                                                                              
SUPERIOR must satisfy FORT-P.                                                                              
Single value returned satisfies BOOLEANP."
  )

(defun gt-inferiors (predicate fort &optional mt)                                                      
  "Returns direct inferiors of FORT via transitive PREDICATE                                                  
PREDICATE must satisfy FORT-P.                                                                             
FORT must satisfy FORT-P.                                                                                  
Single value returned is a list of elements satisfying FORT-P."
  )

(defun gt-leaves (predicate fort &optional mt)                                                         
  "Returns minimal inferiors (i.e., leaves) of FORT via PREDICATE                                             
PREDICATE must satisfy FORT-P.                                                                             
FORT must satisfy FORT-P.                                                                                  
Single value returned is a list of elements satisfying FORT-P."
  )

(defun gt-max-floors (predicate forts &optional candidates mt)                                         
  "Returns the least-subordinate elements or common inferiors of FORTS                                        
- (when CANDIDATES is non-nil, the result must subset it)                                                    
PREDICATE must satisfy FORT-P.                                                                             
FORTS must satisfy LISTP.                                                                                  
Single value returned is a list of elements satisfying FORT-P."
  )

(defun gt-max-inferiors (predicate fort &optional mt)                                                  
  "Returns maximal inferiors of FORT via transitive PREDICATE                                                 
PREDICATE must satisfy FORT-P.                                                                             
FORT must satisfy FORT-P.                                                                                  
Single value returned is a list of elements satisfying FORT-P."
  )

(defun gt-max-inferiors-excluding (predicate inferior superior &optional mt)                           
  "Returns most-general inferiors of SUPERIOR ignoring INFERIOR (expensive)                                   
- (useful for splicing-out INFERIOR from hierarchy)                                                          
PREDICATE must satisfy FORT-P.                                                                             
INFERIOR must satisfy FORT-P.                                                                              
SUPERIOR must satisfy FORT-P.                                                                              
Single value returned is a list of elements satisfying FORT-P."
  )

(defun gt-max-nodes (predicate forts &optional mt direction)                                           
  "Returns returns the least-subordinate elements of FORTS                                                    
- (<direction> should be :up unless all nodes are low in the hierarchy)                                      
PREDICATE must satisfy FORT-P.                                                                             
FORTS must satisfy LISTP.                                                                                  
Single value returned is a list of elements satisfying FORT-P."
  )

(defun gt-min-ceilings (predicate forts &optional candidates mt)                                       
  "Returns the most-subordinate common superiors of FORTS                                                     
- (when CANDIDATES is non-nil, the result must subset it)                                                    
PREDICATE must satisfy FORT-P.                                                                             
FORTS must satisfy LISTP.                                                                                  
Single value returned is a list of elements satisfying FORT-P."
  )

(defun gt-min-nodes (predicate forts &optional mt)                                                     
  "Returns returns the most-subordinate elements of FORTS                                                     
- (one member only of a cycle will be a min-node candidate)                                                  
PREDICATE must satisfy FORT-P.                                                                             
FORTS must satisfy LISTP.                                                                                  
Single value returned is a list of elements satisfying FORT-P."
  )

(defun gt-min-superiors (predicate fort &optional mt)                                                  
  "Returns minimal superiors of FORT via transitive PREDICATE                                                 
PREDICATE must satisfy FORT-P.                                                                             
FORT must satisfy FORT-P.                                                                                  
Single value returned is a list of elements satisfying FORT-P."
  )

(defun gt-min-superiors-excluding (predicate inferior superior &optional mt)                           
  "Returns least-general superiors of INFERIOR ignoring SUPERIOR                                              
- (useful for splicing-out SUPERIOR from hierarchy)                                                          
PREDICATE must satisfy FORT-P.                                                                             
INFERIOR must satisfy FORT-P.                                                                              
SUPERIOR must satisfy FORT-P.                                                                              
Single value returned is a list of elements satisfying FORT-P."
  )

(defun gt-redundant-inferiors (predicate fort &optional mt)                                            
  "Returns direct-inferiors of FORT via PREDICATE that subsumed other inferiors                               
PREDICATE must satisfy FORT-P.                                                                             
FORT must satisfy FORT-P.                                                                                  
Single value returned is a list of elements satisfying FORT-P."
  )

(defun gt-redundant-superiors (predicate fort &optional mt)                                            
  "Returns direct-superiors of FORT via PREDICATE that are subsumed by other superiors                        
PREDICATE must satisfy FORT-P.                                                                             
FORT must satisfy FORT-P.                                                                                  
Single value returned is a list of elements satisfying FORT-P."
  )

(defun gt-roots (predicate fort &optional mt)                                                          
  "Returns maximal superiors (i.e., roots) of FORT via PREDICATE                                              
PREDICATE must satisfy FORT-P.                                                                             
FORT must satisfy FORT-P.                                                                                  
Single value returned is a list of elements satisfying FORT-P."
  )

(defun gt-superiors (predicate fort &optional mt)                                                      
  "Returns direct superiors of FORT via transitive PREDICATE                                                  
PREDICATE must satisfy FORT-P.                                                                             
FORT must satisfy FORT-P.                                                                                  
Single value returned is a list of elements satisfying FORT-P."
  )

(defun gt-why-completes-cycle? (predicate fort1 fort2 &optional mt)                                    
  "Returns justification that a transitive path connects FORT2 to FORT1,                                      
or that a transitive inverse path connects FORT1 to FORT2?                                                 
PREDICATE must satisfy FORT-P.                                                                             
FORT1 must satisfy FORT-P.                                                                                 
FORT2 must satisfy FORT-P."
  )

(defun gt-why-superior? (predicate superior inferior &optional mt)                                     
  "Returns justification of why SUPERIOR is superior to (i.e., hierarchically higher than)                    
INFERIOR                                                                                                   
PREDICATE must satisfy FORT-P.                                                                             
SUPERIOR must satisfy FORT-P.                                                                              
INFERIOR must satisfy FORT-P.                                                                              
Single value returned is a list of elements satisfying ASSERTION-P."
  )

(defun all-genl-attributes (att &optional mt tv)                                                       
  "Returns all genl-attributes of attribute ATT                                                               
- (ascending transitive closure; inexpensive)                                                                
ATT must satisfy EL-FORT-P.                                                                                
Single value returned is a list of elements satisfying FORT-P."
  )

(defun all-genl-attributes-if (function att &opti1onal mt tv)                                           
  "Returns all genl-attributes of attribute ATT that satisfy FUNCTION                                         
- (FUNCTION must not effect sbhl search state)                                                               
- (defun must satisfy FUNCTION-SPEC-P.                                                                     
ATT must satisfy EL-FORT-P.                                                                                
Single value returned is a list of elements satisfying FORT-P."
  )

(defun all-genl-attributes-wrt (spec genl &optional mt tv)                                             
  "Returns all genl-attributes of attribute SPEC that are also specs of attribute GENL (ascending             
transitive closure; inexpensive)                                                                           
SPEC must satisfy EL-FORT-P.                                                                               
GENL must satisfy EL-FORT-P.                                                                               
Single value returned is a list of elements satisfying FORT-P."
  )

(defun argn-genl-attribute (relation n &optional mt)                                                   
  "Returns the local genl-attribute constraints applied to the Nth argument of RELATION.                      
N must satisfy INTEGERP.                                                                                   
Single value returned is a list of elements satisfying INDEXED-TERM-P."
  )

(defun argn-genl-attribute-of (collection n &optional mt)                                              
  "Returns a list of the predicates for which COLLECTION is a                                                 
local genl-attribute constraint applied to the Nth argument.                                               
N must satisfy INTEGERP.                                                                                   
Single value returned is a list of elements satisfying FORT-P."
  )

(defun min-argn-genl-attribute (relation n &optional mt)                                               
  "Return a list of the most specific local genl-attribute constraints applicable                             
to the argument N of RELATION.                                                                             
N must satisfy INTEGERP.                                                                                   
Single value returned is a list of elements satisfying FORT-P."
  )

(defun all-spec-attributes (att &optional mt tv)                                                       
  "Returns all spec-attributes of attribute ATT                                                               
- (descending transitive closure; expensive)                                                                 
ATT must satisfy EL-FORT-P.                                                                                
Single value returned is a list of elements satisfying FORT-P."
  )

(defun all-spec-attributes-if (function att &optional mt tv)                                           
  "Returns all genl-attributes of attribute ATT that satisfy FUNCTION                                         
- (FUNCTION must not effect sbhl search state)                                                               
- (DEFUN must satisfy FUNCTION-SPEC-P.                                                                     
ATT must satisfy EL-FORT-P.                                                                                
Single value returned is a list of elements satisfying FORT-P."
  )

(defun fi-find (name)                                                                                  
  "Return the constant indentified by the string NAME.                                                        
Single value returned satisfies CONSTANT-P or is NIL."
  )

(defun fi-complete (prefix &optional case-sensitive?)                                                  
  "Return a list of constants whose name begins with PREFIX. The comparison is                                
performed in a case-insensitive mode unless CASE-SENSITIVE? is non-nil.                                    
Single value returned is a list of elements satisfying CONSTANT-P."
  )

(defun fi-create (name &optional external-id)                                                          
  "Create a new constant with NAME.                                                                           
If EXTERNAL-ID is non-null it is used, otherwise a unique identifier is generated.                         
Single value returned satisfies CONSTANT-P."
  )

(defun fi-find-or-create (name &optional external-id)                                                  
  "Return constant with NAME if it is present.                                                                
If not present, then create constant with NAME, using EXTERNAL-ID if given.                                
If EXTERNAL-ID is not given, generate a new one for the new constant.                                      
Single value returned satisfies CONSTANT-P."
  )

(defun fi-kill (fort)                                                                                  
  "Kill FORT and all its uses from the KB. If FORT is a microtheory, all assertions                           
in that microtheory are removed.                                                                           
Single value returned satisfies BOOLEANP."
  )

(defun fi-rename (constant name)                                                                       
  "Change name of CONSTANT to NAME. Return the constant if no error, otherwise return NIL.                    
Single value returned satisfies CONSTANT-P or is NIL."
  )

(defun fi-lookup (formula mt)                                                                          
  "Returns two values when looking up the EL FORMULA in the microtheory MT. The                               
first value returned is a list of HL formulas resulting from the canonicalization                          
of the EL FORMULA. The second value is T iff all the HL assertions were properly                           
put into the KB.                                                                                           
Value 1 returned is a list of elements satisfying CONSP.                                                   
Value 2 returned satisfies BOOLEANP"
  )

(defun fi-assert (formula mt &optional strength direction)                                             
  "Assert the FORMULA in the specified MT. STRENGTH is :default or :monotonic.                                
DIRECTION is :forward or :backward. GAF assertion direction defaults to :forward, and rule                 
assertion direction defaults to :backward. Return T if there was no error.                                 
Single value returned satisfies BOOLEANP."
  )

(defun fi-unassert (formula mt)                                                                        
  "Remove the assertions canonicalized from FORMULA in the microtheory MT.                                    
Return T if the operation succeeded, otherwise return NIL.                                                 
Single value returned satisfies BOOLEANP."
  )

(defun fi-edit (old-formula new-formula &optional old-mt new-mt strength direction)                    
  "Unassert the assertions canonicalized from OLD-FORMULA in the microtheory OLD-MT.                          
Assert NEW-FORMULA in the specified NEW-MT.                                                                
STRENGTH is :default or :monotonic.                                                                        
DIRECTION is :forward or :backward.                                                                        
GAF assertion direction defaults to :forward.                                                              
Rule assertion direction defaults to :backward.                                                            
Return T if there was no error.                                                                            
Single value returned satisfies BOOLEANP."
  )

(defun fi-blast (formula mt)                                                                           
  "Remove all arguments for the FORMULA within MT, including both those                                       
arguments resulting the direct assertion of the FORMULA, and                                               
those arguments supporting the FORMULA which were derived through inference.                               
Return T if successful, otherwise return NIL.                                                              
Single value returned satisfies BOOLEANP."
  )

(defun fi-ask (formula &optional mt backchain number time depth)                                       
  "Ask for bindings for free variables which will satisfy FORMULA within MT.                                  
If BACKCHAIN is NIL, no inference is performed.                                                            
If BACKCHAIN is an integer, then at most that many backchaining steps using rules                          
are performed.                                                                                             
If BACKCHAIN is T, then inference is performed without limit on the number of                              
backchaining steps when searching for bindings.                                                            
If NUMBER is an integer, then at most that number of bindings are returned.                                
If TIME is an integer, then at most TIME seconds are consumed by the search for                            
bindings.                                                                                                  
If DEPTH is an integer, then the inference paths are limited to that number of                             
total steps.                                                                                               
Returns NIL if the operation had an error. Otherwise returns a list of variable/                           
binding pairs. In the case where the FORMULA has no free variables, the form                               
- (((T . T))) is returned indicating that the gaf is either directly asserted in the                         
KB, or that it can be derived via rules in the KB.                                                         
Single value returned satisfies LISTP or is NIL."
  )

(defun fi-continue-last-ask (&optional backchain number time depth reconsider-deep)                    
  "Continue the last ask that was performed with more resources.                                              
If BACKCHAIN is NIL, no inference is performed.                                                            
If BACKCHAIN is an integer, then at most that many backchaining steps using rules                          
are performed.                                                                                             
If BACKCHAIN is T, then inference is performed without limit on the number of                              
backchaining steps when searching for bindings.                                                            
If NUMBER is an integer, then at most that number of bindings are returned.                                
If TIME is an integer, then at most TIME seconds are consumed by the search for                            
bindings.                                                                                                  
If DEPTH is an integer, then the inference paths are limited to that number of                             
total steps.                                                                                               
Returns NIL if the operation had an error. Otherwise returns a list of variable/                           
binding pairs. In the case where the FORMULA has no free variables, the form                               
- (((T . T))) is returned indicating that the gaf is either directly asserted in the                         
KB, or that it can be derived via rules in the KB.                                                         
Single value returned satisfies LISTP or is NIL."
  )

(defun fi-ask-status ()                                                                                
  "Return a status as to how the last ask successfully completed regarding                                    
resource limits.                                                                                           
:EXHAUST if the search spaces was exhausted.                                                               
:DEPTH if the search space was limited because some nodes were too deep.                                   
:NUMBER if the requested number of bindings was found without exceeding other limits.                      
:TIME if the time alloted expired prior to exhausting the search space.                                    
Return NIL if there was no prior successful ask.                                                           
Single value returned satisfies ATOM or is NIL."
  )

(defun fi-tms-reconsider-formula (formula mt)                                                          
  "Reconsider all arguments for FORMULA within MT. Return T if the                                            
operation succeeded, NIL if there was an error.                                                            
Single value returned satisfies BOOLEANP."
  )

(defun fi-tms-reconsider-mt (mt)                                                                       
  "Reconsider all arguments for all formulas within MT. Return T if the                                       
operation succeeded, NIL if there was an error.                                                            
Single value returned satisfies BOOLEANP."
  )

(defun fi-tms-reconsider-gafs (term &optional arg predicate mt)                                        
  "Reconsider all arguments for all gaf formulas involving TERM.                                              
ARG optionally constrains gafs such that the TERM occupies a specific arg position.                        
PREDICATE optionally constrains gafs such that the specifed PREDICATE                                      
occupies the arg0 position.                                                                                
MT optionally constrains gafs such that they must be included in the specific                              
microtheory.                                                                                               
Return T if the operation succeeded, NIL if there was an error.                                            
Single value returned satisfies BOOLEANP."
  )

(defun fi-tms-reconsider-term (term &optional mt)                                                      
  "Reconsider all arguments involving TERM.                                                                   
If MT is provided, then only arguments in that microtheory are reconsidered.                               
Return T if the operation succeeded, NIL if there was an error.                                            
Single value returned satisfies BOOLEANP."
  )

(defun fi-hypothesize (formula mt &optional name-prefix term-ids)                                      
  "Cyc attempts to check if FORMULA is satisfiable in MT by 'hypothesizing'                                   
constants for the variables in FORMULA, substituting them into FORMULA,                                    
and asserting the new formula in MT. If this would trigger a                                               
contradiction, then NIL is returned. Otherwise a binding list of variable /                                
constant pairs is returned, indicating the constants which were                                            
successfully 'hypothesized'. The form (((T . T))) is returned if no new terms                              
required creation for the assertion of FORMULA.                                                            
NAME-PREFIX is a string which is used as a prefix for the name of each new                                 
constant hypothesized. TERM-IDS is a list of variable / id pairs indicating                                
that the specified id should be used when generating the constant for the variable                         
in FORMULA. If TERM-IDS is NIL, then unused ids are allocated for the new                                  
constants.                                                                                                 
Single value returned satisfies LISTP or is NIL."
  )

(defun fi-prove (formula mt &optional backchain number time depth)                                     
  "Attempts to prove FORMULA is true in MT under the given resource constraints.                              
BACKCHAIN, NUMBER, TIME and DEPTH function as described for FI-ASK.                                        
FORMULA is interpreted as follows:                                                                         
If FORMULA is of the form (#$implies [antecedant] [consequent]) then                                       
- (1) free variables in [antecedant] are assumed to be universally                                           
quantified                                                                                                 
- (2) remaining variables in [consequent] are assumed to be existentially                                    
quantified.                                                                                                
Otherwise FORMULA is interpreted as (#$implies #$True FORMULA) and handled as the                          
case above.                                                                                                
It returns NIL or a list of arguments as described for FI-JUSTIFY.                                         
Single value returned satisfies (LIST LISTP) or is NIL."
  )

(defun fi-get-error ()                                                                                 
  "Return a description of the error resulting from the last FI operation.                                    
Single value returned satisfies ATOM or is NIL."
  )

(defun fi-get-warning ()                                                                               
  "Return a description of the warning resulting from the last FI operation.                                  
Single value returned satisfies ATOM or is NIL."
  )

(defun cyc-find-or-create (name external-id)                                                           
  "Return constant with NAME if it is present.                                                                
If not present, then create constant with NAME, using EXTERNAL-ID if given.                                
If EXTERNAL-ID is not given, generate a new one for the new constant.                                      
NAME must satisfy VALID-CONSTANT-NAME.                                                                     
EXTERNAL-ID must satisfy (NIL-OR CONSTANT-EXTERNAL-ID-P).                                                  
Single value returned satisfies CONSTANT-P."
  )

(defun cyc-create (name external-id)                                                                   
  "Create a new constant with id EXTERNAL-ID.                                                                 
If NAME is anything other than :unnamed,                                                                   
the new constant will be given the name NAME.                                                              
NAME must satisfy NEW-CONSTANT-NAME-SPEC-P.                                                                
EXTERNAL-ID must satisfy (NIL-OR CONSTANT-EXTERNAL-ID-P).                                                  
Single value returned satisfies CONSTANT-P."
  )

(defun cyc-create-new-ephemeral (name)                                                                 
  "Creates a new constant with name NAME, but makes                                                           
no effort to synchronize its external ID with                                                              
other Cyc images. This is intended for constants                                                           
that will not be transmitted to other Cyc images.                                                          
NAME must satisfy NEW-CONSTANT-NAME-SPEC-P.                                                                
Single value returned satisfies CONSTANT-P."
  )

(defun cyc-create-new-permanent (name)                                                                 
  "Creates a new constant with name NAME, gives it a                                                          
permanent unique external ID, and adds the constant                                                        
creation operation to the transcript queue.                                                                
NAME must satisfy NEW-CONSTANT-NAME-SPEC-P.                                                                
Single value returned satisfies CONSTANT-P."
  )

(defun cyc-kill (fort)                                                                                 
  "Kill FORT and all its uses from the KB. If FORT is a microtheory, all assertions                           
in that microtheory are removed.                                                                           
FORT must satisfy FORT-P.                                                                                  
Single value returned satisfies BOOLEANP."
  )

(defun cyc-recreate (constant)                                                                         
  "Doesn't unassert the bookkeeping info,                                                                     
but it might actually move it, or change                                                                   
its format somehow.                                                                                        
CONSTANT must satisfy CONSTANT-P.                                                                          
Single value returned satisfies CONSTANT-P."
  )

(defun cyc-rename (constant name)                                                                      
  "Change name of CONSTANT to NAME. Return the constant if no error, otherwise return NIL.                    
CONSTANT must satisfy CONSTANT-P.                                                                          
NAME must satisfy VALID-CONSTANT-NAME.                                                                     
Single value returned satisfies CONSTANT-P or is NIL."
  )

(defun cyc-merge (kill-fort keep-fort)                                                                 
  "Move asserted assertions on KILL-TERM onto KEEP-TERM before killing KILL-TERM.                             
@return fort-p; KEEP-FORT                                                                                  
KILL-FORT must satisfy FORT-P.                                                                             
KEEP-FORT must satisfy FORT-P.                                                                             
Single value returned satisfies FORT-P."
  )

(defun cyc-assert (sentence &optional mt properties)                                                   
  "Assert SENTENCE in the specified MT.                                                                       
properties; :strength el-strength-p (:default or :monotonic)                                               
:direction direction-p (:forward or :backward)                                                             
GAF assertion direction defaults to :forward, and rule                                                     
assertion direction defaults to :backward.                                                                 
@return booleanp; t iff the assert succeeded. If the assertion                                             
already existed, it is considered a success.                                                               
SENTENCE must satisfy POSSIBLY-SENTENCE-P.                                                                 
MT must satisfy (NIL-OR HLMT-P).                                                                           
PROPERTIES must satisfy ASSERT-PROPERTIES-P.                                                               
Single value returned satisfies BOOLEANP."
  )

(defun cyc-unassert (sentence &optional mt)                                                            
  "Remove the assertions canonicalized from FORMULA in the microtheory MT.                                    
Return T if the operation succeeded, otherwise return NIL.                                                 
SENTENCE must satisfy POSSIBLY-SENTENCE-P.                                                                 
MT must satisfy (NIL-OR HLMT-P).                                                                           
Single value returned satisfies BOOLEANP."
  )

(defun cyc-edit (old-sentence new-sentence &optional old-mt new-mt properties)                         
  "Unassert OLD-SENTENCE in OLD-MT, and assert NEW-SENTENCE in the specified NEW-MT.                          
@see cyc-unassert and @xref cyc-assert                                                                     
OLD-SENTENCE must satisfy POSSIBLY-SENTENCE-P.                                                             
NEW-SENTENCE must satisfy POSSIBLY-SENTENCE-P.                                                             
OLD-MT must satisfy (NIL-OR HLMT-P).                                                                       
NEW-MT must satisfy (NIL-OR HLMT-P).                                                                       
Single value returned satisfies BOOLEANP."
  )

(defun cyc-query (sentence &optional mt properties)                                                    
  "Query for bindings for free variables which will satisfy SENTENCE within MT.                               
;;;{{{DOC                                                                                                  
Properties: :backchain NIL or an integer or T                                                              
:number NIL or an integer                                                                                  
:time NIL or an integer                                                                                    
:depth NIL or an integer                                                                                   
:conditional-sentence boolean                                                                              
If :backchain is NIL, no backchaining is performed.                                                        
If :backchain is an integer, then at most that many backchaining steps using rules                         
are performed.                                                                                             
If :backchain is T, then inference is performed without limit on the number of                             
backchaining steps when searching for bindings.                                                            
If :number is an integer, then at most that number of bindings are returned.                               
If :time is an integer, then at most that many seconds are consumed by the search for                      
bindings.                                                                                                  
If :depth is an integer, then the inference paths are limited to that number of                            
total steps.                                                                                               
Returns NIL if the operation had an error. Otherwise returns a (possibly empty)                            
binding set. In the case where the SENTENCE has no free variables,                                         
the form (NIL), the empty binding set is returned, indicating that the gaf is either                       
directly asserted in the KB, or that it can be derived via rules in the KB.                                
If it fails to be proven, NIL will be returned.                                                            
The second return value indicates the reason why the query halted.                                         
If SENTENCE is an implication, or an ist wrapped around an implication,                                    
and the :conditional-sentence property is non-nil, cyc-query will attempt to                               
prove SENTENCE by reductio ad absurdum.                                                                    
;;;}}}EDOC                                                                                                 
SENTENCE must satisfy POSSIBLY-SENTENCE-P.                                                                 
MT must satisfy (NIL-OR HLMT-P).                                                                           
PROPERTIES must satisfy QUERY-PROPERTIES-P.                                                                
Single value returned satisfies QUERY-RESULTS-P."
  )

(defun cyc-continue-query (&optional query-id properties)                                              
  "Continues a query started by @xref cyc-query.                                                              
If QUERY-ID is :last, the most recent query is continued.                                                  
QUERY-ID must satisfy QUERY-ID-P.                                                                          
PROPERTIES must satisfy QUERY-PROPERTIES-P.                                                                
Single value returned satisfies QUERY-RESULTS-P."
  )

(defun cyc-add-argument (sentence cycl-supports &optional mt properties verify-supports)               
  "Tell Cyc to conclude SENTENCE (optionally in MT) based on the list of CYCL-SUPPORTS which should           
themselves be assertions or                                                                                
otherwise valid for support-p. If VERIFY-SUPPORTS is non-nil, then this function will attempt to           
verify the list of supports                                                                                
before making the assertion.                                                                               
Properties: :direction :forward or :backward                                                               
SENTENCE must satisfy POSSIBLY-SENTENCE-P.                                                                 
CYCL-SUPPORTS must satisfy LIST-OF-CYCL-SUPPORT-P.                                                         
MT must satisfy (NIL-OR HLMT-P).                                                                           
PROPERTIES must satisfy ASSERT-PROPERTIES-P.                                                               
VERIFY-SUPPORTS must satisfy BOOLEANP.                                                                     
Single value returned satisfies BOOLEANP."
  )

(defun cyc-remove-all-arguments (sentence &optional mt)                                                
  "Remove all arguments for SENTENCE within MT, including both those                                          
arguments resulting the direct assertion of SENTENCE, and                                                  
those arguments supporting SENTENCE which were derived through inference.                                  
Return T if successful, otherwise return NIL.                                                              
SENTENCE must satisfy POSSIBLY-SENTENCE-P.                                                                 
MT must satisfy (NIL-OR HLMT-P).                                                                           
Single value returned satisfies BOOLEANP."
  )

(defun cyc-remove-argument (sentence cycl-supports &optional mt)                                       
  "Remove the argument for SENTENCE specified by CYCL-SUPPORTS.                                               
SENTENCE must satisfy POSSIBLY-SENTENCE-P.                                                                 
CYCL-SUPPORTS must satisfy LIST-OF-CYCL-SUPPORT-P.                                                         
MT must satisfy (NIL-OR HLMT-P).                                                                           
Single value returned satisfies BOOLEANP."
  )

(defun cyc-rewrite (source-fort target-fort)                                                           
  "'moves' all asserted arguments from SOURCE-FORT to TARGET-FORT                                             
@return fort-p; TARGET-FORT                                                                                
SOURCE-FORT must satisfy FORT-P.                                                                           
TARGET-FORT must satisfy FORT-P.                                                                           
Single value returned satisfies FORT-P."
  )

(defun cyc-tms-reconsider-sentence (sentence &optional mt)                                             
  "Reconsider all arguments for SENTENCE within MT. Return T if the                                           
operation succeeded, NIL if there was an error.                                                            
SENTENCE must satisfy POSSIBLY-SENTENCE-P.                                                                 
MT must satisfy (NIL-OR HLMT-P).                                                                           
Single value returned satisfies BOOLEANP."
  )


;; ;; Subl primitive functions

;; (defun * ()
;;   "Subl primitive function.")

;; (defun + ()
;;   "Subl primitive function.")

;; (defun - ()
;;   "Subl primitive function.")

;; (defun / ()
;;   "Subl primitive function.")

;; (defun /= ()
;;   "Subl primitive function.")

;; (defun < ()
;;   "Subl primitive function.")

;; (defun <= ()
;;   "Subl primitive function.")

;; (defun = ()
;;   "Subl primitive function.")

;; (defun > ()
;;   "Subl primitive function.")

;; (defun >= ()
;;  "Subl primitive function.")

;; (defun acons ()
;;   "Subl primitive function.")

;; (defun adjoin ()
;;   "Subl primitive function.")

;; (defun alert-user ()
;;   "Subl primitive function.")

;; (defun alpha-char-p ()
;;   "Subl primitive function.")

;; (defun alphanumericp ()
;;   "Subl primitive function.")

;; (defun append ()
;;   "Subl primitive function.")

;; (defun append-files ()
;;   "Subl primitive function.")

;; (defun apply ()
;;   "Subl primitive function.")

;; (defun aref ()
;;   "Subl primitive function.")

;; (defun assoc ()
;;   "Subl primitive function.")

;; (defun assoc-if ()
;;   "Subl primitive function.")

;; (defun atom ()
;;   "Subl primitive function.")

;; (defun both-case-p ()
;;   "Subl primitive function.")

;; (defun boundp ()
;;   "Subl primitive function.")

;; (defun break ()
;;   "Subl primitive function.")

;; (defun butlast ()
;;   "Subl primitive function.")

;; (defun caar ()
;;   "Subl primitive function.")

;; (defun cadr ()
;;   "Subl primitive function.")

;; (defun car ()
;;   "Subl primitive function.")

;; (defun cconcatenate ()
;;   "Subl primitive function.")

;; (defun cdar ()
;;   "Subl primitive function.")

;; (defun cddr ()
;;   "Subl primitive function.")

;; (defun cdr ()
;;   "Subl primitive function.")

;; (defun ceiling ()
;;   "Subl primitive function.")

;; (defun cerror ()
;;   "Subl primitive function.")

;; (defun char ()
;;   "Subl primitive function.")

;; (defun char-code ()
;;   "Subl primitive function.")

;; (defun char-downcase ()
;;   "Subl primitive function.")

;; (defun char-equal ()
;;   "Subl primitive function.")

;; (defun char-greaterp ()
;;   "Subl primitive function.")

;; (defun char-lessp ()
;;   "Subl primitive function.")

;; (defun char-not-equal ()
;;   "Subl primitive function.")

;; (defun char-not-greaterp ()
;;   "Subl primitive function.")

;; (defun char-not-lessp ()
;;   "Subl primitive function.")

;; (defun char-upcase ()
;;   "Subl primitive function.")

;; (defun char/= ()
;;   "Subl primitive function.")

;; (defun char< ()
;;   "Subl primitive function.")

;; (defun char<= ()
;;   "Subl primitive function.")

;; (defun char= ()
;;   "Subl primitive function.")

;; (defun char> ()
;;   "Subl primitive function.")

;; (defun char>= ()
;;   "Subl primitive function.")

;; (defun characterp ()
;;   "Subl primitive function.")

;; (defun close ()
;;   "Subl primitive function.")

;; (defun clrhash ()
;;   "Subl primitive function.")

;; (defun cmerge ()
;;   "Subl primitive function.")

;; (defun code-char ()
;;   "Subl primitive function.")

;; (defun cons ()
;;   "Subl primitive function.")

;; (defun consp ()
;;   "Subl primitive function.")

;; (defun constantp ()
;;   "Subl primitive function.")

;; (defun construct-filename ()
;;   "Subl primitive function.")

;; (defun copy-alist ()
;;   "Subl primitive function.")

;; (defun copy-list ()
;;   "Subl primitive function.")

;; (defun copy-seq ()
;;   "Subl primitive function.")

;; (defun copy-tree ()
;;   "Subl primitive function.")

;; (defun count ()
;;   "Subl primitive function.")

;; (defun count-if ()
;;   "Subl primitive function.")

;; (defun creduce ()
;;   "Subl primitive function.")

;; (defun debug ()
;;   "Subl primitive function.")

;; (defun declare ()
;;   "Subl primitive function.")

;; (defun decode-universal-time ()
;;   "Subl primitive function.")

;; (defun default-signal-handler ()
;;   "Subl primitive function.")

;; (defun default-struct-print-function ()
;;   "Subl primitive function.")

;; (defun delete ()
;;   "Subl primitive function.")

;; (defun delete-duplicates ()
;;   "Subl primitive function.")

;; (defun delete-file ()
;;   "Subl primitive function.")

;; (defun delete-if ()
;;   "Subl primitive function.")

;; (defun digit-char-p ()
;;   "Subl primitive function.")

;; (defun directory ()
;;   "Subl primitive function.")

;; (defun dpb ()
;;   "Subl primitive function.")

;; (defun eighth ()
;;   "Subl primitive function.")

;; (defun elt ()
;;   "Subl primitive function.")

;; (defun encode-timestring ()
;;   "Subl primitive function.")

;; (defun encode-universal-time ()
;;   "Subl primitive function.")

;; (defun endp ()
;;   "Subl primitive function.")

;; (defun eq ()
;;   "Subl primitive function.")

;; (defun eql ()
;;   "Subl primitive function.")

;; (defun equal ()
;;   "Subl primitive function.")

;; (defun equalp ()
;;   "Subl primitive function.")

;; (defun error ()
;;   "Subl primitive function.")

;; (defun eval ()
;;   "Subl primitive function.")

;; (defun evenp ()
;;   "Subl primitive function.")

;; (defun fboundp ()
;;   "Subl primitive function.")

;; (defun fifth ()
;;   "Subl primitive function.")

;; (defun file-author ()
;;   "Subl primitive function.")

;; (defun file-length ()
;;   "Subl primitive function.")

;; (defun file-write-date ()
;;   "Subl primitive function.")

;; (defun fill ()
;;   "Subl primitive function.")

;; (defun find ()
;;   "Subl primitive function.")

;; (defun find-if ()
;;   "Subl primitive function.")

;; (defun find-symbol ()
;;   "Subl primitive function.")

;; (defun first ()
;;   "Subl primitive function.")

;; (defun fixnump ()
;;   "Subl primitive function.")

;; (defun float ()
;;   "Subl primitive function.")

;; (defun floatp ()
;;   "Subl primitive function.")

;; (defun floor ()
;;   "Subl primitive function.")

;; (defun fmakunbound ()
;;   "Subl primitive function.")

;; (defun force-output ()
;;   "Subl primitive function.")

;; (defun fork-process ()
;;   "Subl primitive function.")

;; (defun format ()
;;   "Subl primitive function.")

;; (defun fourth ()
;;   "Subl primitive function.")

;; (defun funcall ()
;;   "Subl primitive function.")

;; (defun function-information ()
;;   "Subl primitive function.")

;; (defun function-spec-p ()
;;   "Subl primitive function.")

;; (defun functionp ()
;;   "Subl primitive function.")

;; (defun gc ()
;;   "Subl primitive function.")

;; (defun gensym ()
;;   "Subl primitive function.")

;; (defun gentemp ()
;;   "Subl primitive function.")

;; (defun get ()
;;   "Subl primitive function.")

;; (defun get-consing-state ()
;;   "Subl primitive function.")

;; (defun get-file-position ()
;;   "Subl primitive function.")

;; (defun get-internal-real-time ()
;;   "Subl primitive function.")

;; (defun get-machine-name ()
;;   "Subl primitive function.")

;; (defun get-network-name ()
;;   "Subl primitive function.")

;; (defun get-process-id ()
;;   "Subl primitive function.")

;; (defun get-string-from-user ()
;;   "Subl primitive function.")

;; (defun get-universal-time ()
;;   "Subl primitive function.")

;; (defun get-user-name ()
;;   "Subl primitive function.")

;; (defun gethash ()
;;   "Subl primitive function.")

;; (defun hash-table-count ()
;;   "Subl primitive function.")

;; (defun hash-table-p ()
;;   "Subl primitive function.")

;; (defun identity ()
;;   "Subl primitive function.")

;; (defun ignore ()
;;   "Subl primitive function.")

;; (defun input-stream-p ()
;;   "Subl primitive function.")

;; (defun install-signal-handler ()
;;   "Subl primitive function.")

;; (defun int/ ()
;;   "Subl primitive function.")

;; (defun integer-decode-float ()
;;   "Subl primitive function.")

;; (defun integerp ()
;;   "Subl primitive function.")

;; (defun intern ()
;;   "Subl primitive function.")

;; (defun intersection ()
;;   "Subl primitive function.")

;; (defun keywordp ()
;;   "Subl primitive function.")

;; (defun last ()
;;   "Subl primitive function.")

;; (defun ldb ()
;;   "Subl primitive function.")

;; (defun ldiff ()
;;   "Subl primitive function.")

;; (defun length ()
;;   "Subl primitive function.")

;; (defun list ()
;;   "Subl primitive function.")

;; (defun list* ()
;;   "Subl primitive function.")

;; (defun list-length ()
;;   "Subl primitive function.")

;; (defun listp ()
;;   "Subl primitive function.")

;; (defun load ()
;;   "Subl primitive function.")

;; (defun log-message ()
;;   "Subl primitive function.")

;; (defun lower-case-p ()
;;   "Subl primitive function.")

;; (defun make-hash-table ()
;;   "Subl primitive function.")

;; (defun make-list ()
;;   "Subl primitive function.")

;; (defun make-string ()
;;   "Subl primitive function.")

;; (defun make-symbol ()
;;   "Subl primitive function.")

;; (defun make-vector ()
;;   "Subl primitive function.")

;; (defun makunbound ()
;;   "Subl primitive function.")

;; (defun mapc ()
;;   "Subl primitive function.")

;; (defun mapcan ()
;;   "Subl primitive function.")

;; (defun mapcar ()
;;   "Subl primitive function.")

;; (defun mapcon ()
;;   "Subl primitive function.")

;; (defun maphash ()
;;   "Subl primitive function.")

;; (defun mapl ()
;;   "Subl primitive function.")

;; (defun maplist ()
;;   "Subl primitive function.")

;; (defun max ()
;;   "Subl primitive function.")

;; (defun member ()
;;   "Subl primitive function.")

;; (defun member-if ()
;;   "Subl primitive function.")

;; (defun min ()
;;   "Subl primitive function.")

;; (defun minusp ()
;;   "Subl primitive function.")

;; (defun mismatch ()
;;   "Subl primitive function.")

;; (defun mod ()
;;   "Subl primitive function.")

;; (defun nbutlast ()
;;   "Subl primitive function.")

;; (defun nconc ()
;;   "Subl primitive function.")

;; (defun nintersection ()
;;   "Subl primitive function.")

;; (defun ninth ()
;;   "Subl primitive function.")

;; (defun note-activity ()
;;   "Subl primitive function.")

;; (defun note-numeric-progress ()
;;   "Subl primitive function.")

;; (defun note-percent-progress ()
;;   "Subl primitive function.")

;; (defun notify-user ()
;;   "Subl primitive function.")

;; (defun nreconc ()
;;   "Subl primitive function.")

;; (defun nreverse ()
;;   "Subl primitive function.")

;; (defun nset-difference ()
;;   "Subl primitive function.")

;; (defun nset-exclusive-or ()
;;   "Subl primitive function.")

;; (defun nstring-capitalize ()
;;   "Subl primitive function.")

;; (defun nstring-downcase ()
;;   "Subl primitive function.")

;; (defun nstring-upcase ()
;;   "Subl primitive function.")

;; (defun nsublis ()
;;   "Subl primitive function.")

;; (defun nsubst ()
;;   "Subl primitive function.")

;; (defun nsubst-if ()
;;   "Subl primitive function.")

;; (defun nsubstitute ()
;;   "Subl primitive function.")

;; (defun nsubstitute-if ()
;;   "Subl primitive function.")

;; (defun nth ()
;;   "Subl primitive function.")

;; (defun nthcdr ()
;;   "Subl primitive function.")

;; (defun null ()
;;   "Subl primitive function.")

;; (defun numberp ()
;;   "Subl primitive function.")

;; (defun nunion ()
;;   "Subl primitive function.")

;; (defun oddp ()
;;   "Subl primitive function.")

;; (defun open-binary ()
;;   "Subl primitive function.")

;; (defun open-tcp-stream ()
;;   "Subl primitive function.")

;; (defun open-text ()
;;   "Subl primitive function.")

;; (defun output-stream-p ()
;;   "Subl primitive function.")

;; (defun pairlis ()
;;   "Subl primitive function.")

;; (defun plusp ()
;;   "Subl primitive function.")

;; (defun position ()
;;   "Subl primitive function.")

;; (defun position-if ()
;;   "Subl primitive function.")

;; (defun prin1 ()
;;   "Subl primitive function.")

;; (defun prin1-to-string ()
;;   "Subl primitive function.")

;; (defun princ ()
;;   "Subl primitive function.")

;; (defun princ-to-string ()
;;   "Subl primitive function.")

;; (defun print ()
;;   "Subl primitive function.")

;; (defun probe-file ()
;;   "Subl primitive function.")

;; (defun proclaim ()
;;   "Subl primitive function.")

;; (defun put ()
;;   "Subl primitive function.")

;; (defun quote ()
;;   "Subl primitive function.")

;; (defun random ()
;;   "Subl primitive function.")

;; (defun rassoc ()
;;   "Subl primitive function.")

;; (defun rassoc-if ()
;;   "Subl primitive function.")

;; (defun read ()
;;   "Subl primitive function.")

;; (defun read-byte ()
;;   "Subl primitive function.")

;; (defun read-char ()
;;   "Subl primitive function.")

;; (defun read-from-string ()
;;   "Subl primitive function.")

;; (defun read-line ()
;;   "Subl primitive function.")

;; (defun rem ()
;;   "Subl primitive function.")

;; (defun remhash ()
;;   "Subl primitive function.")

;; (defun remove ()
;;   "Subl primitive function.")

;; (defun remove-duplicates ()
;;   "Subl primitive function.")

;; (defun remove-if ()
;;   "Subl primitive function.")

;; (defun remprop ()
;;   "Subl primitive function.")

;; (defun rename-file ()
;;   "Subl primitive function.")

;; (defun replace ()
;;   "Subl primitive function.")

;; (defun report-error ()
;;   "Subl primitive function.")

;; (defun rest ()
;;   "Subl primitive function.")

;; (defun restart-process ()
;;   "Subl primitive function.")

;; (defun revappend ()
;;   "Subl primitive function.")

;; (defun reverse ()
;;   "Subl primitive function.")

;; (defun round ()
;;   "Subl primitive function.")

;; (defun rplaca ()
;;   "Subl primitive function.")

;; (defun rplacd ()
;;   "Subl primitive function.")

;; (defun scale-float ()
;;   "Subl primitive function.")

;; (defun search ()
;;   "Subl primitive function.")

;; (defun second ()
;;   "Subl primitive function.")

;; (defun seed-random ()
;;   "Subl primitive function.")

;; (defun sequencep ()
;;   "Subl primitive function.")

;; (defun set ()
;;   "Subl primitive function.")

;; (defun set-aref ()
;;   "Subl primitive function.")

;; (defun set-char ()
;;   "Subl primitive function.")

;; (defun set-consing-state ()
;;   "Subl primitive function.")

;; (defun set-difference ()
;;   "Subl primitive function.")

;; (defun set-exclusive-or ()
;;   "Subl primitive function.")

;; (defun set-file-position ()
;;   "Subl primitive function.")

;; (defun set-nth ()
;;   "Subl primitive function.")

;; (defun sethash ()
;;   "Subl primitive function.")

;; (defun seventh ()
;;   "Subl primitive function.")

;; (defun sixth ()
;;   "Subl primitive function.")

;; (defun sleep ()
;;   "Subl primitive function.")

;; (defun sort ()
;;   "Subl primitive function.")

;; (defun stable-sort ()
;;   "Subl primitive function.")

;; (defun streamp ()
;;   "Subl primitive function.")

;; (defun string ()
;;   "Subl primitive function.")

;; (defun string-capitalize ()
;;   "Subl primitive function.")

;; (defun string-downcase ()
;;   "Subl primitive function.")

;; (defun string-equal ()
;;   "Subl primitive function.")

;; (defun string-greaterp ()
;;   "Subl primitive function.")

;; (defun string-left-trim ()
;;   "Subl primitive function.")

;; (defun string-lessp ()
;;   "Subl primitive function.")

;; (defun string-not-equal ()
;;   "Subl primitive function.")

;; (defun string-not-greaterp ()
;;   "Subl primitive function.")

;; (defun string-not-lessp ()
;;   "Subl primitive function.")

;; (defun string-right-trim ()
;;   "Subl primitive function.")

;; (defun string-trim ()
;;   "Subl primitive function.")

;; (defun string-upcase ()
;;   "Subl primitive function.")

;; (defun string/= ()
;;   "Subl primitive function.")

;; (defun string< ()
;;   "Subl primitive function.")

;; (defun string<= ()
;;   "Subl primitive function.")

;; (defun string= ()
;;   "Subl primitive function.")

;; (defun string> ()
;;   "Subl primitive function.")

;; (defun string>= ()
;;   "Subl primitive function.")

;; (defun stringp ()
;;   "Subl primitive function.")

;; (defun sublis ()
;;   "Subl primitive function.")

;; (defun subseq ()
;;   "Subl primitive function.")

;; (defun subsetp ()
;;   "Subl primitive function.")

;; (defun subst ()
;;   "Subl primitive function.")

;; (defun subst-if ()
;;   "Subl primitive function.")

;; (defun substitute ()
;;   "Subl primitive function.")

;; (defun substitute-if ()
;;   "Subl primitive function.")

;; (defun sxhash ()
;;   "Subl primitive function.")

;; (defun symbol-function ()
;;   "Subl primitive function.")

;; (defun symbol-name ()
;;   "Subl primitive function.")

;; (defun symbol-plist ()
;;   "Subl primitive function.")

;; (defun symbol-value ()
;;   "Subl primitive function.")

;; (defun symbolp ()
;;   "Subl primitive function.")

;; (defun tailp ()
;;   "Subl primitive function.")

;; (defun tenth ()
;;   "Subl primitive function.")

;; (defun terpri ()
;;   "Subl primitive function.")

;; (defun third ()
;;   "Subl primitive function.")

;; (defun throw ()
;;   "Subl primitive function.")

;; (defun time-from-now ()
;;   "Subl primitive function.")

;; (defun time-has-arrived? ()
;;   "Subl primitive function.")

;; (defun timestring ()
;;   "Subl primitive function.")

;; (defun tree-equal ()
;;   "Subl primitive function.")

;; (defun truncate ()
;;   "Subl primitive function.")

;; (defun union ()
;;   "Subl primitive function.")

;; (defun unread-char ()
;;   "Subl primitive function.")

;; (defun upper-case-p ()
;;   "Subl primitive function.")

;; (defun user-confirm ()
;;   "Subl primitive function.")

;; (defun values ()
;;   "Subl primitive function.")

;; (defun variable-information ()
;;   "Subl primitive function.")

;; (defun vector ()
;;   "Subl primitive function.")

;; (defun vectorp ()
;;   "Subl primitive function.")

;; (defun warn ()
;;   "Subl primitive function.")

;; (defun write ()
;;   "Subl primitive function.")

;; (defun write-byte ()
;;   "Subl primitive function.")

;; (defun write-char ()
;;   "Subl primitive function.")

;; (defun write-image ()
;;   "Subl primitive function.")

;; (defun write-line ()
;;   "Subl primitive function.")

;; (defun write-string ()
;;   "Subl primitive function.")

;; (defun write-to-string ()
;;   "Subl primitive function.")

;; (defun zerop ()
;;   "Subl primitive function.")

