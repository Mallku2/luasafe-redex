#lang racket

(require redex
         "./grammar.rkt"
         "./typing_lang_theory.rkt"
         "./desugar/parser.rkt")

;                                                                                                     
;                                                                                                     
;                                                                                                     
;                                                                     ;                               
;                                         ;                           ;                ;              
;                                         ;                                            ;              
;     ;;;      ;;;    ; ;;;;    ;;;;;   ;;;;;;     ; ;;;    ;;;;    ;;;     ; ;;;;   ;;;;;;    ;;;;;  
;    ;   ;    ;   ;   ;;   ;;  ;     ;    ;        ;;   ;  ;    ;     ;     ;;   ;;    ;      ;     ; 
;   ;        ;     ;  ;     ;  ;          ;        ;            ;     ;     ;     ;    ;      ;       
;   ;        ;     ;  ;     ;  ;;;;       ;        ;       ;;;;;;     ;     ;     ;    ;      ;;;;    
;   ;        ;     ;  ;     ;      ;;;    ;        ;      ;;    ;     ;     ;     ;    ;          ;;; 
;   ;        ;     ;  ;     ;        ;    ;        ;      ;     ;     ;     ;     ;    ;            ; 
;    ;   ;    ;   ;   ;     ;  ;     ;    ;        ;      ;    ;;     ;     ;     ;    ;      ;     ; 
;     ;;;      ;;;    ;     ;   ;;;;;      ;;;     ;       ;;;; ;  ;;;;;;;  ;     ;     ;;;    ;;;;;  
;                                                                                                     
;                                                                                                     
;                                                                                                     
;                                                                                                     

; Membership, to be able to use Cs as a set
(define-metafunction core-lang-typed
  cons_in : Cs c -> any

  [(cons_in () c)
   #f]

  [(cons_in (c_1 c_2 ...) c_1)
   #t]

  [(cons_in (c_1 c_2 ...) c_3)
   (cons_in (c_2 ...) c_3)]
  )

(define-metafunction core-lang-typed
  cons_un : Cs Cs -> Cs

  [(cons_un (c_1 ...) (c_2 ...))
   (c ...)

   (where (c ...) ,(remove-duplicates (term (c_1 ... c_2 ...))))]
  )

; Replaces occurrences of $actfunc by a given typevar:
; each return statements add restrictions that refer to $actfunc; we replace
; $actfunc by the typevar that represents this function
(define-metafunction core-lang-typed
  replace_actfunc : Cs τ -> Cs

  [(replace_actfunc () τ)
   ()]
  
  [(replace_actfunc ((($actfunc returntypevar) <: ρ) c_1 ...) τ)
   (((τ returntypevar) <: ρ) c_2 ...)

   (where (c_2 ...) (replace_actfunc (c_1 ...) τ))
   ]

  [(replace_actfunc ((τ_1 <: ($actfunc returntypevar)) c_1 ...) τ_2)
   ((τ_1 <: (τ_2 returntypevar)) c_2 ...)

   (where (c_2 ...) (replace_actfunc (c_1 ...) τ_2))
   ]

  [(replace_actfunc (c_1 c_2 ...) τ)
   (c_1 c_3 ...)

   (where (c_3 ...) (replace_actfunc (c_2 ...) τ))
   ]
  )

; Creates the corresponding typevars required to express the domain type of a
; function; updates γ correspondigly 
(define-metafunction core-lang-typed
  fun-domain-typevar : γ (Name ...) -> (γ ((Name Number typevar) ...))

  [(fun-domain-typevar γ ())
   (γ ())]
  
  [(fun-domain-typevar γ_1 (Name_1 Name_2 ...))
   (γ_3 ((Name_1 Number typevar) τ ...))
   
   (where Number ,(+ 1 (term (index-γ γ_1 Name_1))))
   (where γ_2 (set γ_1 Name_1 Number))
   (where (γ_3 (τ ...)) (fun-domain-typevar γ_2 (Name_2 ...)))]
  )

(define-metafunction core-lang-typed
  extract_ret_type : Cs τ -> (Cs τ)

  [(extract_ret_type (c_1 ...
                      (τ_1 <: ((function Name_1 ((Name_2 label typevar)) τ_2
                                         end)
                               returntypevar))
                      c_2 ...)
                     (function Name_1 ((Name_2 label typevar)) τ_2
                               end))
   ((c_1 ... c_2 ...) τ_1)]

  [(extract_ret_type Cs _)
   (Cs nil)]
  )

;                                                                  
;                                                                  
;                                                                  
;                                                                  
;                                                                  
;                                                                  
;     ;;;    ;;;;   ; ;;;    ;;;;            ;;;;;   ;;;;   ; ;;;  
;    ;   ;  ;;  ;;  ;;   ;  ;    ;          ;;  ;;  ;;  ;;  ;;   ; 
;   ;       ;    ;  ;    ;  ;               ;    ;  ;    ;  ;    ; 
;   ;       ;    ;  ;    ;   ;;;;           ;    ;  ;;;;;;  ;    ; 
;   ;       ;    ;  ;    ;       ;          ;    ;  ;       ;    ; 
;    ;   ;  ;;  ;;  ;    ;  ;    ;          ;;  ;;  ;;   ;  ;    ; 
;     ;;;    ;;;;   ;    ;   ;;;;            ;;; ;   ;;;;   ;    ; 
;                                                ;                 
;                                            ;   ;                 
;                                             ;;;                  
;                                                                  

; constraints for a list of expression (for example, for actual parameters of
; a fun call)
(define-judgment-form
  core-lang-typed
  #:mode (cons_gen_el I I O O O)
  #:contract (cons_gen_el γ (e ...) (τ ...) γ Cs)
  
  [(cons_gen γ_1 e τ γ_2 Cs)
   --------------------------------------------------------
   (cons_gen_el γ_1 (e) (τ) γ_2 Cs)]

  [(cons_gen γ_1 e_1 τ_1 γ_2 Cs_1)
   (cons_gen_el γ_2 (e_2 e_3 ...) (τ_2 ...) γ_3 Cs_2)
   (where Cs_3 (cons_un Cs_1 Cs_2))
   -----------------------------------------------------------
   (cons_gen_el γ_1 (e_1 e_2 e_3 ...) (τ_1 τ_2 ...) γ_3 Cs_3)]
  )

; constraints gen. for table fields
(define-judgment-form
  core-lang-typed
  #:mode (cons_gen_table_field I I O O O)
  #:contract (cons_gen_table_field γ (field_1 field_2 ...) (τ ...) γ Cs)

  [(cons_gen γ_1 e τ γ_2 Cs)
   ; dummy type var for the non-existent key
   (where label ,(+ 1 (term (index-γ γ_1 $dummyt))))
   (where γ_3 (set γ_2 $dummyt label))

   
   (where Cs_2 (cons_un Cs
                        (; key must be constrained to num type
                         (($dummyt label typevar) <: num)
                         )))
   --------------------------------------------------------
   (cons_gen_table_field γ_1 (e) ((\[ ($dummyt label typevar) \] = τ))
                         γ_2 Cs_2)]

  [(cons_gen γ_1 e_1 τ_1 γ_2 Cs_1)
   (cons_gen γ_2 e_2 τ_2 γ_3 Cs_2)
   (where Cs_3 (cons_un Cs_1 Cs_2))
   ----------------------------------------
   (cons_gen_table_field γ_1 ((\[ e_1 \] = e_2)) ((\[ τ_1 \] = τ_2)) γ_3 Cs_3)]


  [(cons_gen_table_field γ_1 (field_1) (τ_1) γ_2 Cs_1)
   (cons_gen_table_field γ_2 (field_2 field_3 ...) (τ_2 τ_3 ...) γ_3 Cs_2)
   (where Cs_3 (cons_un Cs_1 Cs_2))
   ------------------------------------------------------------
   (cons_gen_table_field γ_1
                         (field_1 field_2 field_3 ...)
                         (τ_1 τ_2 τ_3 ...) γ_3 Cs_3)]
  )

(define-judgment-form
  core-lang-typed
  #:mode (cons_gen I I O O O)
  #:contract (cons_gen γ any τ γ Cs)

  
  ;                                  
  ;                                  
  ;                                  
  ;                                  
  ;                                  
  ;                                  
  ;    ;;;;   ;;  ;;  ;;;;;    ;;;;  
  ;   ;;  ;;   ;  ;   ;;  ;;  ;    ; 
  ;   ;    ;    ;;    ;    ;  ;      
  ;   ;;;;;;    ;;    ;    ;   ;;;;  
  ;   ;         ;;    ;    ;       ; 
  ;   ;;   ;   ;  ;   ;;  ;;  ;    ; 
  ;    ;;;;   ;;  ;;  ;;;;;    ;;;;  
  ;                   ;              
  ;                   ;              
  ;                   ;              
  ;                                  

  ; Base types
  [-------------------------------------
   (cons_gen γ nil nil γ ((nil <: (nil : nil))))]

  [-------------------------------------
   (cons_gen γ Number Number γ ((Number <: (Number : num))))]

  [------------------------------------------------------------------
   (cons_gen γ String String γ ((String <: (String : str))))]

  [--------------------------------------------------
   (cons_gen γ Boolean Boolean γ ((Boolean <: (Boolean : bool))))]

  ; id
  [(where label (index-γ γ Name))
   -------------------------------------
   (cons_gen γ Name (Name label typevar) γ ())]


  ; funDef
  [; we are not using numeric label for identifying formal parameters: we use
   ; the function label
   (where (γ_2 ((Name_4 Number_1 typevar) (Name_5 Number_2 typevar) ...))
          (fun-domain-typevar γ_1
                              (Name_2 Name_3 ...)))

   (cons_gen γ_2 s τ_1 γ_3 Cs_1)

   ; each return statements add restrictions that refer to $actfunc; we replace
   ; $actfunc by the typevar that represents this function
   (where Cs_2 (replace_actfunc
                Cs_1
                (function Name_1 ((Name_4 Number_1 typevar)
                                  (Name_5 Number_2 typevar) ...) τ_1 end)
                ))
   
   (where
    Cs_3
    (cons_un
     Cs_2
     (; Constraint over fdef
      ((function Name_1 ((Name_4 Number_1 typevar)
                         (Name_5 Number_2 typevar) ...) τ_1 end)
       <:
       (((function Name_1 ((Name_4 Number_1 typevar)
                           (Name_5 Number_2 typevar) ...) τ_1 end) paramtypevar)
        ->
        ((function Name_1 ((Name_4 Number_1 typevar)
                           (Name_5 Number_2 typevar) ...) τ_1 end)
         returntypevar))
       )
      ; To carry restrictions over the function's type to and from restrictions
      ; over the parameter's type
      (((function Name_1 ((Name_4 Number_1 typevar)
                          (Name_5 Number_2 typevar) ...) τ_1 end) paramtypevar)
       <: ($tup (Name_4 Number_1 typevar) (Name_5 Number_2 typevar) ...))
      )))
   -----------------------------------------------------------
   (cons_gen γ_1
             (function Name_1 (Name_2 Name_3 ...) s end)
             (function Name_1 ((Name_4 Number_1 typevar)
                               (Name_5 Number_2 typevar) ...) τ_1 end)
             γ_3
             Cs_3)]

  [(cons_gen γ_1 s τ_1 γ_2 Cs_1)

   ; each return statements add restrictions that refer to $actfunc; we replace
   ; $actfunc by the typevar that represents this function
   (where Cs_2 (replace_actfunc
                Cs_1
                (function Name_1 () τ_1 end)
                ))
   
   (where
    Cs_3
    (cons_un
     Cs_2
     (; Constraint over fdef
      ((function Name_1 () τ_1 end)
       <:
       (((function Name_1 () τ_1 end) paramtypevar)
        ->
        ((function Name_1 () τ_1 end)
         returntypevar))
       )
      ; To carry restrictions over the function's type to and from restrictions
      ; over the parameter's type
      (((function Name_1 () τ_1 end) paramtypevar)
       <: ($tup))
      )))
   -----------------------------------------------------------
   (cons_gen γ_1
             (function Name_1 () s end)
             (function Name_1 () τ_1 end)
             γ_2
             Cs_3)]

  ; vararg
  ;  [-------------------------------------
  ;   (cons_gen γ <<< _ _ _)]
  
  ; table field
  ; Assumption: indexation of tables fields only over the variable identifier
  ; bound to the table
  [(cons_gen γ_1 e_2 τ_2 γ_2 Cs_1)
   
   ; new label
   (where label ,(+ 1 (term (index-γ γ_2 Name))))
   (where γ_3 (set γ_2 Name label))
   (where Cs_2 (cons_un Cs_1
                        ; τ_1 should be a subtype of a table type that, at least,
                        ; contains a field with a key that is a subtype of τ_2,
                        ; and value with a subtype of (τ_1 \[ τ_2 \])
                        (((Name label typevar)
                          <: (\[ τ_2 \] : ((Name label typevar) \[ τ_2 \])))
                
                         ; we require for the occurrences of Name to have the
                         ; same members constrained for the previous occurrence
                         ; of Name
                         ((Name label typevar)
                          <: τ_2 (Name (index-γ γ_2 Name) typevar))

                         )))
   --------------------------------------------------------------
   (cons_gen γ_1 (Name \[ e_2 \]) ((Name label typevar) \[ τ_2 \]) γ_3 Cs_2)]

  ; funCall
  [(cons_gen γ_1 prefixexp τ_1 γ_2 Cs_1)
   (cons_gen_el γ_2 (e_1 e_2 ...) (τ_2 τ_3 ...) γ_3 Cs_2)
   (where Cs_3 (cons_un (cons_un Cs_1 Cs_2)
                        (; Constraint over params' type: it is ok if the call
                         ; passes to the function an actual parameter with a
                         ; type <: according to subtyping
                         (($tup τ_2 τ_3 ...) <: (τ_1 paramtypevar))
                         ((τ_1 returntypevar) <: (τ_1 (τ_2 τ_3 ...)))
                         ; prefixexp should have a function type
                         (τ_1 <: ((τ_1 paramtypevar) -> (τ_1 returntypevar)))
                         )))
   --------------------------------------------------------
   (cons_gen γ_1 (prefixexp (e_1 e_2 ...))
             (τ_1 (τ_2 τ_3 ...))
             γ_3 Cs_3)]

  [(cons_gen γ_1 prefixexp τ_1 γ_2 Cs_1)
   (where Cs_2 (cons_un Cs_1
                        (; Constraint over params' type: it is ok if the call
                         ; passes to the function an actual parameter with a
                         ; type <: according to subtyping
                         (($tup) <: (τ_1 paramtypevar))
                         ((τ_1 returntypevar) <: (τ_1 ()))
                         ; prefixexp should have a function type
                         (τ_1 <: ((τ_1 paramtypevar) -> (τ_1 returntypevar)))
                         )))
   --------------------------------------------------------
   (cons_gen γ_1 (prefixexp ())
             (τ_1 ())
             γ_2 Cs_2)]

  ; parenthesized exp
  [(cons_gen γ_1 e τ γ_2 Cs_1)
   ; Type of the parenthesized expression should be the same as the expression,
   ; if it is not a tuple value
   ; TODO: tuple value case!
   (where Cs_2 (cons_un Cs_1
                        (
                         ((\( τ \)) <: τ)
                         (τ <: (\( τ \)))
                         )))
   ----------------------------------------
   (cons_gen γ_1 (\( e \)) (\( τ \)) γ_2 Cs_2)]

  ; tableconstructor
  [(cons_gen_table_field γ_1 (field_1 field_2 ...) ((\[ τ_1 \] = τ_2) ...) γ_2
                         Cs_1)
   (where Cs_2 (cons_un Cs_1
                        (((\{ (\[ τ_1 \] = τ_2) ... \})
                          <: (\[ τ_1 \] : τ_2)) ...
                         )))
   ------------------------------------------------------
   (cons_gen γ_1 (\{ field_1 field_2 ... \}) (\{ (\[ τ_1 \] = τ_2) ... \})
             γ_2 Cs_2)]

  [------------------------------------------------------
   (cons_gen γ (\{ \}) (\{ \}) γ (((\{ \}) <: ((\{ \}) strong))))]

  ; binops
  [(cons_gen γ_1 e_1 τ_1 γ_2 Cs_1)
   (cons_gen γ_2 e_2 τ_2 γ_3 Cs_2)
   (where Cs_3 (cons_un (cons_un Cs_1 Cs_2)
                        ; operands must be a subtype of the expected operands'
                        ; type for an arithop: included for improved type
                        ; inference capabilities
                        ((τ_1 <: num)
                         (τ_2 <: num)
                         ((τ_1 arithop τ_2) <: num))))
   ----------------------------------
   (cons_gen γ_1
             (e_1 arithop e_2)
             (τ_1 arithop τ_2) γ_3 Cs_3)]

  [(cons_gen γ_1 e_1 τ_1 γ_2 Cs_1)
   (cons_gen γ_2 e_2 τ_2 γ_3 Cs_2)
   (where Cs_3 (cons_un (cons_un Cs_1 Cs_2)
                        ; operands must be of the same subtype
                        ((τ_1 <: τ_2)
                         (τ_2 <: τ_1)
                         ((τ_1 relop τ_2) <: bool))))
   ----------------------------------
   (cons_gen γ_1 (e_1 relop e_2)
             (τ_1 relop τ_2) γ_3 Cs_3)]

  [(cons_gen γ_1 e_1 τ_1 γ_2 Cs_1)
   (cons_gen γ_2 e_2 τ_2 γ_3 Cs_2)
   (where Cs_3 (cons_un (cons_un Cs_1 Cs_2)
                        ; operands must be a subtype of the expected operands'
                        ; type for string concat op
                        ((τ_1 <: str)
                         (τ_2 <: str)
                         ((τ_1 .. τ_2) <: str))))
   ----------------------------------
   (cons_gen γ_1 (e_1 .. e_2)
             (τ_1 .. τ_2) γ_3 Cs_3)]

  [(cons_gen γ_1 e_1 τ_1 γ_2 Cs_1)
   (cons_gen γ_2 e_2 τ_2 γ_3 Cs_2)
   (where Cs_3 (cons_un (cons_un Cs_1 Cs_2)
                        ; operands must be of the same subtype
                        ((τ_1 <: τ_2)
                         (τ_2 <: τ_1)
                         ((τ_1 == τ_2) <: bool))))
   ----------------------------------
   (cons_gen γ_1 (e_1 == e_2)
             (τ_1 == τ_2) γ_3 Cs_3)]

  ; and, or operator
  [(cons_gen γ_1 e_1 τ_1 γ_2 Cs_1)
   (cons_gen γ_2 e_2 τ_2 γ_3 Cs_2)
   (where Cs_3 (cons_un Cs_1 Cs_2))
   ; TODO: tuple type to impose some restriction over the
   ; result: ((τ_1 and τ_2) <: (unt τ_1 τ_2))
   ----------------------------------
   (cons_gen γ_1
             (e_1 and e_2)
             (τ_1 and τ_2) γ_3 Cs_3)]

  [(cons_gen γ_1 e_1 τ_1 γ_2 Cs_1)
   (cons_gen γ_2 e_2 τ_2 γ_3 Cs_2)
   (where Cs_3 (cons_un Cs_1 Cs_2))
   ; TODO: tuple type to impose some restriction over the
   ; result: ((τ_1 or τ_2) <: (unt τ_1 τ_2))
   ----------------------------------
   (cons_gen γ_1
             (e_1 or e_2)
             (τ_1 or τ_2) γ_3 Cs_3)]
  
  ; unops
  [(cons_gen γ_1 e τ γ_2 Cs_1)
   (where Cs_2 (cons_un Cs_1
                        ; operand must be a number
                        ((τ <: num)
                         ((- τ) <: num))))
   ----------------------------------
   (cons_gen γ_1 (- e) (- τ) γ_2 Cs_2)]

  [(cons_gen γ_1 e τ γ_2 Cs_1)
   (where Cs_2 (cons_un Cs_1
                        (((not τ) <: bool))))
   ------------------------------------
   (cons_gen γ_1 (not e) (not τ) γ_2 Cs_2)]

  [(cons_gen γ_1 e τ γ_2 Cs_1)
   (where Cs_2 (cons_un Cs_1
                        ; operand could be a str or table; we do not impose
                        ; restrictions
                        (((\# τ) <: num))))
   ----------------------------------
   (cons_gen γ_1 (\# e) (\# τ) γ_2 Cs_2)]
  
  
  ;                                          
  ;                                          
  ;                                          
  ;                                          
  ;             ;               ;            
  ;             ;               ;            
  ;    ;;;;   ;;;;;;    ;;;   ;;;;;;   ;;;;  
  ;   ;    ;    ;      ;   ;    ;     ;    ; 
  ;   ;         ;          ;    ;     ;      
  ;    ;;;;     ;      ;;;;;    ;      ;;;;  
  ;        ;    ;     ;    ;    ;          ; 
  ;   ;    ;    ;     ;   ;;    ;     ;    ; 
  ;    ;;;;      ;;;   ;;; ;     ;;;   ;;;;  
  ;                                          
  ;                                          
  ;                                          
  ;                                          

  [-----------------------
   (cons_gen γ \; \; γ ())]

  [-----------------------
   (cons_gen γ break break γ ())]

  ; TOOD: only one returned value
  [; referring to actual function through $actfunc
   (cons_gen γ_1 e τ γ_2 Cs_1)
   (where Cs_2 (cons_un Cs_1
                        ((τ <: ($actfunc returntypevar)))))
   -------------------------------------------------------------------
   (cons_gen γ_1 (return e) (return τ) γ_2 Cs_2)                   ]

  [; referring to actual function through $actfunc
   -------------------------------------------------------------------
   (cons_gen γ (return) (return) γ ())                   ]

  [(cons_gen γ_1 s τ γ_2 Cs)
   -----------------------------------------
   (cons_gen γ_1 (do s end) (do τ end) γ_1 Cs)]

  [(cons_gen γ_1 e τ_1 γ_2 Cs_1)
   (cons_gen γ_2 s_1 τ_2 γ_3 Cs_2)
   (cons_gen γ_3 s_2 τ_3 γ_4 Cs_3)
   (where Cs_4 (cons_un (cons_un Cs_1 Cs_2) Cs_3))
   --------------------------------------------------------
   (cons_gen γ_1 (if e then s_1 else s_2 end)
             (if τ_1 then τ_2 else τ_3 end) γ_4 Cs_4)]

  [(cons_gen γ_1 e τ_1 γ_2 Cs_1)
   (cons_gen γ_2 s τ_2 γ_3 Cs_2)
   (where Cs_3 (cons_un Cs_1 Cs_2))
   --------------------------------------------------------
   (cons_gen γ_1 (while e do s end)
             (while τ_1 do τ_2 end) γ_3 Cs_3)]


  ; local var def
  ; TODO: only one variable
  [(cons_gen γ_1 e τ_1 γ_2 Cs_1)

   (where label ,(+ 1 (term (index-γ γ_2 Name))))
   (where γ_3 (set γ_2 Name label))
   
   (cons_gen γ_3 s τ_2 γ_4 Cs_2)
   (where Cs_3 (cons_un (cons_un Cs_1 Cs_2)
                        (; Constraint over the type of the variables
                         (τ_1 <: (Name label typevar))
                         )))
   ----------------------------------------------------------------
   (cons_gen γ_1 (local Name = e in s end)
             (local (Name label typevar) = τ_1 in τ_2 end)
             γ_2 
             Cs_3)]

  
  ; TODO: only one variable
  [; The dynamic semantics indicates that e is evaluated before assignment;
   ; hence the type variable for Name shouldn't be the new one
   (cons_gen γ_1 e τ γ_2 Cs_1)

   (where Number ,(term (index-γ γ_2 Name)))

   (where Cs_3 (cons_un Cs_1
                        ((τ <: (Name Number typevar)))))
   -----------------------------------------------------------------
   (cons_gen γ_1 (Name = e) ((Name Number typevar) = τ) γ_2 Cs_3)]

  [(cons_gen γ_1 e_1 τ_2 γ_2 Cs_1)
   (cons_gen γ_2 e_2 τ_3 γ_3 Cs_2)
   
   ; new label
   (where label ,(+ 1 (term (index-γ γ_3 Name))))
   (where γ_4 (set γ_3 Name label))
   
   (where Cs_3
          (cons_un
           (cons_un Cs_1 Cs_2)
           (; we require (Name label typevar) to have a member τ_2 with type
            ; ((Name label typevar) \[ τ_2 \])
            ((Name label typevar)
             <: (\[ τ_2 \] : ((Name label typevar) \[ τ_2 \])))

            ; we require for the occurrences of Name to have the same members
            ; constrained for the previous occurrence of Name
            ((Name label typevar) <: τ_2 (Name (index-γ γ_3 Name) typevar))

            (τ_3 <: ((Name label typevar) \[ τ_2 \]))
            )))
   -----------------------------------------------------------
   (cons_gen γ_1
             ((Name \[ e_1 \]) = e_2)
             (((Name label typevar) \[ τ_2 \]) = τ_3)
             γ_4 Cs_3)                         ]

  [(side-condition ,(not (redex-match? core-lang-typed
                                       Name
                                       (term e_1))))
   (cons_gen γ_1 e_1 τ_1 γ_2 Cs_1)
   (cons_gen γ_2 e_2 τ_2 γ_3 Cs_2)
   (cons_gen γ_3 e_3 τ_3 γ_4 Cs_3)
   
   (where Cs_4 (cons_un (cons_un (cons_un Cs_1 Cs_2) Cs_3)
                        ((τ_3 <: (τ_1 \[ τ_2 \]))
                         (τ_1 <: (\[ τ_2 \] : (τ_1 \[ τ_2 \]))))))
   -----------------------------------------------------------
   (cons_gen γ_1
             ((e_1 \[ e_2 \]) = e_3)
             ((τ_1 \[ τ_2 \]) = τ_3)
             γ_4 Cs_4)                         ]

  [(cons_gen γ_1 prefixexp τ_1 γ_2 Cs_1)
   (cons_gen_el γ_2 (e_1 e_2 ...) (τ_2 τ_3 ...) γ_3 Cs_2)
   (where Cs_3 (cons_un (cons_un Cs_1 Cs_2)
                        (; Constraint over params' type: it is ok if the call
                         ; passes to the function an actual parameter with a
                         ; type <: according to subtyping
                         (($tup τ_2 τ_3 ...) <: (τ_1 paramtypevar))
                         ((τ_1 returntypevar) <: (τ_1 (τ_2 τ_3 ...)))
                         ; prefixexp should have a function type
                         (τ_1 <: ((τ_1 paramtypevar) -> (τ_1 returntypevar)))
                         )))
   --------------------------------------------------------
   (cons_gen γ_1 ($statFunCall prefixexp (e_1 e_2 ...))
             ($statFunCall τ_1 (τ_2 τ_3 ...))
             γ_3 Cs_3)]

  [(cons_gen γ_1 prefixexp τ_1 γ_2 Cs_1)
   (where Cs_2 (cons_un Cs_1
                        (; Constraint over params' type: it is ok if the call
                         ; passes to the function an actual parameter with a
                         ; type <: according to subtyping
                         (($tup) <: (τ_1 paramtypevar))
                         ((τ_1 returntypevar) <: (τ_1 ()))
                         ; prefixexp should have a function type
                         (τ_1 <: ((τ_1 paramtypevar) -> (τ_1 returntypevar)))
                         )))
   --------------------------------------------------------
   (cons_gen γ_1 ($statFunCall prefixexp ())
             ($statFunCall τ_1 ())
             γ_2 Cs_2)]
  
  [(cons_gen γ_1 s_1 τ_1 γ_2 Cs_1)
   (cons_gen γ_2 s_2 τ_2 γ_3 Cs_2)
   (where Cs_3 (cons_un Cs_1 Cs_2))
   --------------------------------------
   (cons_gen γ_1 (s_1 s_2) (τ_1 τ_2) γ_3 Cs_3)]

  [(cons_gen γ_1 s_1 τ_1 γ_2 Cs_1)
   (cons_gen γ_2 (s_2 s_3 s_4 ...) (τ_2 ...) γ_3 Cs_2)
   (where Cs_3 (cons_un Cs_1 Cs_2))
   --------------------------------------
   (cons_gen γ_1 (s_1 s_2 s_3 s_4 ...) (τ_1 τ_2 ...) γ_3 Cs_3)]

  )

(provide cons_gen)

;                                                                          
;                                                                          
;                                                   ;;;                    
;                                                     ;                    
;                                                     ;                    
;                                                     ;                    
;     ;;;    ;;;;   ; ;;;    ;;;;             ;;;     ;      ;;;;    ;;;;  
;    ;   ;  ;;  ;;  ;;   ;  ;    ;           ;   ;    ;     ;;  ;;  ;    ; 
;   ;       ;    ;  ;    ;  ;               ;         ;     ;    ;  ;      
;   ;       ;    ;  ;    ;   ;;;;           ;         ;     ;    ;   ;;;;  
;   ;       ;    ;  ;    ;       ;          ;         ;     ;    ;       ; 
;    ;   ;  ;;  ;;  ;    ;  ;    ;           ;   ;    ;     ;;  ;;  ;    ; 
;     ;;;    ;;;;   ;    ;   ;;;;             ;;;      ;;;   ;;;;    ;;;;  
;                                                                          
;                                                                          
;                                                                          
;
(define-metafunction core-lang-typed
  combine_clos_steps : Cs -> Cs

  ; Base case
  [(combine_clos_steps Cs)
   Cs

   ; Cs is already a closed set of constraints: no new constraint can be
   ; inferred from C
   (where () ,(judgment-holds (cons_clos_step Cs c) c))]

  [(combine_clos_steps (c_1 ...))
   (combine_clos_steps (c_4 ...))

   (where (c_2 c_3 ...) ,(judgment-holds (cons_clos_step (c_1 ...) c) c))
   ; remove duplicates
   (where (c_4 ...) ,(remove-duplicates (term (c_1 ... c_2 c_3 ...))))]
  )

(provide combine_clos_steps)


; FUNDAMENTAL PROPERTY: a solution for a given set C, must also be a solution
; for the closure set of C
(define-judgment-form
  core-lang-typed
  #:mode (cons_clos_step I O)
  #:contract (cons_clos_step Cs c) 

  ; constraint implied by transitivity of subtyping  
  [(where (c_1 ... (τ_1 <: τ_2) c_2 ...) (c ...))
   (where (c_3 ... (τ_2 <: ρ) c_4 ...) (c_1 ... c_2 ...))
   (side-condition ,(not (term (cons_in (c ...) (τ_1 <: ρ)))))
   ; TODO: better way to deal with this?
   (side-condition ,(not (equal? (term τ_1) (term ρ))))
   -----------------------------------------------------------
   (cons_clos_step (c ...) (τ_1 <: ρ))                     ]

  ; to transfer constraints about required fields, to new occurrences of
  ; a single variable constrained to have table type 
  [(where (c_1 ... (τ_1 <: τ_2 τ_3) c_2 ...) (c ...))
   (where (c_3 ... (τ_3 <: (\[ τ_4 \] : τ_5)) c_4 ...) (c_1 ... c_2 ...))
   (side-condition ,(not (term (cons_in (c ...) (τ_1 <: (\[ τ_4 \] : τ_5))))))
   ---------------------------------------------------------------------------
   (cons_clos_step (c ...) (τ_1 <: (\[ τ_4 \] : τ_5)))]

  ; closeFunc: subtyping for functions reduces to equality 
  [(where (c_1 ... (τ_1 <: (τ_2 -> τ_3)) c_2 ...) (c ...))
   (where (c_3 ... (τ_1 <: (τ_4 -> τ_5)) c_4 ...) (c_1 ... c_2 ...))
   (side-condition ,(not (term (cons_in (c ...) (τ_2 <: τ_4)))))
   ; TODO: better way to deal with this?
   (side-condition ,(not (equal? (term τ_2) (term τ_4))))
   -----------------------------------------------------------------------
   (cons_clos_step (c ...) (τ_2 <: τ_4))]
  
  [(where (c_1 ... (τ_1 <: (τ_2 -> τ_3)) c_2 ...) (c ...))
   (where (c_3 ... (τ_1 <: (τ_4 -> τ_5)) c_4 ...) (c_1 ... c_2 ...))
   (side-condition ,(not (term (cons_in (c ...) (τ_4 <: τ_2)))))
   ; TODO: better way to deal with this?
   (side-condition ,(not (equal? (term τ_4) (term τ_2))))
   -----------------------------------------------------------------------
   (cons_clos_step (c ...) (τ_4 <: τ_2))]
  
  [(where (c_1 ... (τ_1 <: (τ_2 -> τ_3)) c_2 ...) (c ...))
   (where (c_3 ... (τ_1 <: (τ_4 -> τ_5)) c_4 ...) (c_1 ... c_2 ...))
   (side-condition ,(not (term (cons_in (c ...) (τ_3 <: τ_5)))))
   ; TODO: better way to deal with this?
   (side-condition ,(not (equal? (term τ_3) (term τ_5))))
   -----------------------------------------------------------------------
   (cons_clos_step (c ...) (τ_3 <: τ_5))]
  
  [(where (c_1 ... (τ_1 <: (τ_2 -> τ_3)) c_2 ...) (c ...))
   (where (c_3 ... (τ_1 <: (τ_4 -> τ_5)) c_4 ...) (c_1 ... c_2 ...))
   (side-condition ,(not (term (cons_in (c ...) (τ_5 <: τ_3)))))
   ; TODO: better way to deal with this?
   (side-condition ,(not (equal? (term τ_5) (term τ_3))))
   -----------------------------------------------------------------------
   (cons_clos_step (c ...) (τ_5 <: τ_3))]

  ; Tables: for a given key, force same value type over every type var that
  ; refers to the associated value
  [(where (c_1 ... (τ_1 <: (\[ τ_2 \] : τ_3)) c_2 ...) (c ...))
   (where (c_3 ... (τ_1 <: (\[ τ_2 \] : τ_4)) c_4 ...) (c_1 ... c_2 ...))
   (side-condition ,(not (term (cons_in (c ...) (τ_3 <: τ_4)))))
   ; TODO: better way to deal with this?
   (side-condition ,(not (equal? (term τ_3) (term τ_4))))
   -----------------------------------------------------------------------
   (cons_clos_step (c ...) (τ_3 <: τ_4))]

  [(where (c_1 ... (τ_1 <: (\[ τ_2 \] : τ_3)) c_2 ...) (c ...))
   (where (c_3 ... (τ_1 <: (\[ τ_2 \] : τ_4)) c_4 ...) (c_1 ... c_2 ...))
   (side-condition ,(not (term (cons_in (c ...) (τ_4 <: τ_3)))))
   ; TODO: better way to deal with this?
   (side-condition ,(not (equal? (term τ_3) (term τ_4))))
   -----------------------------------------------------------------------
   (cons_clos_step (c ...) (τ_4 <: τ_3))]
 

  ; tables are indexed by singleton types: associated values must be equivalent
  [(where (c_1 ... (τ_1 <: ((\{ (\[ τ_2 \] : τ_3) ...
                                (\[ τ_4 \] : τ_5)
                                (\[ τ_6 \] : τ_7) ... \}) weakness)) c_2 ...)
          (c ...))
   (where (c_3 ... (τ_1 <: (\[ τ_4 \] : τ_10)) c_4 ...)
          (c_1 ... c_2 ...))
   (side-condition ,(not (term (cons_in (c ...) (τ_5 <: τ_10)))))
   ; TODO: better way to deal with this?
   (side-condition ,(not (equal? (term τ_5) (term τ_10))))
   -----------------------------------------------------------------------
   (cons_clos_step (c ...) (τ_5 <: τ_10))]

  [(where (c_1 ... (τ_1 <: ((\{ (\[ τ_2 \] : τ_3) ...
                                (\[ τ_4 \] : τ_5)
                                (\[ τ_6 \] : τ_7) ... \}) weakness)) c_2 ...)
          (c ...))
   (where (c_3 ... (τ_1 <: (\[ τ_4 \] : τ_10)) c_4 ...)
          (c_1 ... c_2 ...))
   (side-condition ,(not (term (cons_in (c ...) (τ_10 <: τ_5)))))
   ; TODO: better way to deal with this?
   (side-condition ,(not (equal? (term τ_5) (term τ_10))))
   -----------------------------------------------------------------------
   (cons_clos_step (c ...) (τ_10 <: τ_5))]

  ; closeBalance:
  ; subtype constraints with supertypes that are also minimals
  [(where (c_1 ... (τ_1 <: τ_2) c_2 ...) (c ...))
   (where (c_3 ... (τ_1 <: υ) c_4 ...) (c_1 ... c_2 ...))
   (side-condition ,(not (term (cons_in (c ...) (υ <: τ_2)))))
   ; to avoid useless constraints
   (side-condition ,(not (term (cons_in (c ...) (τ_2 <: υ)))))
   ; TODO: better way to deal with this?
   (side-condition ,(not (equal? (term υ) (term τ_2))))
   -----------------------------------------------------------------------
   (cons_clos_step (c ...) (υ <: τ_2))]

  ; subtype constraints with supertypes which belong to ϕ imply a disjunction
  ; of type constraints, given the structure of our subtyping relation
  [(where (c_1 ... (τ_1 <: τ_2) c_2 ...) (c ...))
   (where (c_3 ... (τ_1 <: ϕ) c_4 ...) (c_1 ... c_2 ...))
   (side-condition ,(not (redex-match? core-lang-typed
                                       bt
                                       (term ξ))))
   (side-condition ,(not (term (cons_in (c ...) (τ_2 <: ϕ ∨ ϕ <: τ_2)))))
   ; TODO: better way to deal with this?
   (side-condition ,(not (equal? (term τ_2) (term ϕ))))
   -----------------------------------------------------------------------
   (cons_clos_step (c ...) (τ_2 <: ϕ ∨ ϕ <: τ_2))]

  [(where (c_1 ... (τ_1 <: τ_2) c_2 ...) (c ...))
   (where (c_3 ... (τ_1 <: ϕ ∨ ϕ <: τ_1) c_4 ...) (c_1 ... c_2 ...))
   (side-condition ,(not (redex-match? core-lang-typed
                                       bt
                                       (term ξ))))
   (side-condition ,(not (term (cons_in (c ...) (τ_2 <: ϕ ∨ ϕ <: τ_2)))))
   ; TODO: better way to deal with this?
   (side-condition ,(not (equal? (term τ_2) (term ϕ))))
   -----------------------------------------------------------------------
   (cons_clos_step (c ...) (τ_2 <: ϕ ∨ ϕ <: τ_2))]
 
  ; subtyping rel. for tuples:
  ; using named ellipses to force same length lists
  [(where (c_1 ...
           (($tup τ_1 ..._1 τ_2 τ_3 ..._2) <: ($tup τ_4 ..._1 τ_5 τ_6 ..._2))
           c_2 ...) (c ...))
   (side-condition ,(not (term (cons_in (c ...) (τ_2 <: τ_5)))))
   ; TODO: better way to deal with this?
   (side-condition ,(not (equal? (term τ_2) (term τ_5))))
   -----------------------------------------------------------
   (cons_clos_step (c ...) (τ_2 <: τ_5))                     ]

  )
(provide cons_clos_step)

(define-metafunction core-lang-typed
  combine_clos_refine_steps : Cs -> Cs

  ; Base case
  [(combine_clos_refine_steps Cs)
   Cs

   ; Cs is already a closed set of constraints: no new constraint can be
   ; inferred from C
   (where () ,(judgment-holds (cons_refine_step Cs c) c))]

  [(combine_clos_refine_steps (c_1 ...))
   (combine_clos_refine_steps (c_4 ...))

   (where (c_2 c_3 ...) ,(judgment-holds (cons_refine_step (c_1 ...) c) c))
   ; remove constraints
   (where (c_4 ...) ,(remove* (term (c_2 c_3 ...))
                              (term (c_1 ...))))]
  )

(provide combine_clos_refine_steps)

(define-judgment-form
  core-lang-typed
  #:mode (cons_refine_step I O)
  #:contract (cons_refine_step Cs c)

  [(where (c_1 ... (τ <: υ) c_2 ...) (c ...))
   (where (c_3 ... (τ <: χ) c_4 ...) (c_1 ... c_2 ...))
   (side-condition ,(not (redex-match?
                          core-lang-typed
                          υ
                          (term χ))))
   -----------------------------------------------------------
   (cons_refine_step (c ...) (τ <: χ))                     ]
)
;                                                                                          
;                                                                                          
;                   ;;;     ;;;                ;;                                        ; 
;                     ;       ;               ;                                          ; 
;                     ;       ;               ;                                          ; 
;                     ;       ;               ;                                          ; 
;  ;      ;  ;;;;     ;       ;             ;;;;;    ;;;;    ;;;;   ;;;;;;;  ;;;;    ;;;;; 
;  ;      ; ;;  ;;    ;       ;               ;     ;;  ;;   ;;  ;  ;  ;  ; ;;  ;;  ;;  ;; 
;   ; ;; ;  ;    ;    ;       ;               ;     ;    ;   ;      ;  ;  ; ;    ;  ;    ; 
;   ; ;; ;  ;;;;;;    ;       ;      ;;;      ;     ;    ;   ;      ;  ;  ; ;;;;;;  ;    ; 
;   ; ;; ;  ;         ;       ;               ;     ;    ;   ;      ;  ;  ; ;       ;    ; 
;    ;  ;   ;;   ;    ;       ;               ;     ;;  ;;   ;      ;  ;  ; ;;   ;  ;;  ;; 
;    ;  ;    ;;;;      ;;;     ;;;            ;      ;;;;    ;      ;  ;  ;  ;;;;    ;;;;; 
;                                                                                          
;                                                                                          
;                                                                                          
;                                                                                          

(define-judgment-form
  core-lang-typed
  #:mode (well_form_cons_set I)
  #:contract (well_form_cons_set Cs)
  ; For constructions for which there are no constraints to be applied
  [------------------------
   (well_form_cons_set ())]

  [; TODO: cómo decidimos si Cs es closed? (side-condition (cons_clos_step Cs ?))
   ; TODO: cómo pedimos por well_form_cons para todas las constraints? 
   (well_form_cons (c_1 c_2 ...) c_1)
   (well_form_cons (c_1 c_2 ...) c_2) ...
   ------------------------------------------
   (well_form_cons_set (c_1 c_2 ...))]
  
  )

(provide well_form_cons_set)

(define-metafunction core-lang-typed
  nil_chain : (τ Cs) -> any

  [(nil_chain (side-condition
               (τ_1 (c_1 ... c c_2 ...))
               (or (redex-match core-lang-typed
                                (side-condition (τ_2 <: χ)
                                                (and
                                                 (equal? (term τ_1) (term τ_2))
                                                 (not (is_nil_chain? (term χ))))
                                                )
                                (term c))
                   (redex-match core-lang-typed
                                (side-condition (χ <: τ_2)
                                                (and
                                                 (equal? (term τ_1) (term τ_2))
                                                 (not (is_nil_chain? (term χ))))
                                                )
                                (term c))
                   (redex-match core-lang-typed
                                (side-condition (τ_2 <: χ ∨ χ <: τ_2)
                                                (and
                                                 (equal? (term τ_1) (term τ_2))
                                                 (not (is_nil_chain? (term χ))))
                                                )
                                (term c))
                   (redex-match core-lang-typed
                                (side-condition (τ_2 <: (\[ τ_3 \] : τ_4))
                                                (equal? (term τ_1) (term τ_2)))
                                (term c))
                   (redex-match core-lang-typed
                                (side-condition (τ_2 <: τ_3 τ_4)
                                                (equal? (term τ_1) (term τ_2)))
                                (term c)))))
   #f]

  [(nil_chain _)
   #t]
  )

(define-metafunction core-lang-typed
  num_chain : (τ Cs) -> any

  [(num_chain (side-condition
               (τ_1 (c_1 ... c c_2 ...))
               (or (redex-match core-lang-typed
                                (side-condition (τ_2 <: χ)
                                                (and
                                                 (equal? (term τ_1) (term τ_2))
                                                 (not (is_num_chain? (term χ))))
                                                )
                                (term c))
                   (redex-match core-lang-typed
                                (side-condition (χ <: τ_2)
                                                (and
                                                 (equal? (term τ_1) (term τ_2))
                                                 (not (is_num_chain? (term χ))))
                                                )
                                (term c))
                   (redex-match core-lang-typed
                                (side-condition (τ_2 <: χ ∨ χ <: τ_2)
                                                (and
                                                 (equal? (term τ_1) (term τ_2))
                                                 (not (is_num_chain? (term χ))))
                                                )
                                (term c))
                   (redex-match core-lang-typed
                                (side-condition (τ_2 <: (\[ τ_3 \] : τ_4))
                                                (equal? (term τ_1) (term τ_2)))
                                (term c))
                   (redex-match core-lang-typed
                                (side-condition (τ_2 <: τ_3 τ_4)
                                                (equal? (term τ_1) (term τ_2)))
                                (term c)))))
   #f]

  [(num_chain _)
   #t]
  )

(define-metafunction core-lang-typed
  str_chain : (τ Cs) -> any

  [(str_chain (side-condition
               (τ_1 (c_1 ... c c_2 ...))
               (or (redex-match core-lang-typed
                                (side-condition (τ_2 <: χ)
                                                (and
                                                 (equal? (term τ_1) (term τ_2))
                                                 (not (is_str_chain? (term χ))))
                                                )
                                (term c))
                   (redex-match core-lang-typed
                                (side-condition (χ <: τ_2)
                                                (and
                                                 (equal? (term τ_1) (term τ_2))
                                                 (not (is_str_chain? (term χ))))
                                                )
                                (term c))
                   (redex-match core-lang-typed
                                (side-condition (τ_2 <: χ ∨ χ <: τ_2)
                                                (and
                                                 (equal? (term τ_1) (term τ_2))
                                                 (not (is_str_chain? (term χ))))
                                                )
                                (term c))
                   (redex-match core-lang-typed
                                (side-condition (τ_2 <: (\[ τ_3 \] : τ_4))
                                                (equal? (term τ_1) (term τ_2)))
                                (term c))
                   (redex-match core-lang-typed
                                (side-condition (τ_2 <: τ_3 τ_4)
                                                (equal? (term τ_1) (term τ_2)))
                                (term c)))))
   #f]

  [(str_chain _)
   #t]
  )


(define-metafunction core-lang-typed
  bool_chain : (τ Cs) -> any

  [(bool_chain (side-condition
               (τ_1 (c_1 ... c c_2 ...))
               (or (redex-match core-lang-typed
                                (side-condition (τ_2 <: χ)
                                                (and
                                                 (equal? (term τ_1) (term τ_2))
                                                 (not (is_bool_chain? (term χ)))
                                                 ))
                                (term c))
                   (redex-match core-lang-typed
                                (side-condition (χ <: τ_2)
                                                (and
                                                 (equal? (term τ_1) (term τ_2))
                                                 (not (is_bool_chain? (term χ)))
                                                 ))
                                (term c))
                   (redex-match core-lang-typed
                                (side-condition (τ_2 <: χ ∨ χ <: τ_2)
                                                (and
                                                 (equal? (term τ_1) (term τ_2))
                                                 (not (is_bool_chain? (term χ)))
                                                 ))
                                (term c))
                   (redex-match core-lang-typed
                                (side-condition (τ_2 <: (\[ τ_3 \] : τ_4))
                                                (equal? (term τ_1) (term τ_2)))
                                (term c))
                   (redex-match core-lang-typed
                                (side-condition (τ_2 <: τ_3 τ_4)
                                                (equal? (term τ_1) (term τ_2)))
                                (term c)))))
   #f]

  [(bool_chain _)
   #t]
  )

(define-metafunction core-lang-typed
  func_chain : (τ Cs) -> any

  [(func_chain (side-condition
               (τ_1 (c_1 ... c c_2 ...))
               (or (redex-match core-lang-typed
                                (side-condition (τ_2 <: χ)
                                                (and
                                                 (equal? (term τ_1) (term τ_2))
                                                 (not (is_func_chain? (term χ)))
                                                 ))
                                (term c))
                   (redex-match core-lang-typed
                                (side-condition (χ <: τ_2)
                                                (and
                                                 (equal? (term τ_1) (term τ_2))
                                                 (not (is_func_chain? (term χ)))
                                                 ))
                                (term c))
                   (redex-match core-lang-typed
                                (side-condition (τ_2 <: χ ∨ χ <: τ_2)
                                                (and
                                                 (equal? (term τ_1) (term τ_2))
                                                 (not (is_func_chain? (term χ)))
                                                 ))
                                (term c))
                   (redex-match core-lang-typed
                                (side-condition (τ_2 <: (\[ τ_3 \] : τ_4))
                                                (equal? (term τ_1) (term τ_2)))
                                (term c))
                   (redex-match core-lang-typed
                                (side-condition (τ_2 <: τ_3 τ_4)
                                                (equal? (term τ_1) (term τ_2)))
                                (term c)))))
   #f]

  [(func_chain _)
   #t]
  )

(define-metafunction core-lang-typed
  table_chain : (τ Cs) -> any

  [(table_chain (side-condition
               (τ_1 (c_1 ... c c_2 ...))
               (or (redex-match core-lang-typed
                                (side-condition (τ_2 <: χ)
                                                (and
                                                 (equal? (term τ_1) (term τ_2))
                                                 (not (is_table_chain? (term χ))
                                                      )))
                                (term c))
                   (redex-match core-lang-typed
                                (side-condition (χ <: τ_2)
                                                (and
                                                 (equal? (term τ_1) (term τ_2))
                                                 (not (is_table_chain? (term χ))
                                                      )))
                                (term c))
                   (redex-match core-lang-typed
                                (side-condition (τ_2 <: χ ∨ χ <: τ_2)
                                                (and
                                                 (equal? (term τ_1) (term τ_2))
                                                 (not (is_table_chain? (term χ))
                                                      )))
                                (term c)))))
   #f]

  [(table_chain _)
   #t]
  )

(define-metafunction core-lang-typed
  noemptup_chain : (τ Cs) -> any

  [(noemptup_chain (side-condition
               (τ_1 (c_1 ... c c_2 ...))
               (or (redex-match core-lang-typed
                                (side-condition (τ_2 <: χ)
                                                (and
                                                 (equal? (term τ_1) (term τ_2))
                                                 (not (is_noemptup_chain?
                                                       (term χ)))
                                                 ))
                                (term c))
                   (redex-match core-lang-typed
                                (side-condition (χ <: τ_2)
                                                (and
                                                 (equal? (term τ_1) (term τ_2))
                                                 (not (is_noemptup_chain?
                                                       (term χ)))
                                                 ))
                                (term c))
                   (redex-match core-lang-typed
                                (side-condition (τ_2 <: χ ∨ χ <: τ_2)
                                                (and
                                                 (equal? (term τ_1) (term τ_2))
                                                 (not (is_noemptup_chain?
                                                       (term χ)))
                                                 ))
                                (term c))
                   (redex-match core-lang-typed
                                (side-condition (τ_2 <: (\[ τ_3 \] : τ_4))
                                                (equal? (term τ_1) (term τ_2)))
                                (term c))
                   (redex-match core-lang-typed
                                (side-condition (τ_2 <: τ_3 τ_4)
                                                (equal? (term τ_1) (term τ_2)))
                                (term c)))))
   #f]

  [(noemptup_chain _)
   #t]
  )

(define-metafunction core-lang-typed
  emptup_chain : (τ Cs) -> any

  [(emptup_chain (side-condition
               (τ_1 (c_1 ... c c_2 ...))
               (or (redex-match core-lang-typed
                                (side-condition (τ_2 <: χ)
                                                (and
                                                 (equal? (term τ_1) (term τ_2))
                                                 (not (is_emptup_chain?
                                                       (term χ)))
                                                 ))
                                (term c))
                   (redex-match core-lang-typed
                                (side-condition (χ <: τ_2)
                                                (and
                                                 (equal? (term τ_1) (term τ_2))
                                                 (not (is_emptup_chain?
                                                       (term χ)))
                                                 ))
                                (term c))
                   (redex-match core-lang-typed
                                (side-condition (τ_2 <: χ ∨ χ <: τ_2)
                                                (and
                                                 (equal? (term τ_1) (term τ_2))
                                                 (not (is_emptup_chain?
                                                       (term χ)))
                                                 ))
                                (term c))
                   (redex-match core-lang-typed
                                (side-condition (τ_2 <: (\[ τ_3 \] : τ_4))
                                                (equal? (term τ_1) (term τ_2)))
                                (term c))
                   (redex-match core-lang-typed
                                (side-condition (τ_2 <: τ_3 τ_4)
                                                (equal? (term τ_1) (term τ_2)))
                                (term c)))))
   #f]

  [(emptup_chain _)
   #t]
  )

(define-metafunction core-lang-typed
  choose_chain : τ χ Cs -> any

  ; dynamic type: nothing to check
  [(choose_chain τ dyn Cs)
   #t]

  [(choose_chain τ χ Cs)
   (nil_chain (τ Cs))

   (side-condition (is_nil_chain? (term χ)))]

  [(choose_chain τ χ Cs)
   (num_chain (τ Cs))

   (side-condition (is_num_chain? (term χ)))]

  [(choose_chain τ χ Cs)
   (bool_chain (τ Cs))

   (side-condition (is_bool_chain? (term χ)))]

  [(choose_chain τ χ Cs)
   (str_chain (τ Cs))

   (side-condition (is_str_chain? (term χ)))]

  [(choose_chain τ χ Cs)
   (func_chain (τ Cs))

   (side-condition (is_func_chain? (term χ)))]

  [(choose_chain τ χ Cs)
   (table_chain (τ Cs))

   (side-condition (is_table_chain? (term χ)))]

  [(choose_chain τ χ Cs)
   (noemptup_chain (τ Cs))

   (side-condition (is_noemptup_chain? (term χ)))]

  [(choose_chain τ χ Cs)
   (emptup_chain (τ Cs))

   (side-condition (is_emptup_chain? (term χ)))]
  )

(define-judgment-form
  core-lang-typed
  #:mode (well_form_cons I I)
  #:contract (well_form_cons Cs c) 

  [(side-condition (choose_chain τ χ Cs))
   --------------------------------------
   (well_form_cons Cs (χ <: τ))]

  [(side-condition (choose_chain τ χ Cs))
   --------------------------------------
   (well_form_cons Cs (τ <: χ))]

  [(side-condition (choose_chain τ χ Cs))
   --------------------------------------
   (well_form_cons Cs (τ <: χ ∨ χ <: τ))]

  [(side-condition (table_chain (τ_1 Cs)))
   (side-condition (table_chain (τ_3 Cs)))
   --------------------------------------
   (well_form_cons Cs (τ_1 <: τ_2 τ_3))]

  [(side-condition (table_chain (τ_1 Cs)))
   --------------------------------------
   (well_form_cons Cs (τ_1 <: (\[ τ_2 \] : τ_3)))]

  ; type variables which are not type terms
  [--------------------------------
   (well_form_cons Cs (τ_1 <: τ_2))]
  
  )

;                                                          
;                                                          
;                   ;;;                                    
;                     ;                                    
;                     ;                                    
;                     ;                                    
;    ;;;;    ;;;;     ;              ;;;;;   ;;;;   ; ;;;  
;   ;    ;  ;;  ;;    ;             ;;  ;;  ;;  ;;  ;;   ; 
;   ;       ;    ;    ;             ;    ;  ;    ;  ;    ; 
;    ;;;;   ;    ;    ;             ;    ;  ;;;;;;  ;    ; 
;        ;  ;    ;    ;             ;    ;  ;       ;    ; 
;   ;    ;  ;;  ;;    ;             ;;  ;;  ;;   ;  ;    ; 
;    ;;;;    ;;;;      ;;;           ;;; ;   ;;;;   ;    ; 
;                                        ;                 
;                                    ;   ;                 
;                                     ;;;                  
;                                                          
; looks for constraints of the form (τ <: (\[ τ_1 \] : τ_2)), for a given τ, in
; order to reconstruct the table type
(define-metafunction core-lang-typed
  combine_table_cons : Cs τ -> ((\[ τ \] : τ) ...)

  [(combine_table_cons Cs τ)
   (combine_table_cons_aux Cs τ ())]
  )

(define-metafunction core-lang-typed
  combine_table_cons_aux : Cs τ ((\[ τ \] : τ) ...) -> ((\[ τ \] : τ) ...)

  [(combine_table_cons_aux ()
                           τ
                           ((\[ τ_3 \] : τ_4) ...))
   ((\[ τ_3 \] : τ_4) ...)]

  [(combine_table_cons_aux ((τ <: (\[ τ_1 \] : τ_2))
                            c ...)
                           τ
                           ((\[ τ_3 \] : τ_4) ...))

   (combine_table_cons_aux (c ...)
                           τ
                           ((\[ τ_5 \] : τ_6) ...))

   (where ((\[ τ_5 \] : τ_6) ...) ,(remove-duplicates
                                    (term ((\[ τ_1 \] : τ_2)
                                           (\[ τ_3 \] : τ_4) ...))))]

  [(combine_table_cons_aux (c_1 c_2 ...)
                           τ
                           ((\[ τ_3 \] : τ_4) ...))

   (combine_table_cons_aux (c_2 ...)
                           τ
                           ((\[ τ_3 \] : τ_4) ...))]
  )


; solves constraints for each field of a table 
(define-judgment-form
  core-lang-typed
  #:mode (sol_gen_table_fields I I I O O)
  #:contract (sol_gen_table_fields Cs V ((\[ τ \] : τ) ...)
                                   ((\[ t \] : t) ...) V)

  ; {Cs is a closed, well-formed set of constraints}

  [(sol_gen Cs V_1 τ_1 t_1 V_2)
   (sol_gen Cs V_2 τ_2 t_2 V_3)
   ---------------------------------------------------------------------
   (sol_gen_table_fields Cs V_1 ((\[ τ_1 \] : τ_2)) ((\[ t_1 \] : t_2)) V_3)]

  [(sol_gen Cs V_1 τ_1 t_1 V_2)
   (sol_gen Cs V_2 τ_2 t_2 V_3)
   (sol_gen_table_fields Cs V_3 ((\[ τ_3 \] : τ_4)
                                 (\[ τ_5 \] : τ_6) ...)
                         ((\[ t_3 \] : t_4) ...)
                         V_4)
   ----------------------------------------------------------------------
   (sol_gen_table_fields Cs V_1 ((\[ τ_1 \] : τ_2)
                                 (\[ τ_3 \] : τ_4)
                                 (\[ τ_5 \] : τ_6) ...)
                         ((\[ t_1 \] : t_2) (\[ t_3 \] : t_4) ...) V_4)]
  )
  
(define-judgment-form
  core-lang-typed
  #:mode (sol_gen I I I O O)
  #:contract (sol_gen Cs V ρ t V)

  ; {Cs is a closed, well-formed set of constraints}

  ; base cases
  ; type variable already solved
  [(side-condition (in-map V τ))
   (where α (index V τ))
   -------------------------------------------------------
   (sol_gen Cs V τ α V)                                    ]

  ; Base types
  [(where (c_1 ... (τ <: pt) c_2 ...) Cs)
   ---------------------------------------------------------
   (sol_gen Cs V τ pt V)                                    ]

  [(where (c_1 ... (pt <: τ) c_2 ...) Cs)
   ---------------------------------------------------------
   (sol_gen Cs V τ pt V)                                    ]
  
   [(where (c_1 ... (τ <: ϕ ∨ ϕ <: τ) c_2 ...) Cs)
   --------------------------------------------------------------------
   (sol_gen Cs V τ ϕ V)]

  ; tuple
  [(where (c_1 ... (τ_1 <: ($tup τ_2 ...)) c_2 ...) Cs)
   (sol_gen Cs V τ_2 t V_2) ...
   ---------------------------------------------------------
   (sol_gen Cs V τ_1 ($tup t ...) V)                        ]

  [(where (c_1 ... (τ_1 <: ($tup)) c_2 ...) Cs)
   ---------------------------------------------------------
   (sol_gen Cs V τ_1 ($tup) V)                              ]

  [(where (c_1 ... (($tup) <: τ_1) c_2 ...) Cs)
   ---------------------------------------------------------
   (sol_gen Cs V τ_1 ($tup) V)                              ]

  ; function type
  [(where (c_1 ... (τ_1 <: (τ_2 -> τ_3)) c_2 ...) Cs)

   (side-condition ,(not (term (in-map V_1 τ_1))))
   (where α (FreshVar V_1))
   (where V_2 (τ_1 : α V_1))
   
   (sol_gen Cs V_2 τ_2 t_2 V_3)
   (sol_gen Cs V_3 τ_3 t_3 V_4)

   (side-condition ,(or (redex-match? core-lang-typed
                                      (in-hole Ct α)
                                      (term t_2))
                        (redex-match? core-lang-typed
                                      (in-hole Ct α)
                                      (term t_3))))
   --------------------------------------------------------------------
   (sol_gen Cs V_1 τ_1 (μ α (t_2 -> t_3)) V_4)                            ]

  [(where (c_1 ... (τ_1 <: (τ_2 -> τ_3)) c_2 ...) Cs)

   (side-condition ,(not (term (in-map V_1 τ_1))))
   (where α (FreshVar V_1))
   (where V_2 (τ_1 : α V_1))
   
   (sol_gen Cs V_2 τ_2 t_2 V_3)
   (sol_gen Cs V_3 τ_3 t_3 V_4)

   (side-condition ,(not (or (redex-match? core-lang-typed
                                           (in-hole Ct α)
                                           (term t_2))
                             (redex-match? core-lang-typed
                                           (in-hole Ct α)
                                           (term t_3)))))
   --------------------------------------------------------------------
   (sol_gen Cs V_1 τ_1 (t_2 -> t_3) V_4)                            ]

  [(where (c_1 ... ((τ_2 -> τ_3) <: τ_1) c_2 ...) Cs)

   (side-condition ,(not (term (in-map V_1 τ_1))))
   (where α (FreshVar V_1))
   (where V_2 (τ_1 : α V_1))
   
   (sol_gen Cs V_2 τ_2 t_2 V_3)
   (sol_gen Cs V_3 τ_3 t_3 V_4)

   (side-condition ,(or (redex-match? core-lang-typed
                                      (in-hole Ct α)
                                      (term t_2))
                        (redex-match? core-lang-typed
                                      (in-hole Ct α)
                                      (term t_3))))
   --------------------------------------------------------------------
   (sol_gen Cs V_1 τ_1 (μ α (t_2 -> t_3)) V_4)                            ]

  [(where (c_1 ... ((τ_2 -> τ_3) <: τ_1) c_2 ...) Cs)

   (side-condition ,(not (term (in-map V_1 τ_1))))
   (where α (FreshVar V_1))
   (where V_2 (τ_1 : α V_1))
   
   (sol_gen Cs V_2 τ_2 t_2 V_3)
   (sol_gen Cs V_3 τ_3 t_3 V_4)

   (side-condition ,(not (or (redex-match? core-lang-typed
                                           (in-hole Ct α)
                                           (term t_2))
                             (redex-match? core-lang-typed
                                           (in-hole Ct α)
                                           (term t_3)))))
   --------------------------------------------------------------------
   (sol_gen Cs V_1 τ_1 (t_2 -> t_3) V_4)                            ]

  ; Table constructor
  [(where (c_1 ... (τ <: ((\{ \}) weakness)) c_2 ...) Cs)
   --------------------------------------------------------------------
   (sol_gen Cs V τ ((\{ \}) weakness) V)]

  ; constraints about presence of table fields
  [(where (c_1 ... (τ_1 <: (\[ τ_2 \] : τ_3)) c_2 ...)
          Cs)

   (side-condition ,(not (term (in-map V_1 τ_1))))
   (where α (FreshVar V_1))
   (where V_2 (τ_1 : α V_1))

   ; look for every constraint of the form (τ_1 <: (\[ τ_2 \] : τ_3))
   (where ((\[ τ_4 \] : τ_5) ...) (combine_table_cons Cs τ_1))

   (sol_gen_table_fields Cs V_2 ((\[ τ_4 \] : τ_5) ...)
                         ((\[ t_1 \] : t_2) ...) V_3)

   ; fix for duplicated fields
   (where ((\[ t_3 \] : t_4) ...)
          ,(remove-duplicates (term ((\[ t_1 \] : t_2) ...))))

   ; recursive type
   (side-condition ,(redex-match? core-lang-typed
                                  (in-hole Ct α)
                                  (term ((\{ (\[ t_3 \] : t_4) ... \})
                                         weakness))))
   --------------------------------------------------------------------
   (sol_gen Cs V_1 τ_1 (μ α ((\{ (\[ t_3 \] : t_4) ... \}) strong)) V_3)]

  
  [(where (c_1 ... (τ_1 <: (\[ τ_2 \] : τ_3)) c_2 ...)
          Cs)

   (side-condition ,(not (term (in-map V_1 τ_1))))
   (where α (FreshVar V_1))
   (where V_2 (τ_1 : α V_1))

   ; look for every constraint of the form (τ_1 <: (\[ τ_2 \] : τ_3))
   (where ((\[ τ_4 \] : τ_5) ...) (combine_table_cons Cs τ_1))

   (sol_gen_table_fields Cs V_2 ((\[ τ_4 \] : τ_5) ...)
                         ((\[ t_1 \] : t_2) ...) V_3)

   ; fix for duplicated fields
   (where ((\[ t_3 \] : t_4) ...)
          ,(remove-duplicates (term ((\[ t_1 \] : t_2) ...))))

    ; not a recursive type
   (side-condition ,(not (redex-match? core-lang-typed
                                       (in-hole Ct α)
                                       (term ((\{ (\[ t_3 \] : t_4) ... \})
                                              weakness)))))
   --------------------------------------------------------------------
   (sol_gen Cs V_1 τ_1 ((\{ (\[ t_3 \] : t_4) ... \}) strong) V_3)]

  ; solution for type vars that must have a supremum type
  [(where (c_1 ... (τ <: (Name label typevar)) c_2 ...) Cs)
   (sol_gen (c_1 ... c_2 ...) V_1 τ t V_2)
   --------------------------------------------------------------------
   (sol_gen Cs V_1 (Name label typevar) t V_2)]

  [(where (c_1 ... (τ_2 <: (τ_1 returntypevar)) c_2 ...) Cs)
   (sol_gen (c_1 ... c_2 ...) V_1 τ_2 t V_2)
   --------------------------------------------------------------------
   (sol_gen Cs V_1 (τ_1 returntypevar) t V_2)]

  ; Dynamic type
  [(side-condition
    ,(not
      (redex-match?
       core-lang-typed
       (c_1 ...
        (side-condition c
                        (or
                         ; no constraint of the form τ_1 <: χ or χ <: τ_1 
                         (redex-match? core-lang-typed
                                      (side-condition
                                       (ρ_1 <: ρ_2)
                                       (or (and (equal? (term τ_1)
                                                        (term ρ_1))
                                                (redex-match?
                                                 core-lang-typed
                                                 χ
                                                 (term ρ_2)))

                                           (and (equal? (term τ_1)
                                                        (term ρ_1))
                                                (redex-match?
                                                 core-lang-typed
                                                 (\{ (\[ τ_1 \] = τ_2) ... \})
                                                 (term ρ_2)))
                                           
                                           (and (equal? (term τ_1)
                                                        (term ρ_2))
                                                (redex-match?
                                                 core-lang-typed
                                                 χ
                                                 (term ρ_1)))

                                           (and (equal? (term τ_1)
                                                        (term ρ_2))
                                                (redex-match?
                                                 core-lang-typed
                                                 (\{ (\[ τ_1 \] = τ_2) ... \})
                                                 (term ρ_1)))))
                                      (term c))
                         ; no constraint of the form τ_1 <: ϕ ∨ ϕ <: τ_1
                         (redex-match? core-lang-typed
                                      (side-condition
                                       (τ_2 <: χ ∨ χ <: τ_2)
                                       (equal? (term τ_1)
                                               (term τ_2)))
                                      (term c))

                         ; no constraint about table fields
                         (redex-match? core-lang-typed
                                      (side-condition
                                       (τ_2 <: (\[ τ_3 \] : τ_4))
                                       (equal? (term τ_1)
                                               (term τ_2)))
                                      (term c))

                         (redex-match? core-lang-typed
                                      (side-condition
                                       (τ_2 <: τ_3 τ_4)
                                       (equal? (term τ_1)
                                               (term τ_2)))
                                      (term c))
                         ))
        c_2 ...)
       (term Cs))))
   --------------------------------------------------------------------
   (sol_gen Cs V τ_1 dyn V)]
  )

(provide sol_gen)
;                                  
;                                  
;                                  
;                                  
;     ;                            
;     ;                            
;   ;;;;;;  ;    ;  ;;;;;    ;;;;  
;     ;      ;   ;  ;;  ;;  ;;  ;; 
;     ;      ;  ;   ;    ;  ;    ; 
;     ;      ;  ;   ;    ;  ;;;;;; 
;     ;       ; ;   ;    ;  ;      
;     ;       ;;    ;;  ;;  ;;   ; 
;      ;;;     ;    ;;;;;    ;;;;  
;              ;    ;              
;             ;     ;              
;            ;;     ;              
;

; Given a set of constraints and a type variable, returns the supremum type
; amongs the solutions returned by judgment-holds
; PARAMS
; Cs : the set of constraints
; τ : the type var whose supremum type will be inferred
; PRE : {well_formed Cs}
;
; RETURNS
; supremum type, among possible solutions for Cs or dyn, if no solution can be
; found
(define-metafunction core-lang-typed
  gen_solution : Cs τ -> t

  [(gen_solution Cs τ)
   t_3

   (where ((t_1 V_1) (t_2 V_2) ...) ,(judgment-holds
                                      (sol_gen Cs · τ t V)
                                      (t V)))
   
   (where t_3 (supertype (t_1 t_2 ...) t_1))]

  ; No solution for τ, given Cs 
  [(gen_solution Cs τ)
   dyn]
  )

(define-metafunction core-lang-typed
  type_varids : Cs ((Name label typevar) ...) -> ((Name : t) ...)

  [(type_varids Cs ())
   ()]

  [(type_varids Cs ((Name_1 label typevar) τ ...))
   ((Name_1 : t_3) (Name_2 : t_2) ...)
   
;   (where ((t_1 V_1) (t_2 V_2) ...) ,(judgment-holds
;                                      (sol_gen Cs
;                                               ·
;                                               (Name_1 label typevar) t V)
;                                      (t V)))
;   
;   (where t_3 (supertype (t_1 t_2 ...) t_1))

   (where t_3 (gen_solution Cs (Name_1 label typevar)))

   (where ((Name_2 : t_2) ...) (type_varids Cs (τ ...)))
   ]

  ; Default: dyn
  [(type_varids Cs ((Name_1 label typevar) τ ...))
   ((Name_1 : dyn) (Name_2 : t_2) ...)
   
   (where ((Name_2 : t_2) ...) (type_varids Cs (τ ...)))]
  )

; returns the supertype of a list of types, according to subtyping
; PARAMS
; (t ...) : list of types from which the supertype must be chosen
; t : max. type found so far
(define-metafunction core-lang-typed
  supertype : (t ...) t -> t

  [(supertype () t)
   t]

  [(supertype (t_1 t_2 ...) t_3)
   (supertype (t_2 ...) t_4)

   (where t_4 (supremum_type t_1 t_3))]
  )

(provide supertype)

(define-metafunction core-lang-typed
  type_typevar_e : Cs τ -> any
  
  [(type_typevar_e Cs nil)
   nil]

  [(type_typevar_e Cs Boolean)
   Boolean]

  [(type_typevar_e Cs String)
   String]

  [(type_typevar_e Cs Number)
   Number]

  [(type_typevar_e Cs (function Name_1 ((Name_2 label_1 typevar)
                                        (Name_3 label_2 typevar) ...) τ end))
   (t_1 function Name_1 ((Name_2 : t_2) (Name_3 : t_3) ...)
        (type_typevar_s Cs τ) end)

   ;   (where ((t_3 V_3) (t_4 V_4) ...)
   ;          ,(judgment-holds
   ;            (sol_gen
   ;             Cs
   ;             ·
   ;             ((function Name_1 ((Name_2 label_1 typevar)
   ;                                (Name_3 label_2 typevar)...) τ end)
   ;              returntypevar) t V)
   ;            (t V)))
   ;
   ;   ; get the most general type
   ;   (where t_5 (supertype (t_3 t_4 ...) t_3))

   (where t_1 (gen_solution Cs ((function Name_1 ((Name_2 label_1 typevar)
                                                  (Name_3 label_2 typevar)...)
                                          τ end)
                                returntypevar)))
   
   (where ((Name_2 : t_2) (Name_3 : t_3) ...)
          (type_varids Cs ((Name_2 label_1 typevar)
                           (Name_3 label_2 typevar) ...)))
   ]

  [(type_typevar_e Cs (function Name_1 () τ end))
   (t function Name_1 () (type_typevar_s Cs τ) end)

   ;   (where ((t_3 V_3) (t_4 V_4) ...)
   ;          ,(judgment-holds
   ;            (sol_gen
   ;             Cs
   ;             ·
   ;             ((function Name_1 () τ end)
   ;              returntypevar) t V)
   ;            (t V)))
   ;
   ;   ; get the most general type
   ;   (where t_5 (supertype (t_3 t_4 ...) t_3))

   (where t (gen_solution Cs ((function Name_1 ()τ end) returntypevar)))
   ]

  [(type_typevar_e Cs <<<)
   <<<]

  [(type_typevar_e Cs (Name label typevar))
   Name]

  [(type_typevar_e Cs (τ_1 \[ τ_2 \]))
   ((type_typevar_e Cs τ_1) \[ (type_typevar_e Cs τ_2) \])]

  [(type_typevar_e Cs (τ_1 ()))
   ((type_typevar_e Cs τ_1) ())]

  [(type_typevar_e Cs (τ_1 (τ_2 ...)))
   ((type_typevar_e Cs τ_1) (e ...))

   (where (e ...) (type_typevar_el Cs (τ_2 ...)))]

  [(type_typevar_e Cs (τ_1 : Name (τ_2 ...)))
   ((type_typevar_e Cs τ_1) : Name (type_typevar_el Cs (τ_2 ...)))]

  [(type_typevar_e Cs (\( τ \)))
   (\( (type_typevar_e Cs τ) \))]

  [(type_typevar_e Cs (\{ τ_1 τ_2 ... \}))
   (\{ any ... \})

   (where (any ...) (type_typevar_el Cs (τ_1 τ_2 ...)))]

  [(type_typevar_e Cs (\{ \}))
   (\{ \})]

  [(type_typevar_e Cs (\[ τ_1 \] = τ_2))
   (\[ (type_typevar_e Cs τ_1) \] = (type_typevar_e Cs τ_2))]
  
  [(type_typevar_e Cs (τ_1 binop τ_2))
   ((type_typevar_e Cs τ_1) binop (type_typevar_e Cs τ_2))]

  [(type_typevar_e Cs (unop τ))
   (unop (type_typevar_e Cs τ))]
  ) 

(provide type_typevar_e)

(define-metafunction core-lang-typed
  type_typevar_el : Cs (τ ...) -> (any ...)

  [(type_typevar_el Cs (τ))
   ((type_typevar_e Cs τ))]

  [(type_typevar_el Cs (τ_1 τ_2 τ_3 ...))
   ((type_typevar_e Cs τ_1) any ...)

   (where (any ...) (type_typevar_el Cs (τ_2 τ_3 ...)))]

  )

(define-metafunction core-lang-typed
  type_typevar_s : Cs τ -> s
  
  [(type_typevar_s Cs \;)
   \;]

  [(type_typevar_s Cs break)
   break]

  [(type_typevar_s Cs (return))
   (return)]
  ; only one returned value
  [(type_typevar_s Cs (return τ))
   (return any)

   (where any (type_typevar_e Cs τ))]

  ; TODO: quick fix, see if there should be a particular treatment for tuples
  [(type_typevar_s Cs (return ($tup τ ...)))
   (return any ...)

   (where (any ...) (type_typevar_el Cs (τ ...)))]

  [(type_typevar_s Cs ($statFunCall τ_1 ()))
   ($statFunCall e_1 ())
   
   (where (e_1 ()) (type_typevar_e Cs (τ_1 ())))]
  
  
  [(type_typevar_s Cs ($statFunCall τ_1 (τ_2 τ_3 ...)))
   ($statFunCall e_1 (e_2 e_3 ...))

   (where (e_1 (e_2 e_3 ...)) (type_typevar_e Cs (τ_1 (τ_2 τ_3 ...))))]

  [(type_typevar_s Cs ($statFunCall τ_1 : Name (τ_2 ...)))
   ($statFunCall e_1 : Name (e_2 ...))

   (where (e_1 : Name (e_2 ...)) (type_typevar_e Cs (τ_1 : Name (τ_2 ...))))]

  ; TODO: just one variable
  [(type_typevar_s Cs (τ_1 = τ_2))
   (any_1 = any_2)

   (where any_1 (type_typevar_e Cs τ_1))
   (where any_2 (type_typevar_e Cs τ_2))]

  [(type_typevar_s Cs (do τ end))
   (do (type_typevar_s Cs τ) end)]

  [(type_typevar_s Cs (if τ_1 then τ_2 else τ_3 end))
   (if (type_typevar_e Cs τ_1)
       then (type_typevar_s Cs τ_2)
       else (type_typevar_s Cs τ_3) end)]
  
  [(type_typevar_s Cs (while τ_1 do τ_2 end))
   (while (type_typevar_e Cs τ_1) do (type_typevar_s Cs τ_2) end)]

  [(type_typevar_s Cs (local (Name label typevar) ... = τ_1 ... in τ_2 end))
   (local (Name_2 : t_2) ... = any_1 ...
     in (type_typevar_s Cs τ_2) end)

   (where (any_1 ...) (type_typevar_el Cs (τ_1  ...)))
   (where ((Name_2 : t_2) ...) (type_varids Cs ((Name label typevar) ...)))]

  [(type_typevar_s Cs (τ_1 τ_2))
   ((type_typevar_s Cs τ_1) (type_typevar_s Cs τ_2))]

  [(type_typevar_s Cs (τ_1 τ_2 τ_3 τ_4 ...))
   ((type_typevar_s Cs τ_1) s ...)

   (where (s ...) (type_typevar_s Cs (τ_2 τ_3 τ_4 ...)))]
  )

(define-metafunction core-lang-typed
  type_term : any -> any

  [(type_term s_1)
   s_2
   
   ; Generation of constraints
   (where ((τ_2 γ Cs_1)) ,(judgment-holds (cons_gen
                                           ·
                                           s_1 τ γ Cs)
                                          (τ γ Cs)))

   ; refine types
   (where Cs_2 (combine_clos_refine_steps Cs_1))

   ; Closure
   (where Cs_3 (combine_clos_steps Cs_2))

   ; Check for well-formedness
   (where #t ,(judgment-holds (well_form_cons_set Cs_3)))
   
   ; Reconstruct typed term
   (where s_2 (type_typevar_s Cs_3 τ_2))]

  [(type_term e_1)
   e_2
   
   ; Generation of constraints
   (where ((τ_2 γ Cs_1)) ,(judgment-holds (cons_gen
                                           ·
                                           e_1 τ γ Cs)
                                          (τ γ Cs)))

   ; refine types
   (where Cs_2 (combine_clos_refine_steps Cs_1))

   ; Closure
   (where Cs_3 (combine_clos_steps Cs_2))

   ; Check for well-formedness
   (where #t ,(judgment-holds (well_form_cons_set Cs_3)))

   ; Reconstruct typed term
   (where e_2 (type_typevar_e Cs_3 τ_2))]

  ;Terms that cannot be typed
  [(type_term any_1)
   (-> "the term cannot be typed" <-)]
  )

(provide type_term)
;                                                          
;                   ;;;                                    
;                     ;                                    
;                     ;                               ;    
;                     ;                               ;    
;    ;;;;    ;;;;     ;              ;;;;     ;;;   ;;;;;; 
;   ;    ;  ;;  ;;    ;             ;    ;   ;   ;    ;    
;   ;       ;    ;    ;             ;            ;    ;    
;    ;;;;   ;    ;    ;              ;;;;    ;;;;;    ;    
;        ;  ;    ;    ;                  ;  ;    ;    ;    
;   ;    ;  ;;  ;;    ;             ;    ;  ;   ;;    ;    
;    ;;;;    ;;;;      ;;;           ;;;;    ;;; ;     ;;; 
;                                                          
;                                                          
;                                                          
;                                                          
;

(define-judgment-form
  core-lang-typed
  #:mode (solutions_sat I)
  #:contract (solutions_sat Cs) 

  ; solSat
  [(sol_sat (c_1 c_2 ...) c_1)
   (sol_sat (c_1 c_2 ...) c_2) ...
   --------------------------------
   (solutions_sat (c_1 c_2 ...))]
  )

(define-judgment-form
  core-lang-typed
  #:mode (sol_sat I I)
  #:contract (sol_sat Cs c) 

  ; solSub
  [(where t_1 (gen_solution Cs τ_1))
   (where t_2 (gen_solution Cs τ_2))
   (side-condition (subtyping_rel t_1 t_2))
   ------------------------------------------------------------
   (sol_sat Cs (τ_1 <: τ_2))                                    ]

  ; solSubFunc
  [(where t_1 (gen_solution Cs τ_1))
   (where t_2 (gen_solution Cs τ_2))
   (where t_3 (gen_solution Cs τ_3))
   (side-condition (subtyping_rel t_1 (t_2 -> t_3)))
   ------------------------------------------------------------------
   (sol_sat Cs (τ_1 <: (τ_2 -> τ_3)))                           ]

  ; solPt
  [(where t (gen_solution Cs τ))
   (side-condition (subtyping_rel t pt))
   -------------------------------------------------------
   (sol_sat Cs (τ <: pt))                                 ]

  [(where t_1 (gen_solution Cs τ_1))
   (sol_sat_fields Cs ((\[ τ_2 \] : τ_3) ...) ((\[ t_2 \] : t_3) ...))
   (side-condition (subtyping_rel t_1
                                  ((\{ (\[ t_2 \] : t_3) ... \}) weakness)))
   -------------------------------------------------------
   (sol_sat Cs (τ_1 <: ((\{ (\[ τ_2 \] : τ_3) ... \}) weakness))) ]
  )

(define-judgment-form
  core-lang-typed
  #:mode (sol_sat_fields I I O)
  #:contract (sol_sat_fields Cs ((\[ τ \] : τ) ...) ((\[ t \] : t) ...)) 

  ; base
  [(where t_1 (gen_solution Cs τ_1))
   (where t_2 (gen_solution Cs τ_2))
   -----------------------------------------------------------
   (sol_sat_fields Cs ((\[ τ_1 \] : τ_2)) ((\[ t_1 \] : t_2)))]

  [(where t_1 (gen_solution Cs τ_1))
   (where t_2 (gen_solution Cs τ_2))
   (sol_sat_fields Cs ((\[ τ_3 \] : τ_4) (\[ τ_5 \] : τ_6) ...)
                   ((\[ t_3 \] : t_4) (\[ t_5 \] : t_6) ...))
   -----------------------------------------------------------
   (sol_sat_fields Cs ((\[ τ_1 \] : τ_2)
                       (\[ τ_3 \] : τ_4) (\[ τ_5 \] : τ_6) ...)
                   ((\[ t_1 \] : t_2) (\[ t_3 \] : t_4) (\[ t_5 \] : t_6) ...))]
  )
