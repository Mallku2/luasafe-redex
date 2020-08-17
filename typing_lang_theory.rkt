#lang racket

(require redex
         "./grammar.rkt"
         "./desugar/parser.rkt")

(provide (all-defined-out))

(define-extended-language core-lang-typed core-lang
  
  ;                                                        
  ;                                                        
  ;                                                        
  ;                                 ;                      
  ;     ;                           ;                      
  ;     ;                                                  
  ;   ;;;;;;   ;     ;  ; ;;;     ;;;     ; ;;;;     ;;; ; 
  ;     ;       ;   ;   ;;   ;      ;     ;;   ;;   ;   ;; 
  ;     ;       ;   ;   ;     ;     ;     ;     ;  ;     ; 
  ;     ;       ;   ;   ;     ;     ;     ;     ;  ;     ; 
  ;     ;        ; ;    ;     ;     ;     ;     ;  ;     ; 
  ;     ;        ; ;    ;     ;     ;     ;     ;  ;     ; 
  ;     ;         ;;    ;;   ;      ;     ;     ;   ;   ;; 
  ;      ;;;      ;     ; ;;;    ;;;;;;;  ;     ;    ;;; ; 
  ;               ;     ;                                ; 
  ;               ;     ;                           ;   ;; 
  ;             ;;      ;                            ;;;;  
  ;                                                        
  [Γ · (Name : t Γ)]

  [Π · (piid : et Π)]

  [piid Name <<<]

  ;;;;;;;;;;;;;;;;;;;;;
  ; Lua types
  ;;;;;;;;;;;;;;;;;;;;;

  ; Lua types
  [t  pt
      dyn ; dyn type
      ctet
;      (t -> t) ; function types
;      (μ α t) ; recursive types
;      ((\{ (\[ t \] : t) ... \}) weakness)
      tt
      ; type system's variables
      α]

  ; type system's var
  [α (tsv natural)]
  
  ; primitive types
  [pt num
      bool
      str ; the whole set of strings
      
      strWeak
      strMode
      strGc
      
      st]

  [strWeak strK
           strV
           strKv]

  ; singleton types
  [st (Number : num) (Boolean : bool) (String : str) (nil : nil)]

  ; Tuple types
  [tt ($tup t ...)]

  ; possible weakness of weak tables
  [weaktable wk
             wv
             wkv]
  
  [weakness weaktable
            strong]

  

  ;;;;;;;;;;;;;;;;;;;;;
  ; Weak table's related types
  ;;;;;;;;;;;;;;;;;;;;;

  [ctet (t -> t) ; function types
        (μ α t) ; recursive types
        ((\{ (\[ t \] : t) ... \}) weakness)]
  
  [ncte pt]


  
  ; Contexts for type terms, to help defininf predicates about a given type
  ; structure
  [Ct hole
      (Ct -> t)
      (t -> Ct)
      (μ α Ct) ; recursive types
      ; table type
      ((\{ (\[ t_1 \] : t_2) ... (\[ Ct \] : t)
           (\[ t_3 \] : t_4) ... \}) weakness)
      ((\{ (\[ t_1 \] : t_2) ... (\[ t \] : Ct)
           (\[ t_3 \] : t_4) ... \}) weakness)
      ; tuple type
      ($tup t_1 ... Ct t_2 ...)
      ]
  
  ;                                                                          
  ;                                                                          
  ;                                                                          
  ;                                                   ;                  ;;; 
  ;     ;                                             ;                 ;    
  ;     ;                                                               ;    
  ;   ;;;;;;   ;     ;  ; ;;;      ;;;              ;;;     ; ;;;;    ;;;;;; 
  ;     ;       ;   ;   ;;   ;    ;   ;               ;     ;;   ;;     ;    
  ;     ;       ;   ;   ;     ;  ;     ;              ;     ;     ;     ;    
  ;     ;       ;   ;   ;     ;  ;     ;              ;     ;     ;     ;    
  ;     ;        ; ;    ;     ;  ;;;;;;;              ;     ;     ;     ;    
  ;     ;        ; ;    ;     ;  ;                    ;     ;     ;     ;    
  ;     ;         ;;    ;;   ;    ;    ;              ;     ;     ;     ;    
  ;      ;;;      ;     ; ;;;      ;;;;            ;;;;;;;  ;     ;     ;    
  ;               ;     ;                                                    
  ;               ;     ;                                                    
  ;             ;;      ;                                                    
  ;                                                                          

  ; Typed ext lang
  ; Just add type tags to bounding occurrences of variables: local, formal
  ; parameters of functions.
  [typedvar (Name : t)]
  
  [score ....
         (local typedvar ... = e ... in s end)]

  [parameters ....
              (typedvar ...)
              (typedvar ... (<<< : t))]

  [functiondef ....
               (t function Name parameters s end)]

  ; extensions to C for the typed language
  [Ccore ....
         (local typedvar = Ce in s end)
         (local typedvar ... = e_1 ... Ce e_2 ... in s end)
         (local typedvar ... = e ... in C end)
         ; this context is added to better express execution flow of 3-address code
         (local C in s end)
         (t function Name parameters C end)]

  [Cenw ....
        (function Name_1 (Name_2 ...) Cnw end)
        (function Name_1 (Name_2 ... <<<) Cnw end)]

  [Ccorenw ....
           (local typedvar ... = e ... Cenw e ... in s end)
           (local typedvar ... = e ... in Cnw end)]

  [e ....
     t]
  
  ; typevariable
  [τ ; fundamental extensions to the syntax introduced by τ
     (Name label typevar) ; to identify each occurrence of a given var. ident.
     (τ paramtypevar) ; to identify formal parameters of functions
     (τ returntypevar) ; to identify returned values from functions
     (\[ τ \] = τ) ; to identify table fields
     ; product type: for functions' domains
     ($tup τ_1 ...)
     ;num ; field keys of fields of the form e, in table constructors
     
     ; redefinition of s and e
     ; stats
     \;
     break
     (return τ ...)
     (τ_1 τ_2 ... = τ_3 τ_4 ...)
     (do τ end)
     (if τ then τ else τ end)
     (while τ do τ end)
     (local τ ... = τ ... in τ end)
     ($statFunCall τ (τ ...))
     ($statFunCall τ : Name (τ ...))
     (τ_1 τ_2 τ_3 ...)
     
     ; exps
     nil Boolean Number String
     <<<
     ; var
     Name 
     (τ \[ τ \])
     (function Name (τ ...) τ end)
     ; To allow function calls in protected mode, in place of expressions.
     (τ (τ ...))
     (τ : Name (τ ...))
     (\( τ \))
     (\[ τ \] : τ) ; table fields with keys
     (\{ τ ... \})
     (τ binop τ)
     (unop τ)

     ; TODO: check this
     ((\{ (\[ τ \] : τ) ... \}) weakness)
     ]

  ; constraints
  [ρ τ
     χ
     (\[ τ \] : τ)]

  ; type vars that refer. to type terms
  [χ ϕ υ ((\{ (\[ τ \] : τ) (\[ τ \] : τ) ... \}) weakness) ($tup τ_1 τ_2 ...)]

  ; not minimals
  [ϕ num str bool]
  
  ; minimals of subt. rel
  [υ (nil : nil) (Number : num) (Boolean : bool) (String : str) ($tup) (τ -> τ)
     ((\{ \}) weakness)]

  ; Constraint
  [c (ρ <: ρ)
     (ρ <: ρ ∨ ρ <: ρ)
     (τ <: τ τ)]
  ; set of constraints
  [Cs (c ...)]

  ; Solution
  [S · (τ : t S)]

  ; Environment
  [label natural
         Name ; to include function's label
         ]
  
  [γ · (Name : label γ)]

  ; type variable function: allows us to introduce μ-binders, by maintaining
  ; mapping from type variables to variables from the type system
  [V · (τ : α V)]
  )

;                          
;                          
;                          
;                          
;      ;                   
;     ; ;                  
;     ; ;   ;    ;  ;;  ;; 
;     ; ;   ;    ;   ;  ;  
;    ;   ;  ;    ;    ;;   
;    ;   ;  ;    ;    ;;   
;    ;;;;;  ;    ;    ;;   
;   ;;   ;; ;   ;;   ;  ;  
;   ;     ;  ;;; ;  ;;  ;; 
;                          
;                          

; efficient matchs.
(define is_dynt?
  (redex-match? core-lang-typed
                dyn))

(define is_table_typevar?
  (redex-match? core-lang-typed
                ((\{ τ ... \}) weakness)))

(define is_tablet?
  (redex-match? core-lang-typed
                ((\{ (\[ t_1 \] : t_2) ... \}) weakness)))

(define is_table_field_typevar?
  (redex-match? core-lang-typed
                (\[ τ \] : τ)))

(define is_tablecons_typevar?
  (redex-match? core-lang-typed
                (\{ (\[ τ_1 \] = τ_2) ... \})))

(define is_functiont?
  (redex-match? core-lang-typed
                (τ_1 -> τ_2)))

(define is_emptupt?
  (redex-match? core-lang-typed
                    ($tup)))

(define is_noemptupt?
  (redex-match? core-lang-typed
                    ($tup τ_1 τ_2 ...)))

(define (is_tupt? t)
  (or (is_emptupt? t)
      (is_noemptupt? t)))

; primitive types

(define is_pt?
  (redex-match? core-lang-typed
                pt))

(define is_nil_litt?
  (redex-match? core-lang-typed
                (nil : nil)))

(define is_numt?
  (redex-match? core-lang-typed
                num))

(define is_num_litt?
  (redex-match? core-lang-typed
                (Number : num)))

(define is_strt?
  (redex-match? core-lang-typed
                str))

(define is_str_litt?
  (redex-match? core-lang-typed
                (String : str)))

(define is_boolt?
  (redex-match? core-lang-typed
                bool))

(define is_bool_litt?
  (redex-match? core-lang-typed
                (Boolean : bool)))


(provide is_dynt? is_tablet? is_functiont? is_tupt? is_emptupt? is_noemptupt?
         is_nil_litt? is_pt? is_strt?)
;                          
;                                          ;                               
;                                          ;                               
;                                                                          
;   ;;;;;;     ;;;;   ; ;;;    ; ;;;     ;;;     ; ;;;;     ;;; ;   ;;;;;  
;   ;  ;  ;   ;    ;  ;;   ;   ;;   ;      ;     ;;   ;;   ;   ;;  ;     ; 
;   ;  ;  ;        ;  ;     ;  ;     ;     ;     ;     ;  ;     ;  ;       
;   ;  ;  ;   ;;;;;;  ;     ;  ;     ;     ;     ;     ;  ;     ;  ;;;;    
;   ;  ;  ;  ;;    ;  ;     ;  ;     ;     ;     ;     ;  ;     ;      ;;; 
;   ;  ;  ;  ;     ;  ;     ;  ;     ;     ;     ;     ;  ;     ;        ; 
;   ;  ;  ;  ;    ;;  ;;   ;   ;;   ;      ;     ;     ;   ;   ;;  ;     ; 
;   ;  ;  ;   ;;;; ;  ; ;;;    ; ;;;    ;;;;;;;  ;     ;    ;;; ;   ;;;;;  
;                     ;        ;                                ;          
;                     ;        ;                           ;   ;;          
;                     ;        ;                            ;;;;           
;                                                                          

; Extends a given environment γ with new mappings from the given (Name ...) to
; the its type variables
; RETURNS : (γ (τ ...)), the new environment and the type variable that
; correspond to each given identifier
(define-metafunction core-lang-typed
  extend_gamma : γ (Name ...) -> (γ (τ ...))

  [(extend_gamma γ_1 (Name))
   (γ_2 ((Name label typevar)))

   (where label ,(+ 1 (term (index-γ γ_1 $lab))))
   (where γ_2 (set (set γ_1 Name label) $lab ,(+ 1 (term label))))]

  [(extend_gamma γ_1 (Name_1 Name_2 ...))
   (γ_3 ((Name_1 label typevar) τ ...))

   (where label ,(+ 1 (term (index-γ γ_1 $lab))))
   (where γ_2 (set (set γ_1 Name_1 label) $lab ,(+ 1 (term label))))
   (where (γ_3 (τ ...)) (extend_gamma γ_2 (Name_2 ...)))]
  )

(define-metafunction core-lang-typed
  in-map : any any -> any

  [(in-map · any)
   #f]

  [(in-map (any_1 : any_2 any_3) any_1)
   #t]

  [(in-map (any_1 : any_2 any_3) any_4)
   (in-map any_3 any_4)]
  )

; Returns a fresh type system's var
(define-metafunction core-lang-typed
  FreshVar : V -> α

  [(FreshVar ·)
   (tsv 1)]

  [(FreshVar (τ : (tsv natural) V))
   (tsv ,(+ 1 (term natural)))]
  )

(define-metafunction core-lang-typed
  set : any any any -> any

  [(set · any_1 any_2)
   (any_1 : any_2 ·)]

  [(set (any_1 : any_2 any_3) any_1 any_4)
   (any_1 : any_4 any_3)]

  [(set (any_1 : any_2 any_3) any_4 any_5)
   (any_1 : any_2 (set any_3 any_4 any_5))]
  )

; removes a given binding
(define-metafunction core-lang-typed
  del : any any -> any

  [(del (any_1 : any_2 ·) any_1)
   ·]

  [(del (any_1 : any_2 any_3) any_1)
   any_3]

  [(del (any_1 : any_2 any_3) any_4)
   (any_1 : any_2 (del any_3 any_4))]
  )

; remove a list of bindings
(define-metafunction core-lang-typed
  del* : any (any ...) -> any

  [(del* any ())
   any]

  [(del* any_1 (any_2 any_3 ...))
   (del* (del any_1 any_2) (any_3 ...))]
  )

; Indexes a given mapping
; PARAMS:
; any_1 : mapping
; any_2 : key
; PRE: {in-map any_1 any_2}
(define-metafunction core-lang-typed
  index : any any -> any

  [(index (any_1 : any_2 ·) any_1)
   any_2]

  [(index (any_1 : any_2 any_3) any_1)
   any_2]

  [(index (any_1 : any_2 any_3) any_4)
   (index any_3 any_4)]
  )

(define-metafunction core-lang-typed
  index-γ : γ any -> label

  ; first occurrence of the variable
  [(index-γ · any)
   0]

  [(index-γ (any_1 : any_2 γ) any_1)
   any_2]

  [(index-γ (any_1 : any_2 γ) any_3)
   (index-γ γ any_3)]
  )


;                                                                                                     
;                                                                                                     
;                                                                                                     
;                     ;                                                                      ;;;;     
;                     ;          ;                                                              ;     
;                     ;          ;                                                              ;     
;    ;;;;;   ;     ;  ; ;;;    ;;;;;;   ;     ;  ; ;;;                        ; ;;;    ;;;      ;     
;   ;     ;  ;     ;  ;;   ;     ;       ;   ;   ;;   ;                       ;;   ;  ;   ;     ;     
;   ;        ;     ;  ;     ;    ;       ;   ;   ;     ;                      ;      ;     ;    ;     
;   ;;;;     ;     ;  ;     ;    ;       ;   ;   ;     ;                      ;      ;     ;    ;     
;       ;;;  ;     ;  ;     ;    ;        ; ;    ;     ;                      ;      ;;;;;;;    ;     
;         ;  ;     ;  ;     ;    ;        ; ;    ;     ;                      ;      ;          ;     
;   ;     ;  ;;   ;;  ;;   ;     ;         ;;    ;;   ;      ;;               ;       ;    ;    ;     
;    ;;;;;    ;;;; ;  ; ;;;       ;;;      ;     ; ;;;       ;;               ;        ;;;;      ;;;  
;                                          ;     ;                                                    
;                                          ;     ;                                                    
;                                        ;;      ;                                                    
;                                                                                                     
(define-metafunction core-lang-typed
  subtyping_rel : t t -> any

  [(subtyping_rel t dyn)
   #t]

  [(subtyping_rel dyn t)
   ; {t != dyn}
   #f]
  
  ; product type
  [(subtyping_rel ($tup) ($tup))
   #t]

  [(subtyping_rel ($tup t_1 t_2 ...) ($tup t_3 t_4 ...))
   (subtyping_rel ($tup t_2 ...) ($tup t_4 ...))

   (side-condition (term (subtyping_rel t_1 t_3)))]

  [(subtyping_rel ($tup t_1 ...) t_2)
   ; {t_2 ≠ dyn}
   #f]

  [(subtyping_rel t_1 ($tup t_2 ...))
   #f]
  
  ; tables: width, depth and permutation subtyping
  [(subtyping_rel ((\{ (\[ t_1 \] : t_2) ... \}) weakness_1)
                  ((\{ (\[ t_3 \] : t_4) (\[ t_5 \] : t_6) ... \}) weakness_2))
   
   (subtyping_rel ((\{ (\[ t_1 \] : t_2) ... \}) weakness_1)
                  ((\{ (\[ t_5 \] : t_6) ... \}) weakness_2))

   ; there is a field in tableconstructor with elements with a subtype of
   ; (\[ t_1 \] : t_2)
   (where (side-condition
           ((any_1 ...  (\[ t_7 \] : t_8) any_2 ...)
            ; to introduce (\[ t_3 \] : t_4) into scope
            ((\[ t_9 \] : t_10) any_3 ...))
           (and (term (subtyping_rel t_7 t_9))
                (term (subtyping_rel t_8 t_10))))
          
          (((\[ t_1 \] : t_2) ...)
           ((\[ t_3 \] : t_4) (\[ t_5 \] : t_6) ...)))]

  [(subtyping_rel ((\{ (\[ t_1 \] : t_2) ... \}) weakness_1)
                  ((\{ \}) weakness_2))
   #t]
  
  ; strings
  [(subtyping_rel (String : str) str)
   #t]

  ; numbers
  [(subtyping_rel (Number : num) num)
   #t]

  ; booleans
  [(subtyping_rel (Boolean : bool) bool)
   #t]
  

  ; function types
  [(subtyping_rel (t_1 -> t_2) (t_3 -> t_4))
   ,(and (term (subtyping_rel t_3 t_1))
         (term (subtyping_rel t_2 t_4)))]

  ; reflex.
  [(subtyping_rel t t)
   #t]

  [(subtyping_rel t_1 t_2)
   ; {t_1 ≠ t_2}
   #f]
  )

(define (is_num_chain? t)
  (or (is_numt? t)
      (is_num_litt? t)
      (is_dynt? t)))

(define (is_str_chain? t)
  (or (is_strt? t)
      (is_str_litt? t)
      (is_dynt? t)))

(define (is_bool_chain? t)
  (or (is_boolt? t)
      (is_bool_litt? t)
      (is_dynt? t)))

(define (is_nil_chain? t)
  (or (is_nil_litt? t)
      (is_dynt? t)))

(define (is_func_chain? t)
  (or (is_functiont? t)
      (is_dynt? t)))

(define (is_table_chain? t)
  (or (is_table_typevar? t)
      (is_table_field_typevar? t)
      (is_tablecons_typevar? t)
      (is_dynt? t)))

(define (is_emptup_chain? t)
  (or (is_emptupt? t)
      (is_dynt? t)))

(define (is_noemptup_chain? t)
  (or (is_noemptupt? t)
      (is_dynt? t)))

(provide subtyping_rel is_num_chain? is_str_chain? is_bool_chain? is_nil_chain?
         is_table_chain? is_func_chain? is_emptup_chain? is_noemptup_chain?)

; least upper bound of types
(define-metafunction core-lang-typed
  supremum_type : t t -> t

  [(supremum_type t t)
   t]

  ; {t_1 ≠ t_2}
  ; string
  [(supremum_type (String_1 : str) (String_2 : str))
   str]

  [(supremum_type (Number_1 : num) (Number_2 : num))
   num]

  [(supremum_type (Boolean_1 : bool) (Boolean_2 : bool))
   bool]
  
  [(supremum_type t_1 t_2)
   t_2

   (side-condition (term (subtyping_rel t_1 t_2)))]

  [(supremum_type t_1 t_2)
   t_1

   (side-condition (term (subtyping_rel t_2 t_1)))]

  [(supremum_type t_1 t_2)
   dyn]
  )

(define-metafunction core-lang-typed
  infimum_type : t t -> t

  [(infimum_type t t)
   t]

  ; {t_1 ≠ t_2}
  ; string
  [(infimum_type (String : str) str)
   (String : str)]

  [(infimum_type str (String : str))
   (String : str)]

  [(infimum_type (Number_1 : num) (Number_2 : num))
   num]

  [(infimum_type (Boolean_1 : bool) (Boolean_2 : bool))
   bool]
  
  [(infimum_type t_1 t_2)
   t_2

   (side-condition (term (subtyping_rel t_1 t_2)))]

  [(infimum_type t_1 t_2)
   t_1

   (side-condition (term (subtyping_rel t_2 t_1)))]

  [(infimum_type t_1 t_2)
   dyn]
  )

(provide infimum_type)

; least upper bound of environments
(define-metafunction core-lang-typed
  supremum_env : Γ Γ -> Γ

  [(supremum_env · Γ)
   Γ]

  [(supremum_env (Name : t_1 Γ_1) Γ_2)
   (Name : t_2 (supremum_env Γ_1 Γ_3))

   (side-condition (term (in-map Γ_2 Name)))

   (where t_2 (supremum_type t_1
                             (index Γ_2 Name)))

   (where Γ_3 (del Γ_2 Name))]

  ; {(in-map Γ_2 Name) == #f}
  [(supremum_env (Name : t_1 Γ_1) Γ_2)
   (Name : t_1 (supremum_env Γ_1 Γ_2))
   ]
  )

(define-metafunction core-lang-typed
  operands_type : any -> t

  [(operands_type arithop)
   (num)]

  [(operands_type relop)
   (same num str)]

  [(operands_type ..)
   (any num str)]

  [(operands_type ==)
   (same)]

  [(operands_type -)
   (num str)]

  [(operands_type not)
   (any)]

  [(operands_type \#)
   (str table)]
  )