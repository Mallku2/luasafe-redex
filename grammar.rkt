#lang racket

(require redex)

; Core language grammar definition
(define-language core-lang
  
  ;                                  
  ;                                  
  ;                                  
  ;                                  
  ;                                  
  ;                                  
  ;     ;;;    ;;;;    ;;;;    ;;;;  
  ;    ;   ;  ;;  ;;   ;;  ;  ;;  ;; 
  ;   ;       ;    ;   ;      ;    ; 
  ;   ;       ;    ;   ;      ;;;;;; 
  ;   ;       ;    ;   ;      ;      
  ;    ;   ;  ;;  ;;   ;      ;;   ; 
  ;     ;;;    ;;;;    ;       ;;;;  
  ;                                  
  ;                                  
  ;                                  
  ;                                  

  [score \;
         break
         (return e ...)
         ; stat funcall
         ($statFunCall prefixexp (e ...))
         ($statFunCall prefixexp : Name (e ...))
         (var_1 var_2 ... = e_1 e_2 ...)
         (do block end)
         (if e then block else block end)
         (while e do block end)
         ($iter e do block end)
         (local Name_1 Name_2 ... = e_1 e_2 ... in block end)]

  ; Lua's block of code: it helps to avoid an ambiguity in the grammar, between
  ; funcalls and concat. of stats
  [block score
         (score_1 score_2 score_3 ...)]

  [v nil Boolean Number String]

  ; Difference between statements and expressions is present also at a semantics
  ; level: eg., tuples' semantics is defined taking into account if they appear
  ; as an expression or as a statement, and the same with funcall
  [e v
     <<<
     var
     ; To allow function calls in protected mode, in place of expressions.
     functioncall
     ($builtIn builtinserv (e ...))
     (\( e \))
     tableconstructor
     ;(e_1 binop ... e_2)
     (e binop e)
     (unop e)
     functiondef]
  
  ; Built-in services' names, for use by the random generator of terms
  [builtinserv assert
               collectgarbage
               error
               pcall
               print
               rawequal
               select
               tonumber
               type
               xpcall
               setmetatable
               rawset
               ipairs
               next
               pairs
               load
               loadfile
               getmetatable
               tostring
               rawget
               rawlen
               require
               ; math
               math.abs
               math.acos
               math.asin
               math.atan
               math.ceil
               math.cos
               math.cosh
               math.deg
               math.exp
               math.floor
               math.fmod
               math.log
               math.max
               math.modf
               math.rad
               math.sin
               math.sinh
               math.sqrt
               math.tan
               math.tanh
               ; string
               string.len
               string.rep
               string.reverse
               string.sub
               ; table
               table.pack
               ; string
               string.dump
               ; table
               table.concat
               table.unpack]

  [Boolean true false]
  
  [vlist (v ...)]

  ; Variables' Identifiers' syntactic category, to ease the definition of the
  ; substitution function.
  [id Name
      <<<]

  [parameters (Name ...)
              (Name ... <<<)]

  ; This syntactic category is added to ease meta-functions' definitions. 
  [functiondef (function Name parameters block end)]

  [prefixexp var
             functioncall
             (\( e \))]
  
  [functioncall (prefixexp (e ...))
                (prefixexp : Name (e ...))]

  [field (\[ e \] = e)
         ; We need to allow fields like this
         e]
  
  [tableconstructor (\{ field ... \})]
  
  [arithop + - * / ^ %]
  
  [relop < <= > >=]

  ; Not short-circuit binop
  [strictbinop arithop relop == ..]
  
  [binop strictbinop and or]
  
  [unop - not \#]
  
  ; Name can be anything except a keyword of the language
  [Name variable-not-otherwise-mentioned]

  [var Name 
       (e \[ e \])]
  
  ; Number represents real (double-precision floating-point) numbers
  [Number real]
  
  [String string]

  ) 

; Export core-lang grammar definition
(provide core-lang)


;                                                                                  
;                                           ;;;                                    
;                                             ;                                    
;   ;;;;;;;           ;                       ;                                    
;   ;                 ;                       ;                                    
;   ;       ;;  ;;  ;;;;;;                    ;       ;;;   ; ;;;    ;;;;;         
;   ;        ;  ;     ;                       ;      ;   ;  ;;   ;  ;;  ;;         
;   ;;;;;;;   ;;      ;                       ;          ;  ;    ;  ;    ;         
;   ;         ;;      ;                       ;      ;;;;;  ;    ;  ;    ;         
;   ;         ;;      ;                       ;     ;    ;  ;    ;  ;    ;         
;   ;        ;  ;     ;       ;;              ;     ;   ;;  ;    ;  ;;  ;;    ;;   
;   ;;;;;;; ;;  ;;     ;;;    ;;               ;;;   ;;; ;  ;    ;   ;;; ;    ;;   
;                                                                        ;         
;                                                                    ;   ;         
;                                                                     ;;;          

; Extensions made to the grammar to ease the definition of the reduction
; semantics

; closures comprise core-lang stats + environment. We force this representation
; with the following extension to core-lang
(define-extended-language core+env-lang core-lang

  [e ....
     ; Run-time expressions, but codomain of the environment: the
     ; substitution could embed this expressions into statements
     r
     (< e ... >)]
  
  ; Ordinary refs
  [(r vr) (ref natural)]

  [var ....
       ; run-time expression
       evar]
  
  [evar r
        (v \[ v \])]

  )

(define-extended-language ext-lang core+env-lang

  ; Run-time statements
  [srun score
        ; functioncall will not be reduced to prefixexp ()
        ($statFunCall e (e ...))
        ($statFunCall e : Name (e ...))
        
        ; extensions
        (do s end) ; needs to be added to allow reduction into a do-end block 
        ;(s ReturnStat)
        (s Break)
        ; To help with the definition of well-formed programs, we exclude as
        ; many ill-formed programs as possible,  using the grammar
        (((tid \[ v \]) = v) WrongKey)
        (((v \[ v \]) = v) NonTable)
        (($statFunCall v (v ...)) WrongFunCall)
        ; renv is not an expression nor a value.
        (s (renv ...) LocalBody)
        ; To allow intermediate states of execution of a funtioncall
        (s (renv ...) RetStat)

        ; Error objects
        ($err v)
        ]

  [s srun
     (srun score_1 score_2 ...)]

  [v ....
     tid
     cid]
  
  [e ....
     (e (e ...))
     (e : Name (e ...))
     
     ; Run-time expressions
     ($err v)
     ; renv is not an expression nor a value. The previous rules for these
     ; constructions does not describe the renv added
     (s (renv ...) RetExp)
     (e ProtectedMode)
     (e ProtectedMode v)
     
     ; To help with the definition of well-formed programs, we exclude with the
     ; grammar as many ill-formed programs as possible
     ((v_1 (v_2 ...)) WrongFunCall)
     ((v \[ v \]) NonTable)
     ((tid \[ v \]) WrongKey)
     ((v arithop v) ArithWrongOps)
     ((v .. v) StrConcatWrongOps)
     ((v < v) OrdCompWrongOps)
     ((v <= v) OrdCompWrongOps)
     ((- v) NegWrongOp)
     ((\# v) StrLenWrongOp)
     ((v == v) EqFail)
     ]

  
  ; terms: to specify meta-functions and relations that work on both, s and e
  [aterm e s]
  
  [statlabel WrongKey ;metamethods
             NonTable
             WrongFunCall]

  [explabel WrongKey
            NonTable
            ArithWrongOps
            StrConcatWrongOps
            EqFail
            OrdCompWrongOps
            NegWrongOp
            StrLenWrongOp
            WrongFunCall]

  [(efield ef) (\[ v \] = v)
               v]
  
  [evaluatedtable (\{ efield ... \})]
    
  ;                                                  
  ;                                                  
  ;                                                  
  ;    ;;;;;    ;                                    
  ;   ;;    ;   ;                                    
  ;   ;       ;;;;;;   ;;;;    ;;;;    ;;;;    ;;;;  
  ;   ;;        ;     ;;  ;;   ;;  ;  ;;  ;;  ;    ; 
  ;    ;;;;;    ;     ;    ;   ;      ;    ;  ;      
  ;        ;;   ;     ;    ;   ;      ;;;;;;   ;;;;  
  ;         ;   ;     ;    ;   ;      ;            ; 
  ;   ;    ;;   ;     ;;  ;;   ;      ;;   ;  ;    ; 
  ;    ;;;;;     ;;;   ;;;;    ;       ;;;;    ;;;;  
  ;                                                  
  ;                                                  
  ;                                                  

  ; GC
  ; finalization
  [pos natural
       ⊥
       ⊘]
  
  [intreptable (tableconstructor tid pos)
               (tableconstructor nil pos)
               ]

  ; References for the representation of the environment 
  [renv (rEnv r)]
  
  [(objref tid) (objr natural)]

  ; closure id
  [cid (cl natural)]

  ; Elements from dom(θ)
  [objid tid
         cid]

  ; Elements from img(θ)
  [object intreptable
          functiondef]

  ; values store pair
  [(rst vsp) (r v)
             (refStdout String)]
  
  [σ (vsp ...)]

  ; id-object binding: objects store's pairs  
  [(objrefst osp) (tid intreptable)
                  (cid functiondef)]
  
  [θ (osp ...)]

  ;                                                                          
  ;                                                                          
  ;                                                                          
  ;   ;;;;;;;                           ;;;;                    ;            
  ;   ;                                ;    ;                   ;            
  ;   ;       ;    ;                  ;        ;;;;   ; ;;;   ;;;;;;  ;;  ;; 
  ;   ;       ;;  ;;                  ;       ;;  ;;  ;;   ;    ;      ;  ;  
  ;   ;;;;;;;  ;  ;                   ;       ;    ;  ;    ;    ;       ;;   
  ;   ;        ;  ;                   ;       ;    ;  ;    ;    ;       ;;   
  ;   ;        ;;;;                   ;       ;    ;  ;    ;    ;       ;;   
  ;   ;         ;;      ;;             ;    ; ;;  ;;  ;    ;    ;      ;  ;  
  ;   ;;;;;;;   ;;      ;;              ;;;;   ;;;;   ;    ;     ;;;  ;;  ;; 
  ;                                                                          
  ;                                                                          
  ;                                                                          

  ; Blocks without Break, RetStat, etc
  [Elf hole
       ; Statements
       (do Elf end)
       (if Elf then s else s end)
       (local Name ... = v ... Elf e ... in s end)
       ;(local Name ... = renv ... in Elf end)
       (Elf (renv ...) LocalBody)
       (evar ... (Elf \[ e \]) var ... = e ...)
       (evar ... (v \[ Elf \]) var ... = e ...)
       (evar ... = v ... Elf e ...)
       (return v ... Elf e ...)
       (Elf score_1 score_2 ...)   

       ; Function call, method call, built-in services
       ($statFunCall Elf (e ...))
       ($statFunCall v (v ... Elf e ...))
       ($statFunCall Elf : Name (e ...))
       ($builtIn builtinserv (v ... Elf e ...))
       (Elf (e ...))
       (v (v ... Elf e ...))
       (Elf : Name (e ...))

       ; Expressions
       (\( Elf \))
       (Elf binop e)
       (v strictbinop Elf)
       (unop Elf)
       (< v ... Elf e ... >)
       (\{ efield ... (\[ Elf \] = e) field ... \})
       (\{ efield ... (\[ v \] = Elf) field ... \})
       (\{ efield ... Elf field ... \})
       (Elf \[ e \])
       (v \[ Elf \])]

  
  ; Labelled-block, no protected mode
  [Enp Elf
      ; To avoid repeating every production of Elf
      (in-hole Elf (Enp Break))
      (in-hole Elf (Enp (renv ...) RetStat))
      (in-hole Elf (Enp (renv ...) RetExp))
      ]


  ; All possible evaluation contexts
  [E Elf
     (in-hole Elf (E Break))
     (in-hole Elf (E (renv ...) RetStat))
     (in-hole Elf (E (renv ...) RetExp))
     (in-hole Elf (E ProtectedMode))
     (in-hole Elf (E ProtectedMode v))
     ]
  
  ; List of expressions where a tuple is truncated
  [Etel (v ... hole e_1 e_2 ...)]
  
  ; Immediate evaluation contexts where a tuple is truncated
  [Et (if hole then s else s_2 end)
      (local Name ... = v ... hole e_1 e_2 ... in s end)
      (evar ... (hole \[ e_1 \]) var ... = e ...)
      (evar ... (e_1 \[ hole \]) var ... = e ...)
      (evar ... = v ... hole e_1 e_2 ...)
      (return v ... hole e_1 e_2 ...)
      (hole (e ...))
      (v Etel)
      ($statFunCall hole (e ...))
      ($statFunCall v Etel)
      ($builtIn builtinserv Etel)
      (hole : Name (e ...))
      ($statFunCall hole : Name (e ...))
      (hole binop e)
      (v strictbinop hole)
      (unop hole)
      (< v ... hole e_1 e_2 ... >)
      (\{ efield ... hole field_1 field_2 ... \})
      (\{ efield ... (\[ hole \] = e) field ... \})
      (\{ efield ... (\[ e \] = hole) field ... \})
      (hole \[ e \])
      (v \[ hole \])
      ; Introduces shift/reduce and reduce/reduce conflicts
      ; (\( hole \))
      ]
  
  ; List of expressions where a tuple is unwrapped
  [Euel (v ... hole)]
  ; Immediate evaluation contexts where a tuple is unwrapped
  [Eu (local Name ... = v ... hole in s end)
      (return v ... hole)
      (evar ... = v ... hole)
      (v (v ... hole))
      ($statFunCall v (v ... hole))
      ($builtIn builtinserv Euel)
      (< v ... hole >)
      (\{ efield ... hole \})]
  ;                  
  ;                  
  ;                  
  ;                  
  ;                  
  ;    ;;;;;    ;;;  
  ;   ;;  ;;   ;   ; 
  ;   ;    ;  ;      
  ;   ;    ;  ;      
  ;   ;    ;  ;      
  ;   ;;  ;;   ;   ; 
  ;    ;;; ;    ;;;  
  ;        ;         
  ;    ;   ;         
  ;     ;;;
  
  [cte tid
       cid]
  
  [C hole
     ; Statements
     (do C end)
     (if C then s else s end)
     (if e then C else s end)
     (if e then s else C end)
     (local Name ... = e ... C e ... in s end)
     (local Name ... = e ... in C end)
     (e ... C e ... = e ...)
     (e ... = e ... C e ...)
     (return e ... C e ...)
     (while C do s end)
     (while e do C end)
     ; Function call, method call, built-in services
     ($statFunCall C (e ...))
     ($statFunCall e (e ... C e ...))
     ($statFunCall C : Name (e ...))
     ($statFunCall e : Name (e ... C e ...))
     (C score ...)
     (s score_1 ... C score_2 ...)
     
     ; Run-time
     (C (renv ...) RetStat)
     (s (renv_1 ... (rEnv C) renv_2 ...) RetStat)
     (C (renv ...) RetExp)
     (s (renv_1 ... (rEnv C) renv_2 ...) RetExp)
     (C (renv ...) LocalBody)
     (s (renv_1 ... (rEnv C) renv_2 ...) LocalBody)
     (C Break)
     (C ProtectedMode)
     (C ProtectedMode v)
     (e ProtectedMode C)

     ; Added each production, instead of a single (C statlabel) and
     ; (C explabel), to avoid redundancy errors
     (((C \[ v \]) = v) WrongKey)
     (((tid \[ C \]) = v) WrongKey)
     (((tid \[ v \]) = C) WrongKey)
     (((C \[ v \]) = v) NonTable)
     (((v \[ C \]) = v) NonTable)
     (((v \[ v \]) = C) NonTable)
     (($statFunCall C (v ...)) WrongFunCall)
     (($statFunCall v (v ... C v ...)) WrongFunCall)
     ((C (v ...)) WrongFunCall)
     ((v (v ... C v ...)) WrongFunCall)

     ; (C explabel)
     ((C \[ v \]) NonTable)
     ((v \[ C \]) NonTable)
     ((C \[ v \]) WrongKey)
     ((tid \[ C \]) WrongKey)
     ((C arithop v) ArithWrongOps)
     ((v arithop C) ArithWrongOps)
     ((C .. v) StrConcatWrongOps)
     ((v .. C) StrConcatWrongOps)
     ((C < v) OrdCompWrongOps)
     ((v < C) OrdCompWrongOps)
     ((C <= v) OrdCompWrongOps)
     ((v <= C) OrdCompWrongOps)
     ((- C) NegWrongOp)
     ((\# C) StrLenWrongOp)
     ((C == v) EqFail)
     ((v == C) EqFail)
     
     (< e ... C e ... >)
     ($err C)
     ($iter C do s end)
     ($iter e do C end)
  
     ; Function call, method call, built-in services
     (C (e ...))
     (e (e ... C e ...))
     ($builtIn builtinserv (e ... C e ...))
     (C : Name (e ...))
     (e : Name (e ... C e ...))
  
     ; Expressions
     (\( C \))
     (C binop e)
     (e binop C)
     (unop C)
     (\{ field ... (\[ C \] = e) field ... \})
     (\{ field ... (\[ e \] = C) field ... \})
     (\{ field ... C field ... \})
     (C \[ e \])
     (e \[ C \])
     (function Name_1 (Name_2 ...) C end)
     (function Name_1 (Name_2 ... <<<) C end)
     ]  
  )
(provide ext-lang)


;                             
;                             
;                             
;      ;                      
;     ; ;                     
;     ; ;                     
;     ; ;    ;     ;  ;;   ;; 
;     ; ;    ;     ;   ;   ;  
;    ;   ;   ;     ;    ; ;   
;    ;   ;   ;     ;     ;    
;    ;;;;;   ;     ;     ;    
;    ;   ;   ;     ;    ; ;   
;   ;     ;  ;;   ;;   ;   ;  
;   ;     ;   ;;;; ;  ;;   ;; 
;                             
;                             
;                             
;                             
                               
(define is_s?
  (redex-match? ext-lang
                s))

(define is_e?
  (redex-match? ext-lang
                e))

(define (is_term? t)
  (or (is_s? t)

      (is_e? t)))


; values
(define is_v?
  (redex-match? ext-lang
                v))

(define is_number?
  (redex-match? ext-lang
                Number))

(define is_string?
  (redex-match? ext-lang
                String))

(define is_nil?
  (redex-match? ext-lang
                nil))

(define is_false?
  (redex-match? ext-lang
                false))

(define is_true?
  (redex-match? ext-lang
                true))

(define (is_tid? t)
  (redex-match? ext-lang
                tid
                t))

(define is_cid?
  (redex-match? ext-lang
                cid))

(define is_fdef?
  (redex-match? ext-lang
                functiondef))


; operators
(define is_strconcat?
  (redex-match? ext-lang
                ..))

(define is_arithop?
  (redex-match? ext-lang
                arithop))

; exps
(define is_r?
  (redex-match? ext-lang
                r))

; statements
(define is_skip?
  (redex-match? ext-lang
                \;))

(define is_break?
  (redex-match? ext-lang
                break))

; state
(define is_intreptable?
  (redex-match? ext-lang
                intreptable))

(define (is_cte? t)
  (or (is_tid? t)

      (is_cid? t)))

(define is_theta?
  (redex-match? ext-lang
                    θ))

(define is_conf?
  (redex-match? ext-lang
                (σ : θ : s)))


(provide is_s? is_e? is_term?
         ;values
         is_v?
         is_number?
         is_string?
         is_tid?
         is_cid?
         is_fdef?
         is_nil?
         is_false?
         is_true?

         ; ops
         is_strconcat?
         is_arithop?

         ; stats
         is_skip?
         is_break?
         
         is_r?
         is_intreptable? 
         is_cte?
         is_theta?
         is_conf? )