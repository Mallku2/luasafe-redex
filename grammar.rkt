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
         (do s end)
         (if e then s else s end)
         (while e do s end)
         (local Name_1 Name_2 ... = e_1 e_2 ... in s end)
         ]

  ; Lua's block of code: it helps to avoid an ambiguity in the grammar, between
  ; funcalls and concat. of stats
  [s score
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
     (e binop e)
     (unop e)
     functiondef
     ]
  
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
  [functiondef (function Name parameters s end)]

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

  ; terms: to specify meta-functions and relations that work on both, s and e
  [aterm e s]

  [Ce hole
      ; Function call, method call, built-in services
      (Ce (e ...))
      (e (e ... Ce e ...))
      ($builtIn builtinserv (e ... Ce e ...))
      (Ce : Name (e ...))
      (e : Name (e ... Ce e ...))
  
      ; Expressions
      (\( Ce \))
      (Ce binop e)
      (e binop Ce)
      (unop Ce)
      (\{ field ... (\[ Ce \] = e) field ... \})
      (\{ field ... (\[ e \] = Ce) field ... \})
      (\{ field ... Ce field ... \})
      (Ce \[ e \])
      (e \[ Ce \])
      ; function def.
      (function Name_1 (Name_2 ...) C end)
      (function Name_1 (Name_2 ... <<<) C end)]

  [Ccore hole
         Ce
         ; Statements
         (do C end)
         (if Ce then s else s end)
         (if e then C else s end)
         (if e then s else C end)
         (local Name ... = e ... Ce e ... in s end)
         (local Name ... = e ... in C end)
         (e ... Ce e ... = e ...)
         (e ... = e ... Ce e ...)
         (return e ... Ce e ...)
         (while Ce do s end)
         (while e do C end)
         ; Function call, method call, built-in services
         ($statFunCall Ce (e ...))
         ($statFunCall e (e ... Ce e ...))
         ($statFunCall Ce : Name (e ...))
         ($statFunCall e : Name (e ... Ce e ...))]

  [C hole
     Ccore
     (Ccore score_1 score_2 ...)
     (score_1 ... score_2 Ccore score_3 ...)]

  ; To express the concepto of "absence of while loops" in the context of a
  ; given point
  [Cenw hole
      ; Function call, method call, built-in services
      (Cenw (e ...))
      (e (e ... Cenw e ...))
      ($builtIn builtinserv (e ... Cenw e ...))
      (Cenw : Name (e ...))
      (e : Name (e ... Cenw e ...))
  
      ; Expressions
      (\( Cenw \))
      (Cenw binop e)
      (e binop Cenw)
      (unop Cenw)
      (\{ field ... (\[ Cenw \] = e) field ... \})
      (\{ field ... (\[ e \] = Cenw) field ... \})
      (\{ field ... Cenw field ... \})
      (Cenw \[ e \])
      (e \[ Cenw \])
      ; function def.
      (function Name_1 (Name_2 ...) Cnw end)
      (function Name_1 (Name_2 ... <<<) Cnw end)]

  [Ccorenw hole
           Ce
           ; Statements
           (do Cnw end)
           (if Cenw then s else s end)
           (if e then Cnw else s end)
           (if e then s else Cnw end)
           (local Name ... = e ... Cenw e ... in s end)
           (local Name ... = e ... in Cnw end)
           (e ... Cenw e ... = e ...)
           (e ... = e ... Cenw e ...)
           (return e ... Cenw e ...)
           ; Function call, method call, built-in services
           ($statFunCall Cenw (e ...))
           ($statFunCall e (e ... Cenw e ...))
           ($statFunCall Cenwe : Name (e ...))
           ($statFunCall e : Name (e ... CCenw e ...))]
  
  [Cnw hole
       Ccorenw
       (Ccorenw score_1 score_2 ...)
       (score_1 ... score_2 Ccorenw score_3 ...)]

  )
(provide core-lang)


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
  (redex-match? core-lang
                s))

(define is_e?
  (redex-match? core-lang
                e))

(define (is_term? t)
  (or (is_s? t)

      (is_e? t)))


; values
(define is_v?
  (redex-match? core-lang
                v))

(define is_number?
  (redex-match? core-lang
                Number))

(define is_string?
  (redex-match? core-lang
                String))

(define is_nil?
  (redex-match? core-lang
                nil))

(define is_false?
  (redex-match? core-lang
                false))

(define is_true?
  (redex-match? core-lang
                true))

(define is_fdef?
  (redex-match? core-lang
                functiondef))


; operators
(define is_strconcat?
  (redex-match? core-lang
                ..))

(define is_arithop?
  (redex-match? core-lang
                arithop))

; exps
(define is_r?
  (redex-match? core-lang
                r))

; statements
(define is_skip?
  (redex-match? core-lang
                \;))

(define is_break?
  (redex-match? core-lang
                break))


(provide is_s? is_e? is_term?
         ;values
         is_v?
         is_number?
         is_string?
         is_fdef?
         is_nil?
         is_false?
         is_true?

         ; ops
         is_strconcat?
         is_arithop?

         ; stats
         is_skip?
         is_break?)