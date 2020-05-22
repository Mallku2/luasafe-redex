#lang racket

(require redex
         "../type_inference.rkt"
         "../typing.rkt")

(define (type-inf-test-suite)
  
  ;                                                                                            
  ;                                                                                            
  ;                                                                                            
  ;                                                                                            
  ;                                                                                            
  ;                                                                                            
  ;     ;;;      ;;;    ; ;;;;    ;;;;;                       ;;; ;    ;;;    ; ;;;;           
  ;    ;   ;    ;   ;   ;;   ;;  ;     ;                     ;   ;;   ;   ;   ;;   ;;          
  ;   ;        ;     ;  ;     ;  ;                          ;     ;  ;     ;  ;     ;          
  ;   ;        ;     ;  ;     ;  ;;;;                       ;     ;  ;     ;  ;     ;          
  ;   ;        ;     ;  ;     ;      ;;;                    ;     ;  ;;;;;;;  ;     ;          
  ;   ;        ;     ;  ;     ;        ;                    ;     ;  ;        ;     ;          
  ;    ;   ;    ;   ;   ;     ;  ;     ;     ;;              ;   ;;   ;    ;  ;     ;     ;;   
  ;     ;;;      ;;;    ;     ;   ;;;;;      ;;               ;;; ;    ;;;;   ;     ;     ;;   
  ;                                                               ;                            
  ;                                                          ;   ;;                            
  ;                                                           ;;;;                             
  ;                                                                                            

  ;                                      
  ;                                      
  ;                                      
  ;                                      
  ;                                      
  ;                                      
  ;     ;;;    ;;   ;;  ; ;;;     ;;;;;  
  ;    ;   ;    ;   ;   ;;   ;   ;     ; 
  ;   ;     ;    ; ;    ;     ;  ;       
  ;   ;     ;     ;     ;     ;  ;;;;    
  ;   ;;;;;;;     ;     ;     ;      ;;; 
  ;   ;          ; ;    ;     ;        ; 
  ;    ;    ;   ;   ;   ;;   ;   ;     ; 
  ;     ;;;;   ;;   ;;  ; ;;;     ;;;;;  
  ;                     ;                
  ;                     ;                
  ;                     ;                
  ;                                      
  ; primitive types
  (test-equal
   (judgment-holds (cons_gen
                    ·
                    1
                    1
                    ·
                    ((1 <: (1 : num)))))
   #t)

  (test-equal
   (judgment-holds (cons_gen
                    ·
                    true
                    true
                    ·
                    ((true <: (true : bool)))))
   #t)

  (test-equal
   (judgment-holds (cons_gen
                    ·
                    nil
                    nil
                    ·
                    ((nil <: (nil : nil)))))
   #t)

  (test-equal
   (judgment-holds (cons_gen
                    ·
                    "asd"
                    "asd"
                    ·
                    (("asd" <: ("asd" : str)))))
   #t)

  (test-equal
   (judgment-holds (cons_gen
                    (x : 1 ·)
                    x
                    (x 1 typevar)
                    (x : 1 ·)
                    ()))
   #t)
  
  ; functiondef
  (test-equal
   (judgment-holds (cons_gen
                    ·
                    (function x (y) (return 1) end)
                    (function x ((y 1 typevar)) (return 1) end)
                    (y : 1 ·)
                    ((1 <: (1 : num))
                     ; the type of the expression returned by "return" should be
                     ; allowed wherever the return type is accepted
                     (1 <: ((function x ((y 1 typevar)) (return 1) end)
                            returntypevar))
                     ; from the paper: the type of the function is derived from
                     ; the type of the parameter and returned value of the
                     ; function
                     ((function x ((y 1 typevar)) (return 1) end)
                      <:
                      (((function x ((y 1 typevar)) (return 1) end)
                        paramtypevar)
                       ->
                       ((function x ((y 1 typevar)) (return 1) end)
                        returntypevar)))

                     (((function x ((y 1 typevar)) (return 1) end)
                       paramtypevar) <: ($tup (y 1 typevar)))
                     )))
   #t)

  (test-equal
   (judgment-holds (cons_gen
                    ·
                    (function x () (return 1) end)
                    (function x () (return 1) end)
                    ·
                    ((1 <: (1 : num))
                     ; the type of the expression returned by "return" should be
                     ; allowed wherever the return type is accepted
                     (1 <: ((function x () (return 1) end)
                            returntypevar))
                     ; from the paper: the type of the function is derived from
                     ; the type of the parameter and returned value of the
                     ; function
                     ((function x () (return 1) end)
                      <:
                      (((function x () (return 1) end)
                        paramtypevar)
                       ->
                       ((function x () (return 1) end)
                        returntypevar)))

                     (((function x () (return 1) end)
                       paramtypevar) <: ($tup))
                     )))
   #t)

  (test-equal
   (judgment-holds (cons_gen
                    ·
                    (function x (y) (return true) end)
                    (function x ((y 1 typevar)) (return true) end)
                    (y : 1 ·)
                    ((true <: (true : bool))
                     ; the type of the expression returned by "return" should be
                     ; allowed wherever the return type is accepted
                     (true <: ((function x ((y 1 typevar)) (return true) end)
                               returntypevar))
                     ; from the paper: the type of the function is derived from
                     ; the type of the parameter and returned value of the
                     ; function
                     ((function x ((y 1 typevar)) (return true) end)
                      <:
                      (((function x ((y 1 typevar)) (return true) end)
                        paramtypevar)
                       ->
                       ((function x ((y 1 typevar)) (return true) end)
                        returntypevar)))
                     (((function x ((y 1 typevar)) (return true) end)
                       paramtypevar) <: ($tup (y 1 typevar)))
                     )))
   #t)

  (test-equal
   (judgment-holds (cons_gen
                    ·
                    (function x (y)
                              (return (function x (y) (return true) end)) end)
                    (function x ((y 1 typevar))
                              (return (function x ((y 2 typevar))
                                                (return true) end)) end)
                    (y : 2 ·)
                    ((true <: (true : bool))
                     (true <: ((function x ((y 2 typevar))
                                         (return true) end) returntypevar))
                     ((function x ((y 2 typevar)) (return true) end)
                      <:
                      (((function x ((y 2 typevar)) (return true) end)
                        paramtypevar)
                       -> ((function x ((y 2 typevar)) (return true) end)
                           returntypevar)))
                     (((function x ((y 2 typevar)) (return true) end)
                       paramtypevar) <: ($tup (y 2 typevar)))
                     ((function x ((y 2 typevar)) (return true) end)
                      <:
                      ((function x ((y 1 typevar))
                                 (return (function x ((y 2 typevar))
                                                   (return true) end)) end)
                       returntypevar))

                     ((function x ((y 1 typevar))
                                (return (function x ((y 2 typevar))
                                                  (return true) end)) end)
                      <:
                      (((function x ((y 1 typevar))
                                  (return (function x ((y 2 typevar))
                                                    (return true) end)) end)
                        paramtypevar)
                       ->
                       ((function x ((y 1 typevar))
                                  (return (function x ((y 2 typevar))
                                                    (return true) end)) end)
                        returntypevar)))
                     (((function x ((y 1 typevar))
                                 (return (function x ((y 2 typevar))
                                                   (return true) end)) end)
                       paramtypevar) <: ($tup (y 1 typevar)))
                     )))
   #t)

  ;table field
  (test-equal
   (judgment-holds (cons_gen
                    (x : 1 ·)
                    (x \[ 2 \])
                    ((x 2 typevar) \[ 2 \])
                    (x : 2 ·)
                    ((2 <: (2 : num))
                     ((x 2 typevar) <: (\[ 2 \] : ((x 2 typevar) \[ 2 \])))
                     ((x 2 typevar) <: 2 (x 1 typevar)))
                    ))
   #t)

  ; funcall
  (test-equal
   (judgment-holds (cons_gen
                    ·
                    ((\( 1 \)) (2))
                    ((\( 1 \)) (2))
                    ·
                    ((1 <: (1 : num))
                     ((\( 1 \)) <: 1)
                     (1 <: (\( 1 \)))
                     (2 <: (2 : num))
                     (($tup 2) <: ((\( 1 \)) paramtypevar))
                     (((\( 1 \)) returntypevar) <: ((\( 1 \)) (2)))
                     ((\( 1 \))
                      <: (((\( 1 \)) paramtypevar)
                          ->
                          ((\( 1 \)) returntypevar))))))
   #t)

  ; parenthesized exp.
  (test-equal
   (judgment-holds (cons_gen
                    ·
                    (\( 2 \))
                    (\( 2 \))
                    ·
                    ((2 <: (2 : num))
                     ((\( 2 \)) <: 2)
                     (2 <: (\( 2 \))))))
   #t)

  ; table constructor
  (test-equal
   (judgment-holds (cons_gen
                    ·
                    (\{ 2 \})
                    (\{ (\[ ($dummyt 1 typevar) \] = 2) \})
                    ·
                    ((2 <: (2 : num))
                     (($dummyt 1 typevar) <: num)
                     ((\{ (\[ ($dummyt 1 typevar) \] = 2) \})
                      <:
                      (\[ ($dummyt 1 typevar) \] : 2)))))
   #t)

  (test-equal
   (judgment-holds (cons_gen
                    ·
                    (\{ 2 (\[ true \] = "asd") \})
                    (\{ (\[ ($dummyt 1 typevar) \] = 2) (\[ true \] = "asd") \})
                    ·
                    ((2 <: (2 : num))
                     (($dummyt 1 typevar) <: num)
                     (true <: (true : bool))
                     ("asd" <: ("asd" : str))
                     ((\{ (\[ ($dummyt 1 typevar) \] = 2)
                          (\[ true \] = "asd") \})
                      <: (\[ ($dummyt 1 typevar) \] : 2))
                     ((\{ (\[ ($dummyt 1 typevar) \] = 2)
                          (\[ true \] = "asd") \})
                      <:
                      (\[ true \] : "asd")))))
   #t)

  ; binops
  (test-equal
   (judgment-holds (cons_gen
                    ·
                    (1 + 2)
                    (1 + 2)
                    ·
                    ((1 <: (1 : num))
                     (2 <: (2 : num))
                     (1 <: num)
                     (2 <: num)
                     ((1 + 2) <: num))))
   #t)

  (test-equal
   (judgment-holds (cons_gen
                    ·
                    (1 % 2)
                    (1 % 2)
                    ·
                    ((1 <: (1 : num))
                     (2 <: (2 : num))
                     (1 <: num)
                     (2 <: num)
                     ((1 % 2) <: num))))
   #t)

  ; relops
  (test-equal
   (judgment-holds (cons_gen
                    ·
                    (1 == 2)
                    (1 == 2)
                    ·
                    ((1 <: (1 : num))
                     (2 <: (2 : num))
                     (1 <: 2)
                     (2 <: 1)
                     ((1 == 2) <: bool))))
   #t)

  
  (test-equal
   (judgment-holds (cons_gen
                    ·
                    (1 < 2)
                    (1 < 2)
                    ·
                    ((1 <: (1 : num))
                     (2 <: (2 : num))
                     (1 <: 2)
                     (2 <: 1)
                     ((1 < 2) <: bool))))
   #t)

  ; str concat
  (test-equal
   (judgment-holds (cons_gen
                    ·
                    ("a" .. "b")
                    ("a" .. "b")
                    ·
                    (("a" <: ("a" : str))
                     ("b" <: ("b" : str))
                     ("a" <: str)
                     ("b" <: str)
                     (("a" .. "b") <: str))))
   #t)

  ; logical op.
  (test-equal
   (judgment-holds (cons_gen
                    ·
                    ("a" and "b")
                    ("a" and "b")
                    ·
                    (("a" <: ("a" : str))
                     ("b" <: ("b" : str)))))
   #t)

  (test-equal
   (judgment-holds (cons_gen
                    ·
                    ("a" or "b")
                    ("a" or "b")
                    ·
                    (("a" <: ("a" : str))
                     ("b" <: ("b" : str)))))
   #t)

  ; unops
  (test-equal
   (judgment-holds (cons_gen
                    ·
                    (- 1)
                    (- 1)
                    ·
                    ((1 <: (1 : num))
                     (1 <: num)
                     ((- 1) <: num))))
   #t)

  (test-equal
   (judgment-holds (cons_gen
                    ·
                    (not 1)
                    (not 1)
                    ·
                    ((1 <: (1 : num))
                     ((not 1) <: bool))))
   #t)

  (test-equal
   (judgment-holds (cons_gen
                    ·
                    (\# "asd")
                    (\# "asd")
                    ·
                    (("asd" <: ("asd" : str))
                     ((\# "asd") <: num))))
   #t)
  ;                                               
  ;                                               
  ;              ;                 ;              
  ;              ;                 ;              
  ;    ;;;;;   ;;;;;;     ;;;;   ;;;;;;    ;;;;;  
  ;   ;     ;    ;       ;    ;    ;      ;     ; 
  ;   ;          ;            ;    ;      ;       
  ;   ;;;;       ;       ;;;;;;    ;      ;;;;    
  ;       ;;;    ;      ;;    ;    ;          ;;; 
  ;         ;    ;      ;     ;    ;            ; 
  ;   ;     ;    ;      ;    ;;    ;      ;     ; 
  ;    ;;;;;      ;;;    ;;;; ;     ;;;    ;;;;;  
  ;                                               
  ;                                               
  ;                                               
  ;                                               
  ; skip
  (test-equal
   (judgment-holds
    (cons_gen ·
              \;
              \;
              ·
              ()))
   #t)

  ; break
  (test-equal
   (judgment-holds
    (cons_gen ·
              break
              break
              ·
              ()))
   #t)

  ; return
  (test-equal
   (judgment-holds
    (cons_gen ·
              (return 1)
              (return 1)
              ·
              ((1 <: (1 : num))
               (1 <: ($actfunc returntypevar)))))
   #t)

  ; assignment
  (test-equal
   (judgment-holds
    (cons_gen (x : 1 ·)
              (x = 1)
              ((x 1 typevar) = 1)
              (x : 1 ·)
              ((1 <: (1 : num))
               (1 <: (x 1 typevar)))))
   #t)

  ; table field assignment
  (test-equal
   (judgment-holds
    (cons_gen (x : 1 ·)
              ((x \[ 1 \]) = 2)
              (((x 2 typevar) \[ 1 \]) = 2)
              (x : 2 ·)
              ((1 <: (1 : num))
               (2 <: (2 : num))
               ;               (((\{ (\[ 1 \] : ((x 1 typevar) \[ 1 \])) \})
               ;                 strong) <: (x 1 typevar))
               ((x 2 typevar) <: (|[| 1 |]| : ((x 2 typevar) |[| 1 |]|)))
               ((x 2 typevar) <: 1 (x 1 typevar))
               (2 <: ((x 2 typevar) \[ 1 \])))
              ))
   #t)

  ; do-end
  (test-equal
   (judgment-holds
    (cons_gen ·
              (do \; end)
              (do \; end)
              ·
              ()))
   #t)

  ; conditional
  (test-equal
   (judgment-holds
    (cons_gen (x : 1 ·)
              (if x then (x = 1) else (x = 2) end)
              (if (x 1 typevar)
                  then ((x 1 typevar) = 1)
                  else ((x 1 typevar) = 2) end)
              (x : 1 ·)
              ((1 <: (1 : num))
               (1 <: (x 1 typevar))
               (2 <: (2 : num))
               (2 <: (x 1 typevar)))))
   #t)

  ; while
  (test-equal
   (judgment-holds
    (cons_gen (x : 1 ·)
              (while x do (x = 1) end)
              (while (x 1 typevar) do ((x 1 typevar) = 1) end)
              (x : 1 ·)
              ((1 <: (1 : num))
               (1 <: (x 1 typevar)))))
   #t)
  
  ; local var def
  (test-equal
   (judgment-holds
    (cons_gen ·
              (local x = 123 in \; end)
              (local (x 1 typevar) = 123 in \; end)
              ·
              ((123 <: (123 : num)) (123 <: (x 1 typevar)))))
   #t)

  (test-equal
   (judgment-holds
    (cons_gen ·
              (local x = 123 in (x = true) end)
              (local (x 1 typevar) = 123 in ((x 1 typevar) = true) end)
              ·
              ((123 <: (123 : num))
               (true <: (true : bool))
               (true <: (x 1 typevar))
               (123 <: (x 1 typevar)))))
   #t)

  ; funcall
  (test-equal
   (judgment-holds (cons_gen
                    ·
                    ($statFunCall (\( 1 \)) (2))
                    ($statFunCall (|(| 1 |)|) (2))
                    ·
                    ((1 <: (1 : num))
                     ((|(| 1 |)|) <: 1)
                     (1 <: (|(| 1 |)|))
                     (2 <: (2 : num))
                     (($tup 2) <: ((|(| 1 |)|) paramtypevar))
                     (((|(| 1 |)|) returntypevar) <: ((|(| 1 |)|) (2)))
                     ((|(| 1 |)|)
                      <: (((|(| 1 |)|) paramtypevar)
                          -> ((|(| 1 |)|) returntypevar))))))
   #t)

  (test-equal
   (judgment-holds (cons_gen
                    ·
                    ($statFunCall (\( 1 \)) ())
                    ($statFunCall (|(| 1 |)|) ())
                    ·
                    ((1 <: (1 : num))
                     ((|(| 1 |)|) <: 1)
                     (1 <: (|(| 1 |)|))
                     (($tup) <: ((|(| 1 |)|) paramtypevar))
                     (((|(| 1 |)|) returntypevar) <: ((|(| 1 |)|) ()))
                     ((|(| 1 |)|)
                      <: (((|(| 1 |)|) paramtypevar)
                          -> ((|(| 1 |)|) returntypevar))))))
   #t)

  ; stat concat
  (test-equal
   (judgment-holds (cons_gen
                    (x : 1 ·)

                    (($statFunCall (\( 1 \)) (2))
                     (while x do (x = 1) end))
                    
                    (($statFunCall (|(| 1 |)|) (2))
                     (while (x 1 typevar) do ((x 1 typevar) = 1) end))

                    (x : 1 ·)
                   
                    ((1 <: (1 : num))
                     ((|(| 1 |)|) <: 1)
                     (1 <: (|(| 1 |)|))
                     (2 <: (2 : num))
                     (($tup 2) <: ((|(| 1 |)|) paramtypevar))
                     (((|(| 1 |)|) returntypevar) <: ((|(| 1 |)|) (2)))
                     ((|(| 1 |)|)
                      <: (((|(| 1 |)|) paramtypevar)
                          -> ((|(| 1 |)|) returntypevar)))
                     (1 <: (x 1 typevar)))))
   #t)
  
  ;                                                                 
  ;                                                                 
  ;                                                                 
  ;           ;;;;                                                  
  ;              ;                                                  
  ;              ;                                                  
  ;     ;;;      ;        ;;;     ;;;;;   ;     ;    ; ;;;    ;;;   
  ;    ;   ;     ;       ;   ;   ;     ;  ;     ;    ;;   ;  ;   ;  
  ;   ;          ;      ;     ;  ;        ;     ;    ;      ;     ; 
  ;   ;          ;      ;     ;  ;;;;     ;     ;    ;      ;     ; 
  ;   ;          ;      ;     ;      ;;;  ;     ;    ;      ;;;;;;; 
  ;   ;          ;      ;     ;        ;  ;     ;    ;      ;       
  ;    ;   ;     ;       ;   ;   ;     ;  ;;   ;;    ;       ;    ; 
  ;     ;;;       ;;;     ;;;     ;;;;;    ;;;; ;    ;        ;;;;  
  ;                                                                 
  ;                                                                 
  ;                                                                 
  ;                                                                 

  ; transitivity implied by subtyping
  (test-equal
   (term
    (combine_clos_steps (((x 1 typevar) <: 1)
                         (1 <: (1 : num)))))
   (term (((x 1 typevar) <: 1)
          (1 <: (1 : num))
          ((x 1 typevar) <: (1 : num)))))

  (test-equal
   (term
    (combine_clos_steps (((x 1 typevar) <: (\{ \}))
                         ((\{ \}) <: ((\{ \}) strong)))))
   (term (((x 1 typevar) <: (\{ \}))
          ((\{ \}) <: ((\{ \}) strong))
          ((x 1 typevar) <: ((\{ \}) strong)))))

  (test-equal
   (term
    (combine_clos_steps (((x 1 typevar) <: (y 1 typevar))
                         ((y 1 typevar) <: ((z 1 typevar) -> (z 1 typevar))))))
   (term (((x 1 typevar) <: (y 1 typevar))
          ((y 1 typevar) <: ((z 1 typevar) -> (z 1 typevar)))
          ((x 1 typevar) <: ((z 1 typevar) -> (z 1 typevar))))))

  ; closeCong
  (test-equal
   (term
    (combine_clos_steps (((x 1 typevar) <: ((y 1 typevar) -> (z 1 typevar)))
                         ((x 1 typevar) <: ((u 1 typevar) -> (v 1 typevar))))))
   (term (((x 1 typevar) <: ((y 1 typevar) -> (z 1 typevar)))
          ((x 1 typevar) <: ((u 1 typevar) -> (v 1 typevar)))
          ((u 1 typevar) <: (y 1 typevar))
          ((v 1 typevar) <: (z 1 typevar))
          ((y 1 typevar) <: (u 1 typevar))
          ((z 1 typevar) <: (v 1 typevar))
          )))

  ; closeTable Cong
  ;  (test-equal
  ;   (term
  ;    (combine_clos_steps (((x 1 typevar)
  ;                          <: ((\{ (\[ (y 1 typevar) \] : (z 1 typevar)) \})
  ;                              strong))
  ;                         ((x 1 typevar)
  ;                          <: ((\{ (\[ (y 1 typevar) \] : (u 1 typevar)) \})
  ;                              strong)))))
  ;   (term (((x 1 typevar)
  ;           <: ((\{ (\[ (y 1 typevar) \] : (z 1 typevar)) \}) strong))
  ;          ((x 1 typevar)
  ;           <: ((\{ (\[ (y 1 typevar) \] : (u 1 typevar)) \}) strong))
  ;          ((u 1 typevar) <: (z 1 typevar))
  ;          ((z 1 typevar) <: (u 1 typevar))
  ;          )))

  (test-equal
   (term
    (combine_clos_steps (((x 1 typevar)
                          <: (\[ (y 1 typevar) \] : (z 1 typevar)))
                         ((x 1 typevar)
                          <: (\[ (y 1 typevar) \] : (u 1 typevar))))))
   (term (((x 1 typevar)
           <: (\[ (y 1 typevar) \] : (z 1 typevar)))
          ((x 1 typevar)
           <: (\[ (y 1 typevar) \] : (u 1 typevar)))
          ((u 1 typevar) <: (z 1 typevar))
          ((z 1 typevar) <: (u 1 typevar))
          )))

  ; closeBalance
  ; primitive types
  (test-equal
   (term
    (combine_clos_steps (((x 1 typevar) <: num)
                         ((x 1 typevar) <: (y 1 typevar)))))
   (term (((x 1 typevar) <: num)
          ((x 1 typevar) <: (y 1 typevar))
          ((y 1 typevar) <: num ∨ num <: (y 1 typevar)))))

  (test-equal
   (term
    (combine_clos_steps (((x 1 typevar) <: str)
                         ((x 1 typevar) <: (y 1 typevar)))))
   (term (((x 1 typevar) <: str)
          ((x 1 typevar) <: (y 1 typevar))
          ((y 1 typevar) <: str ∨ str <: (y 1 typevar)))))

  (test-equal
   (term
    (combine_clos_steps (((x 1 typevar) <: bool)
                         ((x 1 typevar) <: (y 1 typevar)))))
   (term (((x 1 typevar) <: bool)
          ((x 1 typevar) <: (y 1 typevar))
          ((y 1 typevar) <: bool ∨ bool <: (y 1 typevar)))))

  (test-equal
   (term
    (combine_clos_steps (((x 1 typevar) <: ((z 1 typevar) -> (z 1 typevar)))
                         ((x 1 typevar) <: (y 1 typevar)))))
   (term (((x 1 typevar) <: ((z 1 typevar) -> (z 1 typevar)))
          ((x 1 typevar) <: (y 1 typevar))
          (((z 1 typevar) -> (z 1 typevar)) <: (y 1 typevar)))))

  (test-equal
   (term
    (combine_clos_steps (((x 1 typevar) <: ((\{ \}) strong))
                         ((x 1 typevar) <: (y 1 typevar)))))
   (term (((x 1 typevar) <: ((\{ \}) strong))
          ((x 1 typevar) <: (y 1 typevar))
          (((\{ \}) strong) <: (y 1 typevar)))))

  
  ;                                                                                   
  ;                                                                                   
  ;                                                                                   
  ;                    ;;;;     ;;;;                   ;;;                            
  ;                       ;        ;                  ;                               
  ;                       ;        ;                  ;                               
  ;  ;       ;   ;;;      ;        ;                ;;;;;;    ;;;      ; ;;;  ;;;;;;  
  ;  ;       ;  ;   ;     ;        ;                  ;      ;   ;     ;;   ; ;  ;  ; 
  ;   ;  ;  ;  ;     ;    ;        ;                  ;     ;     ;    ;      ;  ;  ; 
  ;   ;  ;  ;  ;     ;    ;        ;        ;;;;      ;     ;     ;    ;      ;  ;  ; 
  ;   ; ; ; ;  ;;;;;;;    ;        ;                  ;     ;     ;    ;      ;  ;  ; 
  ;   ; ; ; ;  ;          ;        ;                  ;     ;     ;    ;      ;  ;  ; 
  ;    ;   ;    ;    ;    ;        ;                  ;      ;   ;     ;      ;  ;  ; 
  ;    ;   ;     ;;;;      ;;;      ;;;               ;       ;;;      ;      ;  ;  ; 
  ;                                                                                   
  ;                                                                                   
  ;                                                                                   
  ;                                                                                   


  ; types are not mixed
  (test-equal
   (judgment-holds (well_form_cons_set
                    ((123 <: num)
                     (123 <: bool))))
   #f)

  (test-equal
   (judgment-holds (well_form_cons_set
                    ((123 <: num)
                     (123 <: ((x 1 typevar) -> (x 1 typevar))))))
   #f)

  (test-equal
   (judgment-holds (well_form_cons_set
                    ((123 <: num)
                     ((y 1 typevar) <: ((x 1 typevar) -> (x 1 typevar))))))
   #t)

  (test-equal
   (judgment-holds (well_form_cons_set
                    (((y 1 typevar) <: num)
                     ((y 1 typevar) <: ((x 1 typevar) -> (x 1 typevar))))))
   #f)

  (test-equal
   (judgment-holds (well_form_cons_set
                    (((y 1 typevar) <: ((\{ \}) strong))
                     ((y 1 typevar) <: ((x 1 typevar) -> (x 1 typevar))))))
   #f)

  (test-equal
   (judgment-holds (well_form_cons_set
                    (((z 1 typevar) <: ((\{ \}) strong))
                     ((y 1 typevar) <: ((x 1 typevar) -> (x 1 typevar))))))
   #t)

  (test-equal
   (judgment-holds (well_form_cons_set
                    (((z 1 typevar) <: ((\{ (\[ 1 \] : 1) \}) strong))
                     ((y 1 typevar) <: ((x 1 typevar) -> (x 1 typevar))))))
   #t)

  (test-equal
   (judgment-holds (well_form_cons_set
                    (((y 1 typevar) <: ((\{ (\[ 1 \] : 1) \}) strong))
                     ((y 1 typevar) <: ((x 1 typevar) -> (x 1 typevar))))))
   #f)

  (test-equal
   (judgment-holds (well_form_cons_set
                    (((y 1 typevar) <: ((\{ (\[ 1 \] : 1) \}) strong)))))
   #t)
  
  ;                                                                 
  ;                                                                 
  ;                                                                 
  ;                    ;;;;                                         
  ;                       ;                                         
  ;                       ;                                         
  ;    ;;;;;     ;;;      ;                 ;;; ;    ;;;    ; ;;;;  
  ;   ;     ;   ;   ;     ;                ;   ;;   ;   ;   ;;   ;; 
  ;   ;        ;     ;    ;               ;     ;  ;     ;  ;     ; 
  ;   ;;;;     ;     ;    ;               ;     ;  ;     ;  ;     ; 
  ;       ;;;  ;     ;    ;               ;     ;  ;;;;;;;  ;     ; 
  ;         ;  ;     ;    ;               ;     ;  ;        ;     ; 
  ;   ;     ;   ;   ;     ;                ;   ;;   ;    ;  ;     ; 
  ;    ;;;;;     ;;;       ;;;              ;;; ;    ;;;;   ;     ; 
  ;                                             ;                   
  ;                                        ;   ;;                   
  ;                                         ;;;;                    
  ;                             ;;;;;;;;;                           
  ; type variable being solved
  (test-equal
   (judgment-holds
    (sol_gen () ((x 1 typevar) : (tsv 1) ·) (x 1 typevar) (tsv 1)
             ((x 1 typevar) : (tsv 1) ·)))
   #t)

  ; type var has a primitive type
  (test-equal
   (judgment-holds
    (sol_gen (((x 1 typevar) <: num)) · (x 1 typevar) num ·))
   #t)

  (test-equal
   (judgment-holds
    (sol_gen (((x 1 typevar) <: str)) · (x 1 typevar) str ·))
   #t)

  (test-equal
   (judgment-holds
    (sol_gen (((x 1 typevar) <: bool)) · (x 1 typevar) bool ·))
   #t)

  (test-equal
   (judgment-holds
    (sol_gen (((x 1 typevar) <: (nil : nil))) · (x 1 typevar) (nil : nil) ·))
   #t)

  ; function type
  (test-equal
   (judgment-holds
    (sol_gen (((x 1 typevar) <: ((y 1 typevar) -> (z 1 typevar)))
              ((y 1 typevar) <: num)
              ((z 1 typevar) <: bool)) · (x 1 typevar) (num -> bool)
                                       ((x 1 typevar) : (tsv 1) ·)))
   #t)

  (test-equal
   (judgment-holds
    (sol_gen (((x 1 typevar) <: ((y 1 typevar) -> (x 1 typevar)))
              ((y 1 typevar) <: num)) ·
                                      (x 1 typevar)
                                      (μ (tsv 1) (num -> (tsv 1)))
                                      ((x 1 typevar) : (tsv 1) ·)))
   #t)

  ; tables
  (test-equal
   (judgment-holds
    (sol_gen (((x 1 typevar) <: ((\{ \}) strong)))
             ·
             (x 1 typevar)
             ((\{ \}) strong)
             ·))
   #t)

  (test-equal
   (judgment-holds
    (sol_gen (((x 1 typevar) <: (\[ (y 1 typevar) \] : (y 1 typevar)))
              ((y 1 typevar) <: bool))
             ·
             (x 1 typevar)
             ((\{ (\[ bool \] : bool) \}) strong)
             ((x 1 typevar) : (tsv 1) ·)))
   #t)

  (test-equal
   (judgment-holds
    (sol_gen (((x 1 typevar) <: (\[ (y 1 typevar) \] : (y 1 typevar)))
              ((y 1 typevar) <: bool))
             ·
             (x 1 typevar)
             ((\{ (\[ bool \] : bool) \}) strong)
             ((x 1 typevar) : (tsv 1) ·)))
   #t)

  ; dyn
  (test-equal
   (judgment-holds
    (sol_gen (((x 1 typevar) <: (\[ (y 1 typevar) \] : (y 1 typevar)))
              ((y 1 typevar) <: bool))
             ·
             (x 1 typevar)
             ((\{ (\[ bool \] : bool) \}) strong)
             ((x 1 typevar) : (tsv 1) ·)))
   #t)
  ;                                                                                                  
  ;                                                                                                  
  ;                                   ;;;                                       ;                ;;  
  ;                                     ;                                                       ;    
  ;                                     ;               ;                                       ;    
  ;                                     ;               ;                                       ;    
  ;     ;;;    ;;;;   ;;;;;;; ;;;;;     ;      ;;;;   ;;;;;;   ;;;;           ;;;     ; ;;;   ;;;;;  
  ;    ;   ;  ;;  ;;  ;  ;  ; ;;  ;;    ;     ;;  ;;    ;     ;;  ;;            ;     ;;   ;    ;    
  ;   ;       ;    ;  ;  ;  ; ;    ;    ;     ;    ;    ;     ;    ;            ;     ;    ;    ;    
  ;   ;       ;    ;  ;  ;  ; ;    ;    ;     ;;;;;;    ;     ;;;;;;            ;     ;    ;    ;    
  ;   ;       ;    ;  ;  ;  ; ;    ;    ;     ;         ;     ;                 ;     ;    ;    ;    
  ;    ;   ;  ;;  ;;  ;  ;  ; ;;  ;;    ;     ;;   ;    ;     ;;   ;            ;     ;    ;    ;    
  ;     ;;;    ;;;;   ;  ;  ; ;;;;;      ;;;   ;;;;      ;;;   ;;;;           ;;;;;   ;    ;    ;    
  ;                           ;                                                                      
  ;                           ;                                                                      
  ;                           ;                                                                      
  ;


  
  ;                          
  ;                          
  ;                          
  ;                          
  ;                          
  ;                          
  ;    ;;;;   ;;  ;;  ;;;;;  
  ;   ;;  ;;   ;  ;   ;;  ;; 
  ;   ;    ;    ;;    ;    ; 
  ;   ;;;;;;    ;;    ;    ; 
  ;   ;         ;;    ;    ; 
  ;   ;;   ;   ;  ;   ;;  ;; 
  ;    ;;;;   ;;  ;;  ;;;;;  
  ;                   ;      
  ;                   ;      
  ;                   ;      
  ;
  ; values
  (test-equal
   (term (type_term nil))
   (term nil))

  (test-equal
   (term (type_term true))
   (term true))
  
  (test-equal
   (term (type_term 1))
   (term 1))

  (test-equal
   (term (type_term "asd"))
   (term "asd"))

  ; functiondef
  
  ; base types
  (test-equal
   (term (type_term (function x (y) (return 1) end)))
   (term ((1 : num) function x ((y : dyn)) (return 1) end))
   )
  
  (test-equal
   (term (type_term (function x (y) (return (y + 1)) end)))
   (term (num function x ((y : num)) (return (y + 1)) end))
   )

  (test-equal
   (term (type_term (function x (y) (return true) end)))
   (term ((true : bool) function x ((y : dyn)) (return true) end))
   )

  (test-equal
   (term (type_term (function x (y) (return ((1 == 1) == y)) end)))
   (term (bool function x ((y : bool)) (return ((1 == 1) == y)) end))
   )

  (test-equal
   (term (type_term (function x (y) (return "asd") end)))
   (term (("asd" : str) function x ((y : dyn)) (return "asd") end))
   )

  (test-equal
   (term (type_term (function x (y) (return ("asd" .. y)) end)))
   (term (str function x ((y : str)) (return ("asd" .. y)) end))
   )

  ; nil
  (test-equal
   (term (type_term (function x (y) (return nil) end)))
   (term ((nil : nil) function x ((y : dyn)) (return nil) end))
   )

  ; dyn
  (test-equal
   (term (type_term (function x (y) \; end)))
   (term (dyn function x ((y : dyn)) |;| end))
   )

  ; returning functions
  (test-equal
   (term (type_term (function x (y)
                              (return (function z (w) (return nil) end))
                              end)))
   (term ((($tup dyn) -> (nil : nil))
          function x ((y : dyn))
          (return ((nil : nil) function z ((w : dyn))
                               (return nil) end))
          end)))

  ; assigning parameters
  (test-equal
   (term (type_term (function y (z) (z = (z + 1)) end)))
   (term (dyn function y ((z : num)) (z = (z + 1)) end)))
  
  ; function call
  ; parameter <: according to subtyping
  (test-equal
   (term (type_term ((\( (function x (y) \; end) \)) (2))))
   (term ((\( (dyn function x ((y : (2 : num))) \; end) \)) (2))))

  (test-equal
   (term (type_term ((\( (function x (y) ((y |[| 1 |]|) = 2) end) \)) (2))))
   (term (-> "the term cannot be typed" <-)))

  (test-equal
   (term (type_term (x (2))))
   (term (x (2))))

  ; vars
  
  ; TODO: vararg?
  ;  (test-equal
  ;   (term (type_term <<<))
  ;   (term <<<))

  ; parenthesized exps.
  (test-equal
   (term (type_term (\( 1 \))))
   (term (\( 1 \))))

  ; tables
  (test-equal
   (term (type_term (\{ (\[ 1 \] = 2) \})))
   (term (\{ (\[ 1 \] = 2) \})))

  ; binops
  (test-equal
   (term (type_term (1 + 1)))
   (term (1 + 1)))

  (test-equal
   (term (type_term (- 1)))
   (term (- 1)))
  
  ;                                  
  ;                                  
  ;                                  
  ;                                  
  ;             ;               ;    
  ;             ;               ;    
  ;    ;;;;   ;;;;;;    ;;;   ;;;;;; 
  ;   ;    ;    ;      ;   ;    ;    
  ;   ;         ;          ;    ;    
  ;    ;;;;     ;      ;;;;;    ;    
  ;        ;    ;     ;    ;    ;    
  ;   ;    ;    ;     ;   ;;    ;    
  ;    ;;;;      ;;;   ;;; ;     ;;; 
  ;                                  
  ;                                  
  ;                                  
  ;
  (test-equal
   (term (type_term \;))
   (term \;))

  (test-equal
   (term (type_term break))
   (term break))

  (test-equal
   (term (type_term (return 1)))
   (term (return 1)))

  (test-equal
   (term (type_term (do \; end)))
   (term (do \; end)))

  (test-equal
   (term (type_term (if 1 then \; else \; end)))
   (term (if 1 then \; else \; end)))

  (test-equal
   (term (type_term (while true do \; end)))
   (term (while true do \; end)))

  ; local var. defs.
  (test-equal
   (term (type_term (local x = 1 in \; end)))
   (term (local (x : (1 : num)) = 1 in \; end)))

  (test-equal
   (term (type_term (local x = (\( 1 \)) in \; end)))
   (term (local (x : (1 : num)) = (\( 1 \)) in \; end)))

  (test-equal
   (term (type_term (local x = "asd" in \; end)))
   (term (local (x :("asd" : str)) = "asd" in \; end)))

  (test-equal
   (term (type_term (local x = "__mode" in \; end)))
   (term (local (x : ("__mode" : str)) = "__mode" in \; end)))

  (test-equal
   (term (type_term (local x = "__gc" in \; end)))
   (term (local (x : ("__gc" : str)) = "__gc" in \; end)))

  (test-equal
   (term (type_term (local x = "k" in \; end)))
   (term (local (x : ("k" : str)) = "k" in \; end)))

  (test-equal
   (term (type_term (local x = "v" in \; end)))
   (term (local (x : ("v" : str)) = "v" in \; end)))

  (test-equal
   (term (type_term (local x = "kv" in \; end)))
   (term (local (x : ("kv" : str)) = "kv" in \; end)))

  (test-equal
   (term (type_term (local x = true in \; end)))
   (term (local (x : (true : bool)) = true in \; end)))

  
  (test-equal
   (term (type_term (local x = nil in \; end)))
   (term (local (x : (nil : nil)) = nil in \; end)))

  (test-equal
   (term (type_term (local x = (\{ (\[ "__mode" \] = "k") \}) in \; end)))
   (term (local
           (x : ((\{ (\[ ("__mode" : str) \] : ("k" : str)) \}) strong))
           =
           (\{ (\[ "__mode" \] = "k") \}) in \; end)))

  (test-equal
   (term (type_term (local x
                      =
                      (function y (z)
                                (return (\{ (\[ "__mode" \] = "k") \})) end)
                      in \; end)))
   (term (local
           (x : (($tup dyn) -> ((\{ (\[ ("__mode" : str) \] : ("k" : str)) \})
                                strong)))
           =
           (((|{| (|[| ("__mode" : str) |]| : ("k" : str)) |}|) strong)
            function y ((z : dyn))
            (return (|{| (|[| "__mode" |]| = "k") |}|)) end)
           in \; end)))

  ; local var with supertype
  (test-equal
   (term (type_term (local t = (|{| |}|) in ((t |[| 1.0 |]|) = (|{| |}|)) end)))
   (term (local (t : ((|{| |}|) strong)) = (|{| |}|) in
           ((t |[| 1.0 |]|) = (|{| |}|)) end))
   )

  ; testing evolution of tables
  (test-equal
   (term (type_term (local t = (|{| |}|) in
                      (((t |[| 1.0 |]|) = (|{| |}|))
                       (local x = (t |[| 1.0 |]|) in \; end)) end)))
   
   (term (local (t : ((|{| |}|) strong)) = (|{| |}|) in
           (((t |[| 1.0 |]|) = (|{| |}|))
            (local (x : ((|{| |}|) strong)) = (t |[| 1.0 |]|) in |;| end)) end)
         ))

  (test-equal
   (term (type_term (local t = (|{| |}|) in
                      (((t |[| 1.0 |]|) = (|{| |}|))
                       (local x = (t |[| 1.0 |]|) in
                         (((t |[| "asd" |]|) = true)
                          (local y = (t |[| "asd" |]|) in \; end)) end)) end)))
   (term (local
           (t : ((|{| |}|) strong))
           =
           (|{| |}|)
           in
           (((t |[| 1.0 |]|) = (|{| |}|))
            (local (x : ((|{| |}|) strong)) = (t |[| 1.0 |]|) in
              (((t |[| "asd" |]|) = true)
               (local (y : (true : bool)) = (t |[| "asd" |]|) in |;| end)) end))
           end)))
  ; fcalls
  (test-equal
   (term (type_term (local x
                      =
                      (function y (z) (return nil) end)
                      in (local y = (x (1)) in \; end) end)))
   (term (local
           (x : (($tup (1 : num)) -> (nil : nil)))
           =
           ((nil : nil)
            function
            y
            ((z : (1 : num)))
            (return nil)
            end)
           in
           (local (y : (nil : nil)) = (x (1)) in |;| end)
           end)))

  (test-equal
   (term (type_term (local x
                      =
                      (function y () (return nil) end)
                      in (local y = (x ()) in \; end) end)))
   (term (local
           (x : (($tup) -> (nil : nil)))
           =
           ((nil : nil) function y () (return nil) end)
           in
           (local (y : (nil : nil)) = (x ()) in |;| end)
           end)))

  ; cons clos:
  (test-equal
   (term (type_term 
          (local
            t1
            =
            (|{| (|[| 1.0 |]| = (|{| |}|)) |}|)
            in
            (if (not (t1 |[| 1.0 |]|)) then |;| else |;| end)
            end)))
   (term (local
           (t1 : ((|{| (|[| (1.0 : num) |]| : ((|{| |}|) strong)) |}|) strong))
           =
           (|{| (|[| 1.0 |]| = (|{| |}|)) |}|)
           in
           (if (not (t1 |[| 1.0 |]|)) then |;| else |;| end)
           end)))

  ; table evolution
  (test-equal
   (term (type_term
          (local cache =
            (|{| (|[| 1.0 |]| = (function $1 () (return 1.0) end))
                 (|[| 2.0 |]| = 3) |}|)
            in
            ($statFunCall (cache |[| 1.0 |]|) ())
            end)))
   (term (local
           (cache : ((|{| (|[| (2.0 : num) |]| : (3 : num))
                          (|[| (1.0 : num) |]| : (($tup) -> (1.0 : num))) |}|)
                     strong))
           =
           (|{| (|[| 1.0 |]| = ((1.0 : num) function $1 () (return 1.0) end))
                (|[| 2.0 |]| = 3) |}|)
           in
           ($statFunCall (cache |[| 1.0 |]|) ())
           end)))

  (test-equal
   (term (type_term
          (local cache
            =
            (|{| (|[| 1.0 |]| = (function $1 () (return 1.0) end))
                 (|[| 2.0 |]| = (function $2 () (return 2.0) end)) |}|)
            in
            (|;| ($statFunCall (cache |[| 1.0 |]|) ()))
            end)))
   (term (local
           (cache : ((|{| (|[| (2.0 : num) |]| : (($tup) -> (2.0 : num)))
                          (|[| (1.0 : num) |]| : (($tup) -> (1.0 : num))) |}|)
                     strong))
           =
           (|{| (|[| 1.0 |]| = ((1.0 : num) function $1 () (return 1.0) end))
                (|[| 2.0 |]| = ((2.0 : num) function $2 () (return 2.0) end))
                |}|)
           in
           (|;| ($statFunCall (cache |[| 1.0 |]|) ()))
           end)))

  
  ; correct management of environment γ
  (test-equal
   (term (type_term (local x = (function $1 (x) (return (x + 1.0)) end)
                      in \; end)))
   (term (local (x : (($tup num) -> num)) = (num function $1 ((x : num))
                                                 (return (x + 1.0)) end)
           in |;| end)))
  
  (test-results)
  )