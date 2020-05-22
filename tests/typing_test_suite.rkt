#lang racket

(require redex
         "../grammar.rkt"
         "../desugar/parser.rkt"
         "../typing.rkt"
         "../typing_lang_theory.rkt"
         "../reaching_defs.rkt")

(define (typing-test-suite)
  ;                          
  ;                          
  ;                          
  ;                          
  ;                          
  ;                          
  ;     ;;;   ;    ;  ;;  ;; 
  ;    ;   ;  ;    ;   ;  ;  
  ;        ;  ;    ;    ;;   
  ;    ;;;;;  ;    ;    ;;   
  ;   ;    ;  ;    ;    ;;   
  ;   ;   ;;  ;   ;;   ;  ;  
  ;    ;;; ;   ;;; ;  ;;  ;; 
  ;                          
  ;                          
  ;                          
  ;                          
  (test-equal
   (term (set (x : (strong (\{ \})) ·) x (wv (\{ \}))))
   (term (x : (wv (|{| |}|)) ·)))

  (test-equal
   (term (set (y : (wv (\{ \})) (x : (strong (\{ \})) ·)) x (wv (\{ \}))))
   (term (y : (wv (|{| |}|)) (x : (wv (|{| |}|)) ·))))

  (test-equal
   (term (set (y : (wv (\{ \})) (x : (strong (\{ \})) ·)) y (strong (\{ \}))))
   (term (y : (strong (|{| |}|)) (x : (strong (|{| |}|)) ·))))

  (test-equal
   (term (index (x : (strong (\{ \})) ·) x))
   (term (strong (\{ \}))))

  (test-equal
   (term (index (y : (wv (\{ \})) (x : (strong (\{ \})) ·)) x))
   (term (strong (\{ \}))))

  (test-equal
   (term (index (y : (wv (\{ \})) (x : (strong (\{ \})) ·)) y))
   (term (wv (\{ \}))))

  (test-equal
   (term (in-map · y))
   #f)

  (test-equal
   (term (in-map (x : (strong (\{ \})) ·) y))
   #f)

  (test-equal
   (term (in-map (x : (strong (\{ \})) ·) x))
   #t)

  ;                                               
  ;                                               
  ;                                               
  ;                                       ;       
  ;                                       ;       
  ;                                       ;       
  ;     ; ;;;    ;;;      ;;;;     ;;;    ; ;;;;  
  ;     ;;   ;  ;   ;    ;    ;   ;   ;   ;;   ;; 
  ;     ;      ;     ;        ;  ;        ;     ; 
  ;     ;      ;     ;   ;;;;;;  ;        ;     ; 
  ;     ;      ;;;;;;;  ;;    ;  ;        ;     ; 
  ;     ;      ;        ;     ;  ;        ;     ; 
  ;     ;       ;    ;  ;    ;;   ;   ;   ;     ; 
  ;     ;        ;;;;    ;;;; ;    ;;;    ;     ; 
  ;                                               
  ;                                               
  ;                                               
  ;
  ; build-tree
  (test-equal
   (term (build_tree ())) 
   (term empty))

  ; lineal
  (test-equal
   (term (build_tree ((x = y)))) 
   (term (((x = y)) empty)))

  (test-equal
   (term (build_tree ((x = y) (y = z)))) 
   (term (((x = y))
          (((y = z)) empty))))
  
  (test-equal
   (term (build_tree ((x = y) (y = z) (z = 1)))) 
   (term (((x = y))
          (((y = z))
           (((z = 1)) empty)))))

  (test-equal
   (term (build_tree ((x = y) (a = b) (y = z) (b = c) (z = 1) (c = 2)))) 
   (term (((x = y) (a = b))
          (((y = z) (b = c))
           (((z = 1) (c = 2)) empty)))))

  ; SO_stat
  ; weak tables
  ; wv
  (test-equal
   (term (SO_stat (\{ (\[ 1 \] = 1) \})
                  ((\{ (\[ num \] : num) \}) wv)))
   (term ()))

  (test-equal
   (term (SO_stat (\{ (\[ 1 \] = (\{ \})) \})
                  ((\{ (\[ num \] : ((\{ \}) strong)) \}) wv)))
   (term ()))

  (test-equal
   (term (SO_stat (\{ (\[ (\{ \}) \] = (\{ \})) \})
                  ((\{ (\[ ((\{ \}) strong) \] : ((\{ \}) strong)) \}) wv)))
   (term ((\{ \}))))

  ; wk
  (test-equal
   (term (SO_stat (\{ (\[ 1 \] = 1) \})
                  ((\{ (\[ num \] : num) \}) wk)))
   (term ()))

  (test-equal
   (term (SO_stat (\{ (\[ 1 \] = (\{ \})) \})
                  ((\{ (\[ num \] : ((\{ \}) strong)) \}) wk)))
   (term ((1 (\{ \})))))

  (test-equal
   (term (SO_stat (\{ (\[ (\{ \}) \] = (\{ \})) \})
                  ((\{ (\[ ((\{ \}) strong) \] : ((\{ \}) strong)) \}) wk)))
   (term (((\{ \}) (\{ \})))))

  (test-equal
   (term (SO_stat (\{ (\[ (\{ \}) \] = (x ())) \})
                  ((\{ (\[ ((\{ \}) strong) \] : ((\{ \}) strong)) \}) wk)))
   (term (((\{ \}) (x ())))))

  (test-equal
   (term (SO_stat (\{ (\[ (\{ \}) \] = (x ())) \})
                  ((\{ (\[ ((\{ \}) strong) \] : num) \}) wk)))
   (term ()))

  ; strong
  (test-equal
   (term (SO_stat (\{ (\[ (\{ \}) \] = (x ())) \})
                  ((\{ (\[ ((\{ \}) strong) \] : num) \}) strong)))
   (term ((\{ \}))))

  (test-equal
   (term (SO_stat (\{ (\[ (\{ \}) \] = (x ())) \})
                  ((\{ (\[ ((\{ \}) strong) \] : ((\{ \}) strong)) \}) strong)))
   (term ((\{ \}) (x ()))))

  ; reach
  (test-equal
   (term (reachCte_stat (1 (((a = 1)) empty) (a : num ·))))
   #t)
  
  (test-equal
   (term (reachCte_stat (1 (((a = b)) (((b = 1)) empty))
                           (b : num (a : num ·)))))
   #t)

  (test-equal
   (term (reachCte_stat (1 (((a = (num function x () (return b) end)))
                            (((b = 1)) empty)) (b : num (a : num ·)))))
   #t)

  (test-equal
   (term (reachCte_stat (1 (((a = (num function x () (return b) end)))
                            (((b = (num function x () (return c) end)))
                             (((c = 1)) empty)))
                           (c : num
                              (b : (($tup) -> num)
                                 (a : (($tup) -> num) ·))))))
   #t)
  
  (test-equal
   (term (reachCte_stat (z (((x = y) (a = b)) (((y = z) (b = c)) empty))
                           (a : num
                              (b : num
                                 (c : num
                                    (x : num
                                       (y : num
                                          (z : num ·)))))))))
   #t)

  ; tables
  (test-equal
   (term (reachCte_stat (z (((x = (\{ (\[ y \] = 1) \})))
                            (((y = z)) empty))
                           (x : ((\{ (\[ ((\{ \}) strong) \] : num) \}) strong)
                              (y : ((\{ \}) strong)
                                 (z : ((\{ \}) strong) ·))))))
   #t)

  ; wv
  ; non-ctes
  (test-equal
   (term (reachCte_stat (z (((x = (\{ (\[ 1 \] = y) \})))
                            (((y = z)) empty))
                           (x : ((\{ (\[ num \] : num) \}) wv)
                              (y : num
                                 (z : num ·))))))
   #f)

  (test-equal
   (term (reachCte_stat (z (((x = (\{ (\[ y \] = 1) \})))
                            (((y = z)) empty))
                           (x : ((\{ (\[ num \] : num) \}) wv)
                              (y : num
                                 (z : num ·))))))
   #f)

  ; cte
  (test-equal
   (term (reachCte_stat (z (((x = (\{ (\[ 1 \] = y) \})))
                            (((y = z)) empty))
                           (x : ((\{ (\[ num \] : ((\{ \}) strong)) \}) wv)
                              (y : ((\{ \}) strong)
                                 (z : ((\{ \}) strong) ·))))))
   #f)

  (test-equal
   (term (reachCte_stat (z (((x = (\{ (\[ y \] = 1) \})))
                            (((y = z)) empty))
                           (x : ((\{ (\[ ((\{ \}) strong) \] : num) \}) wv)
                              (y : ((\{ \}) strong)
                                 (z : ((\{ \}) strong) ·))))))
   #t)

  (test-equal
   (term (reachCte_stat (z (((a = y) (x = (\{ (\[ 1 \] = y) \})))
                            (((y = z)) empty))
                           (x : ((\{ (\[ ((\{ \}) strong) \] : num) \}) wv)
                              (y : ((\{ \}) strong)
                                 (z : ((\{ \}) strong) ·))))))
   #t)

  ; wk
  ; non-ctes
  (test-equal
   (term (reachCte_stat (z (((x = (\{ (\[ 1 \] = y) \})))
                            (((y = z)) empty))
                           (x : ((\{ (\[ num \] : num) \}) wk)
                              (y : num
                                 (z : num ·))))))
   #f)

  (test-equal
   (term (reachCte_stat (z (((x = (\{ (\[ y \] = 1) \})))
                            (((y = z)) empty))
                           (x : ((\{ (\[ num \] : num) \}) wk)
                              (y : num
                                 (z : num ·))))))
   #f)

  ; cte
  (test-equal
   (term (reachCte_stat (z (((x = (\{ (\[ 1 \] = y) \})))
                            (((y = z)) empty))
                           (x : ((\{ (\[ num \] : ((\{ \}) strong)) \}) wk)
                              (y : ((\{ \}) strong)
                                 (z : ((\{ \}) strong) ·))))))
   #f)

  (test-equal
   (term (reachCte_stat (z (((a = y) (x = (\{ (\[ y \] = 1) \})))
                            (((y = z)) empty))
                           (x : ((\{ (\[ ((\{ \}) strong) \] : num) \}) wk)
                              (y : ((\{ \}) strong)
                                 (z : ((\{ \}) strong) ·))))))
   #t)

  (test-equal
   (term (reachCte_stat (z (((a = y) (x = (\{ (\[ 1 \] = y) \})))
                            (((y = z)) empty))
                           (x : ((\{ (\[ ((\{ \}) strong) \] : num) \}) wk)
                              (y : ((\{ \}) strong)
                                 (z : ((\{ \}) strong) ·))))))
   #t)

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
  ; val
  (test-equal
   (judgment-holds
    (type_e · · () hole
            1
            ·
            t)
    t)
   (term ((1 : num))))
  
  (test-equal
   (judgment-holds
    (type_e · · () hole
            true
            ·
            t)
    t)
   (term ((true : bool))))
  
  (test-equal
   (judgment-holds
    (type_e · · () hole
            "asd"
            ·
            t)
    t)
   (term (("asd" : str))))
  
  (test-equal
   (judgment-holds
    (type_e · · () hole
            nil
            ·
            t)
    t)
   (term ((nil : nil))))
  
  ; var
  (test-equal
   (judgment-holds
    (type_e (x : num ·) · () hole
            x
            (x : num ·)
            t)
    t)
   (term (num)))
  
  (test-equal
   (judgment-holds
    (type_e (x : ((\{ (\[ (1 : num) \] : num) \}) strong) ·) · () hole
            (x \[ 1 \])
            (x : ((\{ (\[ (1 : num) \] : num) \}) strong) ·)
            t)
    t)
   (term (num)))
  
  (test-equal
   (judgment-holds
    (type_e (x : ((\{ (\[ (1 : num) \] : ((\{ \}) strong)) \}) strong) ·) · ()
            hole
            (x \[ 1 \])
            (x : ((\{ (\[ (1 : num) \] : ((\{ \}) strong)) \}) strong) ·)
            t)
    t)
   (term (((\{ \}) strong))))
    
  (test-equal
   (judgment-holds
    (type_e (x : ((\{ (\[ (1 : num) \] : ((\{ \}) strong)) \}) wv) ·) ·
            ; simple cfgKG with a reach. def. containing the field
            (((1 (entry ((y = hole) (x |[| 1 |]|))
                        (hole (y = (x |[| 1 |]|))))
                 ((y = (x |[| 1 |]|))) ()) ()))
            (y = hole)
            (x \[ 1 \])
            (x : ((\{ (\[ (1 : num) \] : ((\{ \}) strong)) \}) wv) ·)
            t)
    t)
   (term (((\{ \}) strong))))
  
  (test-equal
   (judgment-holds
    (type_e (x : ((\{ (\[ (1 : num) \] : ((\{ \}) strong)) \}) wv) ·) ·
            ; simple cfgKG with a reach. def. containing the field
            (((1 (entry ((y = hole) (x |[| 1 |]|))
                        (hole (y = (x |[| 1 |]|))))
                 ((y = (x |[| 1 |]|))) ()) ()))
            (y = hole)
            ; exp not reach
            (x \[ 2 \])
            (x : ((\{ (\[ (1 : num) \] : ((\{ \}) strong)) \}) wv) ·)
            t)
    t)
   (term ((nil : nil))))
  
  ; field is not reach. but also does not contain a cte
  (test-equal
   (judgment-holds
    (type_e (x : ((\{ (\[ (1 : num) \] : num) \}) wv) ·) ·
            ; simple cfgKG with a reach. def. containing the field
            (((1 (entry ((y = hole) (x |[| 1 |]|))
                        (hole (y = (x |[| 1 |]|))))
                 () ((y = (x |[| 2 |]|)))) ()))
            (y = hole)
            (x \[ 1 \])
            (x : ((\{ (\[ (1 : num) \] : num) \}) wv) ·)
            t)
    t)
   (term (num)))
    
  ; functiondef
      
  (test-equal
   (judgment-holds
    (type_e · · () hole
            (num function x () \; end)
            ·
            t)
    t)
   (term ((($tup) -> num))))
    
  (test-equal
   (judgment-holds
    (type_e · · () hole
            (num function x ((y : num)) \; end)
            ·
            t)
    t)
   (term ((($tup num) -> num))))
    
  (test-equal
   (judgment-holds
    (type_e · · () hole
            (num function x ((y : num) (z : bool)) \; end)
            ·
            t)
    t)
   (term ((($tup num bool) -> num))))
  
  ; functioncall
  (test-equal
   (judgment-holds
    (type_e (x : (($tup) -> num) ·) ·
            ()
            hole
            (x ())
            (x : (($tup) -> num) ·)
            t)
    t)
   (term (num)))
  
  (test-equal
   (judgment-holds
    (type_e (x : (($tup num) -> str) ·) ·
            ()
            hole
            (x (1))
            (x : (($tup num) -> str) ·)
            t)
    t)
   (term (str)))
  
  (test-equal
   (judgment-holds
    (type_e (x : (($tup num str) -> bool) ·) ·
            ()
            hole
            (x (1 "asd"))
            (x : (($tup num str) -> bool) ·)
            t)
    t)
   (term (bool)))
  
  (test-equal
   (judgment-holds
    (type_e (x : (($tup num str ((\{ \}) strong)) -> bool) ·) ·
            ()
            hole
            (x (1 "asd" (\{ \})))
            (x : (($tup num str ((\{ \}) strong)) -> bool) ·)
            t)
    t)
   (term (bool)))
  
  ; testing subtyping
  (test-equal
   (judgment-holds
    (type_e (x : (($tup dyn) -> bool) ·) ·
            ()
            hole
            (x (1))
            (x : (($tup dyn) -> bool) ·)
            t)
    t)
   (term (bool)))
  
  (test-equal
   (judgment-holds
    (type_e (x : (($tup ((\{ \}) strong)) -> bool) ·) ·
            ()
            hole
            (x ((\{ \})))
            (x : (($tup ((\{ \}) strong)) -> bool) ·)
            t)
    t)
   (term (bool)))
  
  (test-equal
   (judgment-holds
    (type_e (x : (($tup ((\{ \}) strong)) -> bool) ·) ·
            ()
            hole
            (x ((\{ (\[ 1 \] = true) \})))
            (x : (($tup ((\{ \}) strong)) -> bool) ·)
            t)
    t)
   (term (bool)))
  
  (test-equal
   (judgment-holds
    (type_e (x : (($tup ((\{ (\[ num \] : bool) \}) strong)) -> bool) ·) ·
            ()
            hole
            (x ((\{ (\[ 1 \] = true) \})))
            (x : (($tup ((\{ (\[ num \] : bool) \}) strong)) -> bool) ·)
            t)
    t)
   (term (bool)))
  
  (test-equal
   (judgment-holds
    (type_e (x : (($tup ((\{ (\[ num \] : bool) \}) strong)) -> bool) ·) ·
            ()
            hole
            (x ((\{ (\[ 1 \] = true) (\[ "asd" \] = 2) \})))
            (x : (($tup ((\{ (\[ num \] : bool) \}) strong)) -> bool) ·)
            t)
    t)
   (term (bool)))
  
  (test-equal
   (judgment-holds
    (type_e (x : (($tup ((\{ (\[ num \] : bool) (\[ str \] : num) \}) strong))
                  -> bool) ·) ·
                              ()
                              hole
                              (x ((\{ (\[ 1 \] = true) \})))
                              (x : (($tup ((\{ (\[ num \] : bool)
                                               (\[ str \] : num) \}) strong))
                                    -> bool) ·)
                              t)
    t)
   (term ()))
  
  ; setmetatable
  (test-equal
   (judgment-holds
    (type_e (x : ((\{ (\[ num \] : bool) \}) strong) ·) ·
            ()
            hole
            (($ENV |[| "setmetatable" |]|) (x (\{ (\[ "__mode" \] = "ak") \})))
            (x : ((\{ (\[ num \] : bool) \}) wk) ·)
            t)
    t)
   (term (((\{ (\[ num \] : bool) \}) wk))))
  
  (test-equal
   (judgment-holds
    (type_e (x : ((\{ (\[ num \] : bool) \}) strong) ·) ·
            ()
            hole
            (($ENV |[| "setmetatable" |]|) (x (\{ (\[ "__mode" \] = "av") \})))
            (x : ((\{ (\[ num \] : bool) \}) wv) ·)
            t)
    t)
   (term (((\{ (\[ num \] : bool) \}) wv))))
  
  (test-equal
   (judgment-holds
    (type_e (x : ((\{ (\[ num \] : bool) \}) strong) ·) ·
            ()
            hole
            (($ENV |[| "setmetatable" |]|)
             (x (\{ (\[ "__mode" \] = "akv") \})))
            (x : ((\{ (\[ num \] : bool) \}) wkv) ·)
            t)
    t)
   (term (((\{ (\[ num \] : bool) \}) wkv))))
  
  (test-equal
   (judgment-holds
    (type_e (x : ((\{ (\[ num \] : bool) \}) wv) ·) ·
            ()
            hole
            (($ENV |[| "setmetatable" |]|) (x (\{ (\[ "not_mode" \] = "k")
                                                  \})))
            ; weakness defaults to strong
            (x : ((\{ (\[ num \] : bool) \}) strong) ·)
            t)
    t)
   (term (((\{ (\[ num \] : bool) \}) strong))))
  
  ; method call
  (test-equal
   (judgment-holds
    (type_e (x : ((\{ (\[ str \] : (($tup) -> num)) \}) strong) ·) ·
            ()
            hole
            (x : method ())
            (x : ((\{ (\[ str \] : (($tup) -> num)) \}) strong) ·)
            t)
    t)
   (term (num)))
  
  (test-equal
   (judgment-holds
    (type_e (x : ((\{ (\[ str \] : (($tup num) -> bool)) \}) strong) ·) ·
            ()
            hole
            (x : method (1))
            (x : ((\{ (\[ str \] : (($tup num) -> bool)) \}) strong) ·)
            t)
    t)
   (term (bool)))
  
  ; parenthesized exp
  (test-equal
   (judgment-holds
    (type_e · ·
            ()
            hole
            (\( 1 \))
            ·
            t)
    t)
   (term ((1 : num))))
  
  (test-equal
   (judgment-holds
    (type_e (x : ((\{ (\[ str \] : (($tup num) -> bool)) \}) strong) ·) ·
            ()
            hole
            (\( x \))
            (x : ((\{ (\[ str \] : (($tup num) -> bool)) \}) strong) ·)
            t)
    t)
   (term (((\{ (\[ str \] : (($tup num) -> bool)) \}) strong))))
  
  ; tableconstructor
  (test-equal
   (judgment-holds
    (type_e · ·
            ()
            hole
            (\{ \})
            ·
            t)
    t)
   (term (((\{ \}) strong))))
  
  (test-equal
   (judgment-holds
    (type_e · ·
            ()
            hole
            (\{ (\[ 1 \] = true) \})
            ·
            t)
    t)
   (term (((\{ (\[ (1 : num) \] : (true : bool)) \}) strong))))
  
  (test-equal
   (judgment-holds
    (type_e · ·
            ()
            hole
            (\{ (\[ 1 \] = true) (\[ "asd" \] = "v") \})
            ·
            t)
    t)
   (term (((\{ (\[ (1 : num) \] : (true : bool))
               (\[ ("asd" : str) \] : ("v" : str)) \}) strong))))
  
  ; binary ops
  ; arithop
  (test-equal
   (judgment-holds
    (type_e · ·
            ()
            hole
            (1 + 1)
            ·
            t)
    t)
   (term (num)))
  
  (test-equal
   (judgment-holds
    (type_e · ·
            ()
            hole
            (1 - 1)
            ·
            t)
    t)
   (term (num)))
  
  (test-equal
   (judgment-holds
    (type_e · ·
            ()
            hole
            (1 * 1)
            ·
            t)
    t)
   (term (num)))
  
  (test-equal
   (judgment-holds
    (type_e · ·
            ()
            hole
            (1 / 1)
            ·
            t)
    t)
   (term (num)))
  
  (test-equal
   (judgment-holds
    (type_e · ·
            ()
            hole
            (1 ^ 1)
            ·
            t)
    t)
   (term (num)))
  
  (test-equal
   (judgment-holds
    (type_e · ·
            ()
            hole
            (1 % 1)
            ·
            t)
    t)
   (term (num)))
  
  ; relop
  (test-equal
   (judgment-holds
    (type_e · ·
            ()
            hole
            (1 < 1)
            ·
            t)
    t)
   (term (bool)))
  
  (test-equal
   (judgment-holds
    (type_e · ·
            ()
            hole
            (1 <= 1)
            ·
            t)
    t)
   (term (bool)))
  
  (test-equal
   (judgment-holds
    (type_e · ·
            ()
            hole
            (1 > 1)
            ·
            t)
    t)
   (term (bool)))
  
  (test-equal
   (judgment-holds
    (type_e · ·
            ()
            hole
            (1 >= 1)
            ·
            t)
    t)
   (term (bool)))
  
  (test-equal
   (judgment-holds
    (type_e · ·
            ()
            hole
            ("asd" >= "xcv")
            ·
            t)
    t)
   (term (bool)))
  
  (test-equal
   (judgment-holds
    (type_e · ·
            ()
            hole
            ("asd" >= 1)
            ·
            t)
    t)
   (term ()))
  
  (test-equal
   (judgment-holds
    (type_e · ·
            ()
            hole
            ("asd" == 1)
            ·
            t)
    t)
   (term ()))
  
  (test-equal
   (judgment-holds
    (type_e · ·
            ()
            hole
            ("asd" >= "asd")
            ·
            t)
    t)
   (term (bool)))
  
  ; logical
  (test-equal
   (judgment-holds
    (type_e · ·
            ()
            hole
            ("asd" and "xcv")
            ·
            t)
    t)
   (term (str)))
  
  (test-equal
   (judgment-holds
    (type_e · ·
            ()
            hole
            ("v" or "xcv")
            ·
            t)
    t)
   (term (str)))
  
  (test-equal
   (judgment-holds
    (type_e · ·
            ()
            hole
            ("asd" or 1)
            ·
            t)
    t)
   (term (dyn)))
  
  ; unop
  (test-equal
   (judgment-holds
    (type_e · ·
            ()
            hole
            (- 1)
            ·
            t)
    t)
   (term (num)))
  
  (test-equal
   (judgment-holds
    (type_e · ·
            ()
            hole
            (- "asd")
            ·
            t)
    t)
   (term ()))
  
  (test-equal
   (judgment-holds
    (type_e · ·
            ()
            hole
            (not 1)
            ·
            t)
    t)
   (term (bool)))
  
  (test-equal
   (judgment-holds
    (type_e · ·
            ()
            hole
            (not "asd")
            ·
            t)
    t)
   (term (bool)))
  
  (test-equal
   (judgment-holds
    (type_e · ·
            ()
            hole
            (\# "asd")
            ·
            t)
    t)
   (term (num)))
  
  (test-equal
   (judgment-holds
    (type_e · ·
            ()
            hole
            (\# (\{ \}))
            ·
            t)
    t)
   (term (num)))
  
  (test-equal
   (judgment-holds
    (type_e · ·
            ()
            hole
            (\# 1)
            ·
            t)
    t)
   (term ()))
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
  
  ; skip
  (test-equal
   (judgment-holds
    (type_s · · () hole
            \;
            ·))
   #t)
  
  ; break
  (test-equal
   (judgment-holds
    (type_s · · () hole
            break
            ·))
   #t)
  
  ; return
  (test-equal
   (judgment-holds
    (type_s · · () hole
            (return)
            ·))
   #t)
  
  (test-equal
   (judgment-holds
    (type_s · · () hole
            (return 1 true "asd")
            ·))
   #t)
  
  (test-equal
   (judgment-holds
    (type_s (x : ((\{ (\[ num \] : bool) \}) wv) ·) ·
            ()
            hole
            (return (($ENV |[| "setmetatable" |]|)
                     (x (\{ (\[ "not_mode" \] = "k")
                            \}))))
            ; weakness defaults to strong
            (x : ((\{ (\[ num \] : bool) \}) strong) ·))
    )
   #t)
  
  
  ; funcall
  ; functioncall
  (test-equal
   (judgment-holds
    (type_s (x : (($tup) -> num) ·) ·
            ()
            hole
            ($statFunCall x ())
            (x : (($tup) -> num) ·)))
   #t)
  
  (test-equal
   (judgment-holds
    (type_s (x : (($tup num) -> str) ·) ·
            ()
            hole
            ($statFunCall x (1))
            (x : (($tup num) -> str) ·)
            ))
   #t)
  
  (test-equal
   (judgment-holds
    (type_s (x : (($tup num str) -> bool) ·) ·
            ()
            hole
            ($statFunCall x (1 "asd"))
            (x : (($tup num str) -> bool) ·)
            )
    )
   #t)
  
  (test-equal
   (judgment-holds
    (type_s (x : (($tup num str ((\{ \}) strong)) -> bool) ·) ·
            ()
            hole
            ($statFunCall x (1 "asd" (\{ \})))
            (x : (($tup num str ((\{ \}) strong)) -> bool) ·)
            )
    )
   #t)
  
  ; testing subtyping
  (test-equal
   (judgment-holds
    (type_s (x : (($tup dyn) -> bool) ·) ·
            ()
            hole
            ($statFunCall x (1))
            (x : (($tup dyn) -> bool) ·)
            )
    )
   #t)
  
  (test-equal
   (judgment-holds
    (type_s (x : (($tup ((\{ \}) strong)) -> bool) ·) ·
            ()
            hole
            ($statFunCall x ((\{ \})))
            (x : (($tup ((\{ \}) strong)) -> bool) ·)
            )
    )
   #t)
  
  (test-equal
   (judgment-holds
    (type_s (x : (($tup ((\{ \}) strong)) -> bool) ·) ·
            ()
            hole
            ($statFunCall x ((\{ (\[ 1 \] = true) \})))
            (x : (($tup ((\{ \}) strong)) -> bool) ·)
            )
    )
   #t)
  
  (test-equal
   (judgment-holds
    (type_s (x : (($tup ((\{ (\[ num \] : bool) \}) strong)) -> bool) ·) ·
            ()
            hole
            ($statFunCall x ((\{ (\[ 1 \] = true) \})))
            (x : (($tup ((\{ (\[ num \] : bool) \}) strong)) -> bool) ·)
            )
    )
   #t)
  
  (test-equal
   (judgment-holds
    (type_s (x : (($tup ((\{ (\[ num \] : bool) \}) strong)) -> bool) ·) ·
            ()
            hole
            ($statFunCall x ((\{ (\[ 1 \] = true) (\[ "asd" \] = 2) \})))
            (x : (($tup ((\{ (\[ num \] : bool) \}) strong)) -> bool) ·)
            )
    )
   #t)
  
  (test-equal
   (judgment-holds
    (type_s (x : (($tup ((\{ (\[ num \] : bool) (\[ str \] : num) \}) strong))
                  -> bool) ·)
            ·
            ()
            hole
            ($statFunCall x ((\{ (\[ 1 \] = true) \})))
            (x : (($tup ((\{ (\[ num \] : bool)
                             (\[ str \] : num) \}) strong)) -> bool) ·)
            )
    )
   #t)
  
  ; setmetatable
  (test-equal
   (judgment-holds
    (type_s (x : ((\{ (\[ num \] : bool) \}) strong) ·) ·
            ()
            hole
            ($statFunCall ($ENV |[| "setmetatable" |]|)
                          (x (\{ (\[ "__mode" \] = "ak") \})))
            (x : ((\{ (\[ num \] : bool) \}) wk) ·)
            )
    )
   #t)
  
  (test-equal
   (judgment-holds
    (type_s (x : ((\{ (\[ num \] : bool) \}) strong) ·) ·
            ()
            hole
            ($statFunCall ($ENV |[| "setmetatable" |]|)
                          (x (\{ (\[ "__mode" \] = "av") \})))
            (x : ((\{ (\[ num \] : bool) \}) wv) ·)
            )
    )
   #t)
  
  (test-equal
   (judgment-holds
    (type_s (x : ((\{ (\[ num \] : bool) \}) strong) ·) ·
            ()
            hole
            ($statFunCall ($ENV |[| "setmetatable" |]|)
                          (x (\{ (\[ "__mode" \] = "akv") \})))
            (x : ((\{ (\[ num \] : bool) \}) wkv) ·)
            )
    )
   #t)
  
  (test-equal
   (judgment-holds
    (type_s (x : ((\{ (\[ num \] : bool) \}) wv) ·) ·
            ()
            hole
            ($statFunCall ($ENV |[| "setmetatable" |]|)
                          (x (\{ (\[ "not_mode" \] = "k") \})))
            ; weakness defaults to strong
            (x : ((\{ (\[ num \] : bool) \}) strong) ·)
            )
    )
   #t)
  
  ; setmetatable
  (test-equal
   (judgment-holds
    (type_s (x : ((\{ (\[ num \] : bool) \}) strong) ·) ·
            ()
            hole
            ($statFunCall ($ENV |[| "setmetatable" |]|)
                          (x (\{ (\[ "__mode" \] = "ak") \})))
            (x : ((\{ (\[ num \] : bool) \}) wk) ·)
            )
    )
   #t)
  
  (test-equal
   (judgment-holds
    (type_s (x : ((\{ (\[ num \] : bool) \}) strong) ·) ·
            ()
            hole
            ($statFunCall ($ENV |[| "setmetatable" |]|)
                          (x (\{ (\[ "__mode" \] = "av") \})))
            (x : ((\{ (\[ num \] : bool) \}) wv) ·)
            )
    )
   #t)
  
  (test-equal
   (judgment-holds
    (type_s (x : ((\{ (\[ num \] : bool) \}) strong) ·) ·
            ()
            hole
            ($statFunCall ($ENV |[| "setmetatable" |]|)
                          (x (\{ (\[ "__mode" \] = "akv") \})))
            (x : ((\{ (\[ num \] : bool) \}) wkv) ·)
            )
    )
   #t)
  
  (test-equal
   (judgment-holds
    (type_s (x : ((\{ (\[ num \] : bool) \}) wv) ·) ·
            ()
            hole
            ($statFunCall ($ENV |[| "setmetatable" |]|)
                          (x (\{ (\[ "not_mode" \] = "k") \})))
            ; weakness defaults to strong
            (x : ((\{ (\[ num \] : bool) \}) strong) ·)
            )
    )
   #t)
  
    
  
  (test-equal
   (judgment-holds
    (type_s (t1 : ((|{|  |}|) strong)
                ($ENV : ((\{ (\[ str \] : num) \}) strong) ·))
            · () hole
            ($statFunCall ($ENV |[| "setmetatable" |]|)
                          (t1 (|{| (|[| "__mode" |]| = "v") |}|)))
            (t1 : ((|{| |}|) wv) ($ENV : ((|{| (|[| str |]| : num) |}|)
                                          strong) ·))))
   #t)
  
  ; method call
  (test-equal
   (judgment-holds
    (type_s (x : ((\{ (\[ str \] : (($tup) -> num)) \}) strong) ·) ·
            ()
            hole
            ($statFunCall x : method ())
            (x : ((\{ (\[ str \] : (($tup) -> num)) \}) strong) ·)
            )
    )
   #t)
  
  (test-equal
   (judgment-holds
    (type_s (x : ((\{ (\[ str \] : (($tup num) -> bool)) \}) strong) ·) ·
            ()
            hole
            ($statFunCall x : method (1))
            (x : ((\{ (\[ str \] : (($tup num) -> bool)) \}) strong) ·)
            )
    )
   #t)
  
  ; do-end
  (test-equal
   (judgment-holds
    (type_s (x : ((\{ (\[ num \] : bool) \}) strong) ·) ·
            ()
            hole
            (do ($statFunCall ($ENV |[| "setmetatable" |]|)
                              (x (\{ (\[ "__mode" \] = "akv") \})))
              end)
            (x : ((\{ (\[ num \] : bool) \}) wkv) ·)
            )
    )
   #t)
  
  ; conditional
    
  (test-equal
   (judgment-holds
    (type_s (x : num ·) · () hole
            (if x then (x = 1) else (x = 2) end)
            (x : num ·)))
   #t)
  
  (test-equal
   (judgment-holds
    (type_s (x : str ·) · () hole
            (if (not x) then (x = "k") else (x = "v") end)
            ; supremum type
            (x : str ·)))
   #t)
  
  ; while
  (test-equal
   (judgment-holds
    (type_s (x : num ·) · () hole
            (while (not x) do (x = 1) end)
            (x : num ·)))
   #t)
  
  (test-equal
   (judgment-holds
    (type_s (x : ((\{ (\[ num \] : bool) \}) strong) ·) · () hole
            (while (($ENV |[| "setmetatable" |]|)
                    (x (\{ (\[ "__mode" \] = "k") \})))
                   do
                   ($statFunCall ($ENV |[| "setmetatable" |]|)
                                 (x (\{ (\[ "__mode" \] = "v") \})))
                   end)
            (x : ((|{| (|[| num |]| : bool) |}|) wv) ·)))
   #t)
  
  (test-equal
   (judgment-holds
    (type_s (x : ((\{ (\[ num \] : bool) \}) strong) (y : num ·)) · () hole
            (while (($ENV |[| "setmetatable" |]|)
                    (x (\{ (\[ "__mode" \] = "k") \})))
                   do
                   (y = 1)
                   end)
            (x : ((\{ (\[ num \] : bool) \}) wk) (y : num ·))))
   #t)
  
  ; concat. stat.
  (test-equal
   (judgment-holds
    (type_s (x : num ·) · () hole
            ((x = 2) (x = (\{ \})))
            Γ) (Γ))
   '())
      
  (test-equal
   (judgment-holds
    (type_s (x : num ·) · () hole
            ((x = 2) (x = 3))
            (x : num ·)))
   #t)

  ; assign
  (test-equal
   (judgment-holds
    (type_s (x : num (y : str ·)) · () hole
            (x y = 3 "asd")
            (x : num (y : str ·))))
   #t)
  
  (test-equal
   (judgment-holds
    (type_s (x : num (y : str ·)) · () hole
            (x y = 3)
            (x : num (y : str ·))))
   #t)

  (test-equal
   (judgment-holds
    (type_s (x : num (y : str ·)) · () hole
            (x y = 3 "asd" true)
            (x : num (y : str ·))))
   #t)
  
  ; field assign
  ; change field
  (test-equal
   (judgment-holds
    (type_s (x : ((\{ (\[ (1 : num) \] : num) \}) strong) ·) · () hole
            ((x \[ 1 \]) = 3)
            (x : ((\{ (\[ (1 : num) \] : num) \}) strong) ·)))
   #t)
    
  (test-equal
   (judgment-holds
    (type_s (x : ((\{ (\[ (2 : num) \] : str) \}) strong) ·) · () hole
            ((x \[ 2 \]) = "asd")
            (x : ((\{ (\[ (2 : num) \] : str) \}) strong) ·))
    )
   #t)
      
  (test-equal
   (judgment-holds
    (type_s (x : ((\{ (\[ (1 : num) \] : num) \}) wv) ·) · () hole
            ((x \[ 1 \]) = 3)
            (x : ((\{ (\[ (1 : num) \] : num) \}) wv) ·))
    )
   #t)
      
  (test-equal
   (judgment-holds
    (type_s (x : ((\{ (\[ (1 : num) \] : (($tup dyn) -> dyn)) \}) wv) ·) · ()
            hole
            ((x \[ 1 \]) = (dyn function x ((y : dyn)) \; end))
            (x : ((\{ (\[ (1 : num) \] : (($tup dyn) -> dyn)) \}) wv) ·)))
   #t)
  
  ; add field
  (test-equal
   (judgment-holds
    (type_s (x : ((\{ (\[ (($tup dyn) -> dyn) \] : num) \}) strong) ·)
            · () hole
            ((x = (\{ (\[ (dyn function y ((x : dyn)) \; end) \] = 2)
                      \}))
             ((x \[ 1 \]) = 2))
            (x : ((\{ (\[ (1 : num) \] : (2 : num))
                      (\[ (($tup dyn) -> dyn) \] : num) \}) strong) ·)))
   #t)
        
  (test-equal
   (judgment-holds
    (type_s (x : ((\{ \}) wk) ·) · () hole
            ((x \[ 1 \]) = 2)
            (x : ((\{ (\[ (1 : num) \] : (2 : num)) \}) wk) ·))
    )
   #t)
        
  (test-equal
   (judgment-holds
    (type_s (x : ((\{ \}) wk) ·) · () hole
            ((x \[ (dyn function x ((y : dyn)) \; end) \])
             = (dyn function x ((y : dyn)) \; end))
            (x : ((\{ (\[ (($tup dyn) -> dyn) \] :
                          (($tup dyn) -> dyn)) \})
                  wk) ·))
    )
   #t)
          
  (test-equal
   (judgment-holds
    (type_s (x : ((\{ (\[ (($tup dyn) -> dyn) \] : num) \}) strong) ·)
            · ()
            hole
            (x = (\{ (\[ (dyn function y ((x : dyn)) \; end) \] = 2)
                     \}))
            (x : ((\{ (\[ (($tup dyn) -> dyn) \] : num) \}) strong) ·))
    )
   #t)
      
  (test-equal
   (judgment-holds
    (type_s (x : ((\{ (\[ (($tup dyn) -> dyn) \] : num) \}) strong) ·)
            · ()
            hole
            (x = (\{ (\[ (dyn function y ((z : dyn)) \; end) \] = 2)
                     \}))
            (x : ((\{ (\[ (($tup dyn) -> dyn) \] : num) \}) strong) ·))
    )
   #t)

  ; local vars
  ; local var. removed from Γ
  (test-equal
   (judgment-holds
    (type_s (a : num ·) · () hole
            (local (x : num) (y : bool) (z : str) = 1 true "asd"
              in \; end) (a : num ·)))
   #t)

  (test-equal
   (judgment-holds
    (type_s (a : num ·) · () hole
            (local (x : num) (y : bool) = 1 true "asd"
              in \; end) (a : num ·)))
   #t)

  (test-equal
   (judgment-holds
    (type_s (a : num ·) · () hole
            (local (x : num) (y : bool) = 1 
              in \; end) (a : num ·)))
   #t)

  ; correct subtyping
  (test-equal
   (judgment-holds
    (type_s (a : num ·) · () hole
            (local (x : str) = "k" 
              in \; end) (a : num ·)))
   #t)

  (test-equal
   (judgment-holds
    (type_s (a : num ·) · () hole
            (local (x : strK) = "v" 
              in \; end) (a : num ·)))
   #f)
  
  (test-equal
   (judgment-holds
    (type_s · · () hole
            (local (x : ((\{ (\[ (($tup dyn) -> dyn) \] : num) \})
                         strong)) =
              (\{ (\[ (dyn function y ((z : dyn)) \; end) \] = 2) \})
              in \; end) ·))
   #t)

  (test-equal
   (judgment-holds
    (type_s · · () hole
            (local (x : ((\{ (\[ (($tup num) -> dyn) \] : num) \})
                         strong)) =
              (\{ (\[ (dyn function y ((z : num)) \; end) \] = 2) \})
              in \; end) ·))
   #t)

  (test-equal
   (judgment-holds
    (type_s · · () hole
            (local (x : ((\{ (\[ (($tup ((\{ \}) strong)) -> str) \] : num) \})
                         strong)) =
              (\{ (\[ (str function y ((z : ((\{ \}) strong))) \; end) \] =
                      2) \})
              in \; end) ·))
   #t)
      
  (test-results))