#lang racket

(require redex
         "../data_flow_analysis.rkt")

(define (data-flow-analysis-test-suite)

  
  ;                                                                                                              
  ;                                                                                                              
  ;                                                                                                              
  ;   ;                             ;                       ;       ;;;;                        ;                
  ;   ;                             ;                       ;          ;                        ;                
  ;   ;                                                     ;          ;                        ;                
  ;   ; ;;;      ;;;;    ;;;;;    ;;;       ;;;             ; ;;;      ;        ;;;      ;;;    ;    ;    ;;;;;  
  ;   ;;   ;    ;    ;  ;     ;     ;      ;   ;            ;;   ;     ;       ;   ;    ;   ;   ;  ;;    ;     ; 
  ;   ;     ;        ;  ;           ;     ;                 ;     ;    ;      ;     ;  ;        ; ;      ;       
  ;   ;     ;   ;;;;;;  ;;;;        ;     ;                 ;     ;    ;      ;     ;  ;        ;;;      ;;;;    
  ;   ;     ;  ;;    ;      ;;;     ;     ;                 ;     ;    ;      ;     ;  ;        ;  ;         ;;; 
  ;   ;     ;  ;     ;        ;     ;     ;                 ;     ;    ;      ;     ;  ;        ;   ;          ; 
  ;   ;;   ;   ;    ;;  ;     ;     ;      ;   ;            ;;   ;     ;       ;   ;    ;   ;   ;    ;   ;     ; 
  ;   ; ;;;     ;;;; ;   ;;;;;   ;;;;;;;    ;;;             ; ;;;       ;;;     ;;;      ;;;    ;     ;   ;;;;;  
  ;                                                                                                              
  ;                                                                                                              
  ;                                                                                                              
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
  ; values
  (test-equal
   (judgment-holds (basic_blocks
                    hole
                    $nopos
                    ()
                    1
                    1
                    (((1 ((hole 1)))
                      ()))
                    ((continue 1))))
   #t)

  (test-equal
   (judgment-holds (basic_blocks
                    hole
                    $nopos
                    ()
                    true
                    1
                    (((1 ((hole true)))
                      ()))
                    ((continue 1))))
   #t)

  (test-equal
   (judgment-holds (basic_blocks
                    hole
                    $nopos
                    ()
                    "asd"
                    1
                    (((1 ((hole "asd")))
                      ()))
                    ((continue 1))))
   #t)

  ; var
  (test-equal
   (judgment-holds (basic_blocks
                    hole
                    $nopos
                    ()
                    x
                    1
                    (((1 ((hole x)))
                      ()))
                    ((continue 1))))
   #t)

  (test-equal
   (judgment-holds (basic_blocks
                    hole
                    $nopos
                    ()
                    (x \[ 1 \])
                    1
                    (((1 (((hole \[ 1 \]) x)
                          ((x \[ hole \]) 1)
                          (hole (x \[ 1 \]))))
                      ()))
                    ((continue 1))))
   #t)

  ; vararg
  (test-equal
   (judgment-holds (basic_blocks
                    hole
                    $nopos
                    ()
                    <<<
                    1
                    (((1 ((hole <<<)))
                      ()))
                    ((continue 1))))
   #t)

  ; fun call
  (test-equal
   (judgment-holds (basic_blocks
                    hole
                    $nopos
                    ()
                    (x ())
                    1
                    (((1 (((hole ()) x) (hole (x ()))))
                      ()))
                    ((continue 1))))
   #t)

  (test-equal
   (judgment-holds (basic_blocks
                    hole
                    $nopos
                    ()
                    (x (1))
                    1
                    (((1 (((hole (1)) x) ((x (hole)) 1) (hole (x (1)))))
                      ()))
                    ((continue 1))))
   #t)

  (test-equal
   (judgment-holds (basic_blocks
                    hole
                    $nopos
                    ()
                    (x (1 2))
                    1
                    (((1 (((hole (1 2)) x) ((x (hole 2)) 1)
                                           ((x (1 hole)) 2) (hole (x (1 2)))))
                      ()))
                    ((continue 1))))
   #t)

  (test-equal
   (judgment-holds (basic_blocks
                    hole
                    $nopos
                    ()
                    (x : y (1 2))
                    1
                    (((1 (((hole : y (1 2)) x) ((x : y (hole 2)) 1)
                                               ((x : y (1 hole)) 2) (hole (x : y (1 2)))))
                      ()))
                    ((continue 1))))
   #t)

  ; builtin
  (test-equal
   (judgment-holds (basic_blocks
                    hole
                    $nopos
                    ()
                    ($builtIn assert (1 2))
                    1
                    (((1 ((($builtIn assert (hole 2)) 1)
                          (($builtIn assert (1 hole)) 2)
                          (hole ($builtIn assert (1 2)))))
                      ()))
                    ((continue 1))))
   #t)

  ; parent exp
  (test-equal
   (judgment-holds (basic_blocks
                    hole
                    $nopos
                    ()
                    (\( 1 \))
                    1
                    (((1 (((\( hole \)) 1)
                          (hole (\( 1 \)))))
                      ()))
                    ((continue 1))))
   #t)

  ; table constructor
  (test-equal
   (judgment-holds (basic_blocks
                    hole
                    $nopos
                    ()
                    (\{ \})
                    1
                    (((1 ((hole (\{ \}))))
                      ()))
                    ((continue 1))))
   #t)

  (test-equal
   (judgment-holds (basic_blocks
                    hole
                    $nopos
                    ()
                    (\{ (\[ 1 \] = 2) \})
                    1
                    (((1 (((\{ (\[ hole \] = 2) \}) 1)
                          ((\{ (\[ 1 \] = hole) \}) 2)
                          (hole (\{ (\[ 1 \] = 2) \}))
                          ))
                      ()))
                    ((continue 1))))
   #t)

  (test-equal
   (judgment-holds (basic_blocks
                    hole
                    $nopos
                    ()
                    (\{ (\[ 1 \] = 2) (\[ 3 \] = 4) \})
                    1
                    (((1 (((\{ (\[ hole \] = 2) (\[ 3 \] = 4) \}) 1)
                          ((\{ (\[ 1 \] = hole) (\[ 3 \] = 4) \}) 2)
                          ((\{ (\[ 1 \] = 2) (\[ hole \] = 4) \}) 3)
                          ((\{ (\[ 1 \] = 2) (\[ 3 \] = hole) \}) 4)
                          (hole (\{ (\[ 1 \] = 2) (\[ 3 \] = 4) \}))
                          ))
                      ()))
                    ((continue 1))))
   #t)

  ; binop
  (test-equal
   (judgment-holds (basic_blocks
                    hole
                    $nopos
                    ()
                    (1 + 2)
                    1
                    (((1 (((hole + 2) 1)
                          ((1 + hole) 2)
                          (hole (1 + 2))
                          ))
                      ()))
                    ((continue 1))))
   #t)

  (test-equal
   (judgment-holds (basic_blocks
                    hole
                    $nopos
                    ()
                    (1 / 2)
                    1
                    (((1 (((hole / 2) 1)
                          ((1 / hole) 2)
                          (hole (1 / 2))
                          ))
                      ()))
                    ((continue 1))))
   #t)

  ; unop
  (test-equal
   (judgment-holds (basic_blocks
                    hole
                    $nopos
                    ()
                    (- 1)
                    1
                    (((1 (((- hole) 1)
                          (hole (- 1))
                          ))
                      ()))
                    ((continue 1))))
   #t)

  (test-equal
   (judgment-holds (basic_blocks
                    hole
                    $nopos
                    ()
                    (not 1)
                    1
                    (((1 (((not hole) 1)
                          (hole (not 1))
                          ))
                      ()))
                    ((continue 1))))
   #t)

  ; functiondef
  (test-equal
   (judgment-holds (basic_blocks
                    hole
                    $nopos
                    ()
                    (num function x ((y : num)) (return y) end)
                    1
                    (((1 ((hole (num function x ((y : num)) (return y)
                                     end))
                          ))
                      ()))
                    ((continue 1))))
   #t)
  ;                                               
  ;                                               
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
   (judgment-holds (basic_blocks
                    hole
                    $nopos
                    ()
                    \;
                    1
                    (((1 ((hole \;)))
                      ()))
                    ((continue 1))))
   #t)

  ; return
  (test-equal
   (judgment-holds
    (basic_blocks hole $nopos () (return 1) 1
                  (((1 (((return hole) 1) (hole (return 1)))) ()))
                  ((1 $nopos)
                   (continue 1))))
   #t)

  (test-equal
   (judgment-holds
    (basic_blocks hole $nopos () (return 1 2 3) 1
                  (((1 (((return hole 2 3) 1)
                        ((return 1 hole 3) 2)
                        ((return 1 2 hole) 3)
                        (hole (return 1 2 3)))) ()))
                  ((1 $nopos)
                   (continue 1))))
   #t)

  ; funcall
  (test-equal
   (judgment-holds (basic_blocks
                    hole
                    $nopos
                    ()
                    ($statFunCall x ())
                    1
                    (((1 ((($statFunCall hole ()) x)
                          (hole ($statFunCall x ())))) ()))
                    ((continue 1))))
   #t)
  
  (test-equal
   (judgment-holds (basic_blocks
                    hole
                    $nopos
                    ()
                    ($statFunCall x (1))
                    1
                    (((1 ((($statFunCall hole (1)) x)
                          (($statFunCall x (hole)) 1)
                          (hole ($statFunCall x (1))))) ()))
                    ((continue 1))))
   #t)
  
  (test-equal
   (judgment-holds (basic_blocks
                    hole
                    $nopos
                    ()
                    ($statFunCall x (1 2))
                    1
                    (((1 ((($statFunCall hole (1 2)) x)
                          (($statFunCall x (hole 2)) 1)
                          (($statFunCall x (1 hole)) 2)
                          (hole ($statFunCall x (1 2))))) ()))
                    ((continue 1))))
   #t)

  ; method call
  (test-equal
   (judgment-holds (basic_blocks
                    hole
                    $nopos
                    ()
                    ($statFunCall x : y (1 2))
                    1
                    (((1 ((($statFunCall hole : y (1 2)) x)
                          (($statFunCall x : y (hole 2)) 1)
                          (($statFunCall x : y (1 hole)) 2)
                          (hole ($statFunCall x : y (1 2))))) ()))
                    ((continue 1))))
   #t)

  ; var assign
  (test-equal
   (judgment-holds (basic_blocks
                    hole
                    $nopos
                    ()
                    (x = 1)
                    1
                    (((1 (((hole = 1) x)
                          ((x = hole) 1)
                          (hole (x = 1)))) ()))
                    ((continue 1))
                    )
                   )
   #t)
  
  (test-equal
   (judgment-holds (basic_blocks
                    hole
                    $nopos
                    ()
                    (x y = 1 2)
                    1
                    (((1 (((hole y = 1 2) x)
                          ((x hole = 1 2) y)
                          ((x y = hole 2) 1)
                          ((x y = 1 hole) 2)
                          (hole (x y = 1 2)))) ()))
                    ((continue 1))
                    )
                   )
   #t)
  
  (test-equal
   (judgment-holds (basic_blocks
                    hole
                    $nopos
                    ()
                    (x y = 1 2 3)
                    1
                    (((1 (((hole y = 1 2 3) x)
                          ((x hole = 1 2 3) y)
                          ((x y = hole 2 3) 1)
                          ((x y = 1 hole 3) 2)
                          ((x y = 1 2 hole) 3)
                          (hole (x y = 1 2 3)))) ()))
                    ((continue 1))
                    )
                   )
   #t)
  
  (test-equal
   (judgment-holds (basic_blocks
                    hole
                    $nopos
                    ()
                    (x y z = 1 2)
                    1
                    (((1 (((hole y z = 1 2) x)
                          ((x hole z = 1 2) y)
                          ((x y hole = 1 2) z)
                          ((x y z = hole 2) 1)
                          ((x y z = 1 hole) 2)
                          (hole (x y z = 1 2)))) ()))
                    ((continue 1))
                    )
                   )
   #t)

  ; do-end
  (test-equal
   (judgment-holds (basic_blocks
                    hole
                    $nopos
                    ()
                    (do (x = 1) end)
                    1
                    (((1 ((hole (do (x = 1) end))
                          ((do (hole = 1) end) x)
                          ((do (x = hole) end) 1)
                          ((do hole end) (x = 1)))) ()))
                    ((continue 1))
                    )
                   )
   #t)
  
  ; conditional
  (test-equal
   (judgment-holds (basic_blocks
                    hole
                    $nopos
                    ()
                    (if 1 then \; else \; end)
                    1
                    (((1 (((if hole then |;| else |;| end) 1)))
                      (2 3))
                     ((2 (((if 1 then hole else |;| end) |;|))) ())
                     ((3 (((if 1 then |;| else hole end) |;|))) ()))
                    ((2 $nopos) (3 $nopos))))
   #t)
  
  (test-equal
   (judgment-holds (basic_blocks
                    hole
                    $nopos
                    ()
                    (if (x \[ 1 \]) then \; else \; end)
                    1
                    (((1
                       (((if (hole |[| 1 |]|) then |;| else |;| end) x)
                        ((if (x |[| hole |]|) then |;| else |;| end) 1)
                        ((if hole then |;| else |;| end) (x |[| 1 |]|))))
                      (2 3))
                     ((2 (((if (x |[| 1 |]|) then hole else |;| end) |;|))) ())
                     ((3 (((if (x |[| 1 |]|) then |;| else hole end) |;|))) ()))
                    ((2 $nopos) (3 $nopos))))
   #t)

  (test-equal
     (judgment-holds (basic_blocks
                      hole
                      $nopos
                      ()
                      ((if 1 then ((x = 1) \;) else (\; \;) end) \;)
                      1
                      (((1 ((((if hole then ((x = 1) |;|)
                                  else (|;| |;|) end) |;|) 1))) (2 3))
                       ((2
                         ((((if 1 then ((hole = 1) |;|)
                                else (|;| |;|) end) |;|) x)
                          (((if 1 then ((x = hole) |;|)
                                else (|;| |;|) end) |;|) 1)
                          (((if 1 then (hole |;|)
                                else (|;| |;|) end) |;|) (x = 1))
                          (((if 1 then ((x = 1) hole)
                                else (|;| |;|) end) |;|) |;|)))
                        (4))
                       ((3 ((((if 1 then ((x = 1) |;|)
                                  else (hole |;|) end) |;|) |;|)
                            (((if 1 then ((x = 1) |;|) else (|;| hole)
                                  end) |;|) |;|))) (4))
                       ((4 ((((if 1 then ((x = 1) |;|) else (|;| |;|) end) hole)
                             |;|))) ()))
                      ((continue 4))))
     #t)
  
  ; while
  (test-equal
   (judgment-holds (basic_blocks
                    hole
                    $nopos
                    ()
                    (while 1 do \; end)
                    1
                    (((1 (((while hole do |;| end) 1)))
                      (2))
                     ((2 (((while 1 do hole end) |;|))) (1)))
                    ((1 $nopos))))
   #t)
  
  (test-equal
   (judgment-holds (basic_blocks
                    hole
                    $nopos
                    ()
                    ((while 1 do break end) \;)
                    1
                    (((1 ((((while hole do break end) |;|) 1))) (2 3))
                     ((2 ((((while 1 do hole end) |;|) break))) (3))
                     ((3 ((((while 1 do break end) hole) |;|))) ()))
                    ((continue 3))))
   #t)
  
    (test-equal
     (judgment-holds (basic_blocks
                      hole
                      $nopos
                      ()
                      ((while 1 do
                              (if 1 then break else |;| end)
                              end) |;|)
                      1
                      (((1 ((((while hole do
                                           (if 1 then break else |;| end)
                                           end) |;|) 1))) (2 5))
                       ((2 ((((while 1 do (if hole then break else |;| end)
                                     end) |;|) 1))) (3 4))
                       ((3 ((((while 1 do (if 1 then hole else |;| end)
                                     end) |;|) break))) (5))
                       ((4 ((((while 1 do (if 1 then break else hole end)
                                     end) |;|) |;|))) (1))
                       ((5 ((((while 1 do (if 1 then break else |;| end)
                                     end) hole) |;|))) ()))
                      ((continue 5))))
     #t)
  
    (test-equal
     (judgment-holds (basic_blocks
                      hole
                      $nopos
                      ()
                      ((while 1 do
                              (while 1 do break end)
                              end) |;|)
                      1
                      (((1 ((((while hole do (while 1 do break end) end)
                                    |;|) 1))) (2 4))
                       ((2 ((((while 1 do (while hole do break end) end)
                              |;|) 1))) (3 1))
                       ((3 ((((while 1 do (while 1 do hole end) end)
                              |;|) break))) (1))
                       ((4 ((((while 1 do (while 1 do break end) end) hole)
                             |;|))) ()))
                      ((continue 4)))
                     )
     #t)

  (test-equal
   (judgment-holds (basic_blocks
                    hole $nopos 
                    () ((while 1 do (\; break) end) \; \;) 1 
                   (((1 ((((while hole do (|;| break) end) |;| |;|) 1)))
                       (2 3))
                      ((2 ((((while 1 do (hole break) end) |;| |;|) |;|)
                           (((while 1 do (|;| hole) end) |;| |;|) break))) (3))
                      ((3 ((((while 1 do (|;| break) end) hole |;|) |;|)
                           (((while 1 do (|;| break) end) |;| hole) |;|))) ()))
                     ((continue 3))))
   #t)

  ; unreachable instructions
  (test-equal
   (judgment-holds (basic_blocks
                    hole $nopos 
                    () ((while 1 do (break break) end) \; \;) 1
                    (((1 ((((while hole do (break break) end) |;| |;|) 1)))
                      (2 4))
                     ((2 ((((while 1 do (hole break) end) |;| |;|) break)))
                      (4))
                     ((3 ((((while 1 do (break hole) end) |;| |;|) break)))
                      (4))
                     ((4 ((((while 1 do (break break) end) hole |;|) |;|)
                          (((while 1 do (break break) end) |;| hole) |;|))) ()))
                      ((continue 4))))
   #t)
  
  ; local var def
  (test-equal
   (judgment-holds (basic_blocks
                    hole
                    $nopos
                    ()
                    (local (x : num) = 1 in \; end)
                    1
                    (((1 (((local (x : num) = hole in |;| end) 1)
                          ((local hole in |;| end) (x = 1))
                          ((local (x : num) = 1 in hole end) |;|))) ()))
                    ((continue 1))
                    )
                   )
   #t)
  
  (test-equal
   (judgment-holds (basic_blocks
                    hole
                    $nopos
                    ()
                    (local (x : num) (y : str) = 1 "asd" in \; end)
                    1
                    (((1 (((local (x : num) (y : str) = hole "asd" in
                             |;| end) 1)
                          ((local (x : num) (y : str) = 1 hole in
                             |;| end) "asd")
                          ((local hole in |;| end) (x y = 1 "asd"))
                          ((local (x : num) (y : str) = 1 "asd" in hole
                             end) |;|))) ()))
                    ((continue 1))
                    )
                   )
   #t)
  
  (test-equal
   (judgment-holds (basic_blocks
                    hole
                    $nopos
                    ()
                    (local (x : num) (y : str) = 1 "asd" true in \; end)
                    1
                    (((1 (((local (x : num) (y : str) = hole "asd" true in
                             |;| end) 1)
                          ((local (x : num) (y : str) = 1 hole true in
                             |;| end) "asd")
                          ((local (x : num) (y : str) = 1 "asd" hole in
                             |;| end) true)
                          ((local hole in |;| end) (x y = 1 "asd" true))
                          ((local (x : num) (y : str) = 1 "asd" true in
                             hole
                             end) |;|))) ()))
                    ((continue 1))
                    )
                   )
   #t)
  
  (test-equal
   (judgment-holds (basic_blocks
                    hole
                    $nopos
                    ()
                    (local (x : num) (y : str) = 1 in \; end)
                    1
                    (((1 (((local (x : num) (y : str) = hole in
                             |;| end) 1)
                          ((local hole in |;| end) (x y = 1))
                          ((local (x : num) (y : str) = 1 in
                             hole
                             end) |;|))) ()))
                    ((continue 1))
                    )
                   )
   #t)
  
  (test-equal
   (judgment-holds (basic_blocks
                    hole
                    $nopos
                    ()
                    (local (x : num) = (\{ (\[ 1 \] = 2) \}) in \; end)
                    1
                    (((1 (((local (x : num) =
                             (\{ (\[ hole \] = 2) \}) in |;| end) 1)
                          ((local (x : num) =
                             (\{ (\[ 1 \] = hole) \}) in |;| end) 2)
                          ((local (x : num) = hole in |;| end)
                           (\{ (\[ 1 \] = 2) \}))
                          ((local hole in |;| end)
                           (x = (\{ (\[ 1 \] = 2) \})))
                          ((local (x : num) = (\{ (\[ 1 \] = 2) \}) in
                             hole end) |;|))) ()))
                    ((continue 1))
                    )
                   )
   #t)

  ; concat stats
  (test-equal
   (judgment-holds (basic_blocks
                    hole
                    $nopos
                    ()
                    ((if 1 then \; else \; end) \;)
                    1
                    (((1 ((((if hole then |;| else |;| end) |;|) 1)))
                      (2 3))
                     ((2 ((((if 1 then hole else |;| end) |;|) |;|))) (4))
                     ((3 ((((if 1 then |;| else hole end) |;|) |;|))) (4))
                     ((4 ((((if 1 then |;| else |;| end) hole) |;|))) ()))
                    ((continue 4))))
   #t)
  
  (test-equal
   (judgment-holds (basic_blocks
                    hole
                    $nopos
                    ()
                    (\; \;)
                    1
                    (((1 (((hole |;|) \;) ((\; hole) \;)))
                      ()))
                    ((continue 1))))
   #t)
  
  (test-equal
   (judgment-holds (basic_blocks
                    hole
                    $nopos
                    ()
                    (\; \; \;)
                    1
                    (((1 (((hole |;| \;) \;) ((\; hole \;) \;)
                                             ((\; \; hole) \;)))
                      ()))
                    ((continue 1))))
   #t)
  
  (test-equal
   (judgment-holds (basic_blocks
                    hole
                    $nopos
                    ()
                    (\; \; \; \;)
                    1
                    (((1 (((hole |;| \; \;) \;)
                          ((\; hole \; \;) \;)
                          ((\; \; hole \;) \;) ((\; \; \; hole) \;)))
                      ()))
                    ((continue 1))))
   #t)
  
    (test-equal
     (judgment-holds (basic_blocks
                      hole
                      $nopos
                      ()
                      ((while 1 do \; end) \;)
                      1
                      (((1 ((((while hole do |;| end) \;) 1)))
                        (2 3))
                       ((2 ((((while 1 do hole end) \;) |;|))) (1))
                       ((3 ((((while 1 do \; end) hole) |;|))) ()))
                      ((continue 3))))
     #t)
  
  (test-equal
   (judgment-holds (basic_blocks
                    hole
                    $nopos
                    ()
                    ((if 1 then (x = 1) else (x = 2) end)
                     (if 1 then (x = 1) else (x = 2) end) |;|)
                    1
                    (((1 ((((if hole then (x = 1) else (x = 2) end)
                            (if 1 then (x = 1) else (x = 2) end) |;|) 1)))
                      (2 3))
                     ((2
                       ((((if 1 then (hole = 1) else (x = 2) end)
                          (if 1 then (x = 1) else (x = 2) end) |;|) x)
                        (((if 1 then (x = hole) else (x = 2) end)
                          (if 1 then (x = 1) else (x = 2) end) |;|) 1)
                        (((if 1 then hole else (x = 2) end)
                          (if 1 then (x = 1) else (x = 2) end) |;|) (x = 1))))
                      (4))
                     ((3
                       ((((if 1 then (x = 1) else (hole = 2) end)
                          (if 1 then (x = 1) else (x = 2) end) |;|) x)
                        (((if 1 then (x = 1) else (x = hole) end)
                          (if 1 then (x = 1) else (x = 2) end) |;|) 2)
                        (((if 1 then (x = 1) else hole end)
                          (if 1 then (x = 1) else (x = 2) end) |;|) (x = 2))))
                      (4))
                     ((4 ((((if 1 then (x = 1) else (x = 2) end)
                            (if hole then (x = 1) else (x = 2) end) |;|) 1)))
                      (5 6))
                     ((5
                       ((((if 1 then (x = 1) else (x = 2) end)
                          (if 1 then (hole = 1) else (x = 2) end) |;|) x)
                        (((if 1 then (x = 1) else (x = 2) end)
                          (if 1 then (x = hole) else (x = 2) end) |;|) 1)
                        (((if 1 then (x = 1) else (x = 2) end)
                          (if 1 then hole else (x = 2) end) |;|) (x = 1))))
                      (7))
                     ((6
                       ((((if 1 then (x = 1) else (x = 2) end)
                          (if 1 then (x = 1) else (hole = 2) end) |;|) x)
                        (((if 1 then (x = 1) else (x = 2) end)
                          (if 1 then (x = 1) else (x = hole) end) |;|) 2)
                        (((if 1 then (x = 1) else (x = 2) end)
                          (if 1 then (x = 1) else hole end) |;|) (x = 2))))
                      (7))
                     ((7 ((((if 1 then (x = 1) else (x = 2) end)
                            (if 1 then (x = 1) else (x = 2) end) hole) |;|)))
                      ()))
                    ((continue 7))))
   #t)
  
    
  (test-results)
  )