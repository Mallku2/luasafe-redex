#lang racket

(require redex
         "../reaching_defs.rkt"
         "../data_flow_analysis.rkt")

(define (reaching-defs-test-suite)

  
  ;                                                                          
  ;                                                                          
  ;                                                                          
  ;                                       ;           ;    ;;;;     ;;;;     
  ;                                       ;           ;       ;        ;     
  ;                                       ;                   ;        ;     
  ;     ;;; ;    ;;;    ; ;;;;            ;    ;    ;;;       ;        ;     
  ;    ;   ;;   ;   ;   ;;   ;;           ;  ;;       ;       ;        ;     
  ;   ;     ;  ;     ;  ;     ;           ; ;         ;       ;        ;     
  ;   ;     ;  ;     ;  ;     ;           ;;;         ;       ;        ;     
  ;   ;     ;  ;;;;;;;  ;     ;           ;  ;        ;       ;        ;     
  ;   ;     ;  ;        ;     ;           ;   ;       ;       ;        ;     
  ;    ;   ;;   ;    ;  ;     ;           ;    ;      ;       ;        ;     
  ;     ;;; ;    ;;;;   ;     ;           ;     ;  ;;;;;;;     ;;;      ;;;  
  ;         ;                                                                
  ;    ;   ;;                                                                
  ;     ;;;;                                                                 
  ;                                                                          
  
  ; var assign
  (test-equal
   (term (compute_KG
          (build_cfg (x = 1))))
   (term (((1 (((hole = 1) x)) () ()) (2))
          ((2 (((x = hole) 1)) () ()) (3))
          ((3 ((hole (x = 1))) () ((x = 1))) ()))))

  ; first def is not downward exposeds
  (test-equal
   (term (compute_KG
          (build_cfg ((x = 1) (x = 2)))))
   (term (((1 ((((hole = 1) (x = 2)) x)) () ()) (2))
          ((2 ((((x = hole) (x = 2)) 1)) () ()) (3))
          ((3 (((hole (x = 2)) (x = 1))) ((x = 2)) ((x = 1))) (4))
          ((4 ((((x = 1) (hole = 2)) x)) () ()) (5))
          ((5 ((((x = 1) (x = hole)) 2)) () ()) (6))
          ((6 ((((x = 1) hole) (x = 2))) ((x = 1)) ((x = 2))) ()))))

  (test-equal
   (term (compute_KG
          (build_cfg ((x = 1) (y = 2)))))
   (term (((1 ((((hole = 1) (y = 2)) x)) () ()) (2))
          ((2 ((((x = hole) (y = 2)) 1)) () ()) (3))
          ((3 (((hole (y = 2)) (x = 1))) () ((x = 1))) (4))
          ((4 ((((x = 1) (hole = 2)) y)) () ()) (5))
          ((5 ((((x = 1) (y = hole)) 2)) () ()) (6))
          ((6 ((((x = 1) hole) (y = 2))) () ((y = 2))) ()))))

  ; some kills
  (test-equal
   (term (compute_KG
          (build_cfg (if 1 then ((x = 1) (y = 2)) else ((x = 2) (y = 3)) end))))
   (term (((3 (((if hole then ((x = 1) (y = 2)) else ((x = 2) (y = 3)) end) 1))
              () ()) (4 10))
          ((4 (((if 1 then ((hole = 1) (y = 2)) else ((x = 2) (y = 3)) end) x))
              () ()) (5))
          ((5 (((if 1 then ((x = hole) (y = 2)) else ((x = 2) (y = 3)) end) 1))
              () ()) (6))
          ((6 (((if 1 then (hole (y = 2)) else ((x = 2) (y = 3)) end) (x = 1)))
              ((x = 2)) ((x = 1))) (7))
          ((7 (((if 1 then ((x = 1) (hole = 2)) else ((x = 2) (y = 3)) end) y))
              () ()) (8))
          ((8 (((if 1 then ((x = 1) (y = hole)) else ((x = 2) (y = 3)) end) 2))
              () ()) (9))
          ((9 (((if 1 then ((x = 1) hole) else ((x = 2) (y = 3)) end) (y = 2)))
               ((y = 3)) ((y = 2))) ())
          ((10 (((if 1 then ((x = 1) (y = 2)) else ((hole = 2) (y = 3)) end) x))
               () ()) (11))
          ((11 (((if 1 then ((x = 1) (y = 2)) else ((x = hole) (y = 3)) end) 2))
               () ()) (12))
          ((12 (((if 1 then ((x = 1) (y = 2)) else (hole (y = 3)) end) (x = 2)))
               ((x = 1)) ((x = 2))) (13))
          ((13 (((if 1 then ((x = 1) (y = 2)) else ((x = 2) (hole = 3)) end) y))
               () ()) (14))
          ((14 (((if 1 then ((x = 1) (y = 2)) else ((x = 2) (y = hole)) end) 3))
               () ()) (15))
          ((15 (((if 1 then ((x = 1) (y = 2)) else ((x = 2) hole) end) (y = 3)))
               ((y = 2)) ((y = 3))) ()))))

  ; local vars
  (test-equal
   (term (compute_KG
          (build_cfg (local (x : num) = 1 in \; end))))
   (term (((1 (((local (x : num) = hole in |;| end) 1)) () ()) (2))
          ((2 (((local hole in |;| end) (x = 1))) () ((x = 1))) (3))
          ((3 (((local (x : num) = 1 in hole end) |;|)) () ()) ()))))

  (test-equal
   (term (compute_KG
          (build_cfg (local (x : num) = 1 in
                       (local (x : num) = 2 in \; end) end))))
   (term (((1 (((local (x : num) = hole in
                  (local (x : num) = 2 in |;| end) end) 1)) () ()) (2))
          ((2 (((local hole in
                  (local (x : num) = 2 in |;| end) end) (x = 1)))
              ((x = 2)) ((x = 1))) (3))
          ((3 (((local (x : num) = 1 in
                  (local (x : num) = hole in |;| end) end) 2)) () ()) (4))
          ((4 (((local (x : num) = 1 in
                  (local hole in |;| end) end) (x = 2)))
              ((x = 1)) ((x = 2))) (5))
          ((5 (((local (x : num) = 1 in
                  (local (x : num) = 2 in hole end) end) |;|)) () ()) ()))))

  
  ;                                                                                            
  ;                                                                                            
  ;                                                                                            
  ;                                       ;                       ;               ;;;          
  ;                                       ;                       ;              ;             
  ;                                       ;                       ;              ;             
  ;     ; ;;;    ;;;      ;;;;     ;;;    ; ;;;;              ;;; ;    ;;;     ;;;;;;   ;;;;;  
  ;     ;;   ;  ;   ;    ;    ;   ;   ;   ;;   ;;            ;   ;;   ;   ;      ;     ;     ; 
  ;     ;      ;     ;        ;  ;        ;     ;           ;     ;  ;     ;     ;     ;       
  ;     ;      ;     ;   ;;;;;;  ;        ;     ;           ;     ;  ;     ;     ;     ;;;;    
  ;     ;      ;;;;;;;  ;;    ;  ;        ;     ;           ;     ;  ;;;;;;;     ;         ;;; 
  ;     ;      ;        ;     ;  ;        ;     ;           ;     ;  ;           ;           ; 
  ;     ;       ;    ;  ;    ;;   ;   ;   ;     ;            ;   ;;   ;    ;     ;     ;     ; 
  ;     ;        ;;;;    ;;;; ;    ;;;    ;     ;             ;;; ;    ;;;;      ;      ;;;;;  
  ;                                                                                            
  ;                                                                                            
  ;                                                                                            
  ;                                                                                            

  (test-equal
   (term (reach_defs ((if 1 then (x = 1) else (x = 2) end) |;|)))
   (term (((4 ((((if hole then (x = 1) else (x = 2) end) |;|) 1)) () ()) (5 8))
          ((5 ((((if 1 then (hole = 1) else (x = 2) end) |;|) x)) () ()) (6))
          ((6 ((((if 1 then (x = hole) else (x = 2) end) |;|) 1)) () ()) (7))
          ((7 ((((if 1 then hole else (x = 2) end) |;|) (x = 1))) () ((x = 1)))
           (11))
          ((8 ((((if 1 then (x = 1) else (hole = 2) end) |;|) x)) () ()) (9))
          ((9 ((((if 1 then (x = 1) else (x = hole) end) |;|) 2)) () ()) (10))
          ((10 ((((if 1 then (x = 1) else hole end) |;|) (x = 2))) () ((x = 2)))
           (11))
          ((11 ((((if 1 then (x = 1) else (x = 2) end) hole) |;|))
               ((x = 1) (x = 2)) ((x = 1) (x = 2))) ()))))

  ; jumps after var def
  (test-equal
   (term (reach_defs ((while 1 do ((x = 1) break) end) \;)))
   (term (((3 ((((while hole do ((x = 1) break) end) |;|) 1)) () ()) (4 8))
          ((4 ((((while 1 do ((hole = 1) break) end) |;|) x)) () ()) (5))
          ((5 ((((while 1 do ((x = hole) break) end) |;|) 1)) () ()) (6))
          ((6 ((((while 1 do (hole break) end) |;|) (x = 1))) () ((x = 1)))
           (7))
          ((7 ((((while 1 do ((x = 1) hole) end) |;|) break))
              ((x = 1)) ((x = 1))) (8))
          ((8 ((((while 1 do ((x = 1) break) end) hole) |;|))
              ((x = 1)) ((x = 1))) ()))))

  (test-equal
   (term (reach_defs ((while 1 do ((if 1 then ((x = 1) break) else (x = 2)
                                       end) \;) end) \;)))
   (term (((6 ((((while hole do ((if 1 then ((x = 1) break) else (x = 2) end)
                                 |;|) end) |;|) 1)) () ()) (7 16))
          ((7 ((((while 1 do ((if hole then ((x = 1) break) else (x = 2) end)
                              |;|) end) |;|) 1)) () ()) (8 12))
          ((8 ((((while 1 do ((if 1 then ((hole = 1) break) else (x = 2) end)
                              |;|) end) |;|) x)) () ()) (9))
          ((9 ((((while 1 do ((if 1 then ((x = hole) break) else (x = 2) end)
                               |;|) end) |;|) 1)) () ()) (10))
          ((10 ((((while 1 do ((if 1 then (hole break) else (x = 2) end) |;|)
                         end) |;|) (x = 1))) () ((x = 1))) (11))
          ((11 ((((while 1 do ((if 1 then ((x = 1) hole) else (x = 2) end) |;|)
                         end) |;|) break)) ((x = 1)) ((x = 1))) (16))
          ((12 ((((while 1 do ((if 1 then ((x = 1) break) else (hole = 2) end)
                               |;|) end) |;|) x)) () ()) (13))
          ((13 ((((while 1 do ((if 1 then ((x = 1) break) else (x = hole) end)
                               |;|) end) |;|) 2)) () ()) (14))
          ((14 ((((while 1 do ((if 1 then ((x = 1) break) else hole end) |;|)
                         end) |;|) (x = 2))) () ((x = 2))) (15))
          ((15 ((((while 1 do ((if 1 then ((x = 1) break) else (x = 2) end)
                               hole) end) |;|) |;|)) ((x = 2)) ((x = 2))) (16))
          ((16 ((((while 1 do ((if 1 then ((x = 1) break) else (x = 2) end)
                               |;|) end) hole) |;|))
               ((x = 1) (x = 2)) ((x = 1) (x = 2))) ()))
         ))

  ; var assign
  (test-equal
   (term (reach_defs (x = 1 2)))
   (term (((1 (((hole = 1 2) x)) () ()) (2))
          ((2 (((x = hole 2) 1)) () ()) (3))
          ((3 (((x = 1 hole) 2)) () ()) (4))
          ((4 ((hole (x = 1 2))) () ((x = 1))) ()))))

  (test-equal
   (term (reach_defs (x y = 1 2)))
   (term (((1 (((hole y = 1 2) x)) () ()) (2))
          ((2 (((x hole = 1 2) y)) () ()) (3))
          ((3 (((x y = hole 2) 1)) () ()) (4))
          ((4 (((x y = 1 hole) 2)) () ()) (5))
          ((5 ((hole (x y = 1 2))) () ((x = 1) (y = 2))) ()))))

  (test-equal
   (term (reach_defs (x y z = 1 2)))
   (term (((1 (((hole y z = 1 2) x)) () ()) (2))
          ((2 (((x hole z = 1 2) y)) () ()) (3))
          ((3 (((x y hole = 1 2) z)) () ()) (4))
          ((4 (((x y z = hole 2) 1)) () ()) (5))
          ((5 (((x y z = 1 hole) 2)) () ()) (6))
          ((6 ((hole (x y z = 1 2))) () ((x = 1) (y = 2) (z = nil))) ()))))

  (test-equal
   (term (reach_defs (x y z = 1 2 3)))
   (term (((1 (((hole y z = 1 2 3) x)) () ()) (2))
          ((2 (((x hole z = 1 2 3) y)) () ()) (3))
          ((3 (((x y hole = 1 2 3) z)) () ()) (4))
          ((4 (((x y z = hole 2 3) 1)) () ()) (5))
          ((5 (((x y z = 1 hole 3) 2)) () ()) (6))
          ((6 (((x y z = 1 2 hole) 3)) () ()) (7))
          ((7 ((hole (x y z = 1 2 3))) () ((x = 1) (y = 2) (z = 3))) ()))))

  ; loc var
  (test-equal
   (term (reach_defs (local (x : num) (y : num) = 1 2 in \; end)))
   (term (((1 (((local (x : num) (y : num) = hole 2 in |;| end) 1)) () ()) (2))
          ((2 (((local (x : num) (y : num) = 1 hole in |;| end) 2)) () ()) (3))
          ((3 (((local hole in |;| end) (x y = 1 2))) () ((x = 1) (y = 2))) (4))
          ((4 (((local (x : num) (y : num) = 1 2 in hole end) |;|))
              ((x = 1) (y = 2)) ((x = 1) (y = 2))) ()))))

  (test-equal
   (term (reach_defs (local (x : num) (y : num) (z : num) = 1 2 in \; end)))
   (term (((1 (((local (x : num) (y : num) (z : num) = hole 2 in |;| end) 1))
              () ()) (2))
          ((2 (((local (x : num) (y : num) (z : num) = 1 hole in |;| end) 2))
              () ()) (3))
          ((3 (((local hole in |;| end) (x y z = 1 2)))
              () ((x = 1) (y = 2) (z = nil))) (4))
          ((4 (((local (x : num) (y : num) (z : num) = 1 2 in hole end) |;|))
              ((x = 1) (y = 2) (z = nil)) ((x = 1) (y = 2) (z = nil))) ()))))

  ; variables out of scope
  ; x out of scope, not bound to a cte
  (test-equal
   (term (reach_defs ((local (x : num) = 1 in (z = x) end) \;)))
   (term  (((1 ((((local (x : num) = hole in (z = x) end) |;|) 1)) () ())
            (2))
           ((2 ((((local hole in (z = x) end) |;|) (x = 1))) () ((x = 1)))
            (3))
           ((3 ((((local (x : num) = 1 in (hole = x) end) |;|) z)) ((x = 1))
               ((x = 1)))
            (4))
           ((4 ((((local (x : num) = 1 in (z = hole) end) |;|) x)) ((x = 1))
               ((x = 1)))
            (5))
           ((5 ((((local (x : num) = 1 in hole end) |;|) (z = x))) ((x = 1))
               ((z = x) (x = 1)))
            (6))
           ((6 ((((local (x : num) = 1 in (z = x) end) hole) |;|)) ((z = x))
               ((z = x)))
            ()))))

  ; x out of scope, bound to a cte
  (test-equal
   (term (reach_defs ((local (x : ((\{ \}) strong)) = (\{ \}) in (z = x) end)
                      \;)))
   (term  (((1 ((((local (x : ((|{| |}|) strong)) = hole
                    in (z = x) end) |;|)
                 (|{| |}|))) () ()) (2))
           ((2
             ((((local hole in (z = x) end) |;|) (x = (|{| |}|)))) ()
             ((x = (|{| |}|))))
            (3))
           ((3 ((((local (x : ((|{| |}|) strong)) = (|{| |}|)
                    in
                    (hole = x)
                    end)
                  |;|)
                 z)) ((x = (|{| |}|))) ((x = (|{| |}|))))
            (4))
           ((4 ((((local (x : ((|{| |}|) strong)) = (|{| |}|) in (z = hole)
                    end) |;|) x)) ((x = (|{| |}|))) ((x = (|{| |}|))))
            (5))
           ((5
             ((((local (x : ((|{| |}|) strong)) = (|{| |}|) in hole end)
                |;|) (z = x))) ((x = (|{| |}|)))
                               ((z = x) (x = (|{| |}|))))
            (6))
           ((6
             ((((local (x : ((|{| |}|) strong)) = (|{| |}|) in (z = x)
                  end)
                hole)
               |;|))
             ((z = x) (x = (|{| |}|)))
             ((z = x) (x = (|{| |}|))))
            ()))))
  
  
  
  (test-equal
   (term (reach_defs (local
                       ($ENV : (1 : num))
                       = 1
                       in
                       (local (t1 : (1.0 : num)) = 1.0 in
                         (while true do |;| end) end)
                       end)))
   (term (((2 (((local ($ENV : (1 : num)) = hole in
                  (local (t1 : (1.0 : num)) = 1.0 in
                    (while true do |;| end) end) end) 1))
              (($ENV = 1)) (($ENV = 1))) (3))
          ((3 (((local hole in (local (t1 : (1.0 : num)) = 1.0
                                 in (while true do |;| end) end) end)
                ($ENV = 1))) (($ENV = 1)) (($ENV = 1))) (4))
          ((4 (((local ($ENV : (1 : num)) = 1 in
                  (local (t1 : (1.0 : num)) = hole in
                    (while true do |;| end) end) end) 1.0))
              (($ENV = 1)) (($ENV = 1))) (5))
          ((5 (((local ($ENV : (1 : num)) = 1 in
                  (local hole in (while true do |;| end) end) end) (t1 = 1.0)))
              (($ENV = 1)) ((t1 = 1.0) ($ENV = 1))) (6))
          ((6
            (((local ($ENV : (1 : num)) = 1 in
                (local (t1 : (1.0 : num)) = 1.0 in (while hole do |;| end) end)
                end) true))
            ((t1 = 1.0) ($ENV = 1))
            ((t1 = 1.0) ($ENV = 1)))
           (7))
          ((7
            (((local ($ENV : (1 : num)) = 1 in
                (local (t1 : (1.0 : num)) = 1.0 in
                  (while true do hole end) end) end) |;|))
            ((t1 = 1.0) ($ENV = 1))
            ((t1 = 1.0) ($ENV = 1)))
           (2)))))
  
  (test-results)
  )