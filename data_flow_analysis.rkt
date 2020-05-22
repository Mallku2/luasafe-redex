#lang racket

(require redex
         "./grammar.rkt"
         "./typing_lang_theory.rkt")

(define-extended-language lang-data-flow ext-lang-typed

  ; control flow graph rep.
  [nodeId C]
  
  [node (nodeId aterm)
        ; special nodes: entry and exit nodes
        entry
        exit]

  [bblockid number]

  ; id - nodes of the block 
  [bblock (bblockid (node ...))]

  ; ids of basic blocks to which this block points
  [cfg ((bblock (bblockid ...)) ...)]

  ; control flow info
  [ctrlfinfo ; jumps
   ; jump, from bblockid to the point indicated by C
   (bblockid C) 
   ; continue from bblockid to the next instruction, that
   ; should be in a new basic block (add an edge from bblockid to the
   ; next basic block)
   (newbblock bblockid)
             
   ; continue adding instructions in the same bblock
   (continue bblockid)
   ]
  )

(provide lang-data-flow)

;                             
;                             
;                             
;                             
;                             
;                             
;     ;;;;   ;     ;  ;;   ;; 
;    ;    ;  ;     ;   ;   ;  
;         ;  ;     ;    ; ;   
;    ;;;;;;  ;     ;     ;    
;   ;;    ;  ;     ;     ;    
;   ;     ;  ;     ;    ; ;   
;   ;    ;;  ;;   ;;   ;   ;  
;    ;;;; ;   ;;;; ;  ;;   ;; 
;                             
;                             
;                             
;                             

; functions to manipulate cfg

; constructs the cfg for a given aterm
(define-metafunction lang-data-flow
  build_cfg : aterm -> cfg
  
  [(build_cfg aterm)
   cfg

   (where ((cfg (ctrlfinfo ...)))
          ,(judgment-holds (basic_blocks
                            hole
                            (;((1 (entry)) ())
                             )
                            aterm
                            1 cfg (ctrlfinfo ...))
                           (cfg (ctrlfinfo ...))
                           ))
   ])

(provide build_cfg)

; add a new node to a given bblock, identified by its bblockid 
(define-metafunction lang-data-flow

  add_node_bblock : cfg bblockid node -> cfg

  [(add_node_bblock ((bblock_1 (bblockid_1 ...)) ...
                     ((bblockid_2 (node_1 ...)) (bblockid_3 ...))
                     (bblock_2 (bblockid_4 ...)) ...)
                    bblockid_2
                    node_2)
   
   ((bblock_1 (bblockid_1 ...)) ...
    ((bblockid_2 (node_1 ... node_2)) (bblockid_3 ...))
    (bblock_2 (bblockid_4 ...)) ...)]

  [(add_node_bblock ()
                    bblockid
                    node)
   
   (((bblockid (node)) ()))]
  )

; adds an edge from a given bblock to another bblock.
; PARAMS:
; cfg : the original cfg
; (ctrlinfo ...): the origin of the edges are identified by ctrlfinfo
; (bblockid ...) and (C ...): The end is identified by the block id and a
; context C that specifies the position into the program of the origin of the
; block
; RETURNS:
; a new cfg with the added edges
(define-metafunction lang-data-flow

  add_bblock_edge : cfg (ctrlfinfo ...) (bblockid ...)
  ;TODO: cannot specify (C ...)
  any -> cfg

  [(add_bblock_edge cfg (ctrlfinfo ...) () ())
   cfg
   ]

  [(add_bblock_edge cfg_1 (ctrlfinfo ...) (bblockid_1 bblockid_2 ...)
                    (C_1 C_2 ...))

   (add_bblock_edge cfg_2 (ctrlfinfo ...) (bblockid_2 ...)
                    (C_2 ...))

   (where cfg_2 (add_bblock_edge_aux cfg_1 (ctrlfinfo ...) bblockid_1
                                     C_1))]
  )


(define-metafunction lang-data-flow

  add_bblock_edge_aux : cfg (ctrlfinfo ...) bblockid C -> cfg

  [(add_bblock_edge_aux cfg () bblockid C)
   cfg]

  [(add_bblock_edge_aux ((bblock_1 (bblockid_1 ...)) ...
                         ((bblockid_2 (node ...)) (bblockid_3 ...))
                         (bblock_2 (bblockid_4 ...)) ...)
                        
                        ((newbblock bblockid_2) ctrlfinfo ...)

                        bblockid_5 C)
   
   (add_bblock_edge_aux ((bblock_1 (bblockid_1 ...)) ...
                         ((bblockid_2 (node ...)) (bblockid_3 ... bblockid_5))
                         (bblock_2 (bblockid_4 ...)) ...)

                        (ctrlfinfo ...)

                        bblockid_5 C)]

  [(add_bblock_edge_aux ((bblock_1 (bblockid_1 ...)) ...
                         ((bblockid_2 (node ...)) (bblockid_3 ...))
                         (bblock_2 (bblockid_4 ...)) ...)
                        ((bblockid_2 C) ctrlfinfo ...)
                        bblockid_5
                        C)

   (add_bblock_edge_aux ((bblock_1 (bblockid_1 ...)) ...
                         ((bblockid_2 (node ...)) (bblockid_3 ... bblockid_5))
                         (bblock_2 (bblockid_4 ...)) ...)
                        (ctrlfinfo ...)
                        bblockid_5
                        C)]

  [(add_bblock_edge_aux cfg (ctrlfinfo_1 ctrlfinfo_2 ...)
                        bblockid C)
   
   (add_bblock_edge_aux cfg (ctrlfinfo_2 ...) bblockid C)]
  )

; add new bblock to a given cfg
(define-metafunction lang-data-flow

  add_bblock : cfg bblock -> cfg

  [(add_bblock ((bblock_1 (bblockid_1 ...)) ...)
               bblock_2)
   ((bblock_1 (bblockid_1 ...)) ... (bblock_2 ()))]
  )

; functions to manipulate execution flow info

; converts (continue bblockid) of (newbblock bblockid) into a jump of the form
; (bblockid C), for a given context C that points to the beginning of a loop
(define-metafunction lang-data-flow

  convert_into_loop : (ctrlfinfo ...) C any -> (ctrlfinfo ...)

  [(convert_into_loop () _ _)
   ()]

  [(convert_into_loop ((continue bblockid) ctrlfinfo_1 ...)
                      C (while e do s end))
   ((bblockid (in-hole C (while hole do s end))) ctrlfinfo_2 ...)

   (where (ctrlfinfo_2 ...) (convert_into_loop
                             (ctrlfinfo_1 ...)
                             C (while e do s end)))]

  [(convert_into_loop ((newbblock bblockid) ctrlfinfo_1 ...)
                      C (while e do s end))
   ((bblockid (in-hole C (while hole do s end))) ctrlfinfo_2 ...)

   (where (ctrlfinfo_2 ...) (convert_into_loop
                             (ctrlfinfo_1 ...)
                             C (while e do s end)))]

  ; TODO: ugly fix, to manage breaks present in nested loops that jumps into
  ; the body of a loop
  ; convert (bblockid ((in-hole C (while e do hole end)))) into jumps to the
  ; guard: (bblockid C)
  [(convert_into_loop ((bblockid (in-hole C (while e do hole end)))
                       ctrlfinfo_1 ...) C (while e do s end))
   ((bblockid (in-hole C (while hole do s end))) ctrlfinfo_2 ...)

   (where (ctrlfinfo_2 ...) (convert_into_loop
                             (ctrlfinfo_1 ...)
                             C (while e do s end)))]
  

  [(convert_into_loop ((bblockid C_1) ctrlfinfo_1 ...) C_2 (while e do s end))
   ((bblockid C_1) ctrlfinfo_2 ...)

   (where (ctrlfinfo_2 ...) (convert_into_loop (ctrlfinfo_1 ...) C_2
                                               (while e do s end)))]
  )


; onverts (continue bblockid) into a jump to the following basic block:
; (newbblock bblockid)
(define-metafunction lang-data-flow
  add_jump_next_bblock : (ctrlfinfo ...) -> (ctrlfinfo ...)

  [(add_jump_next_bblock ())
   ()]

  [(add_jump_next_bblock ((continue bblockid) ctrlfinfo_1 ...))
   ((newbblock bblockid) ctrlfinfo_2 ...)

   (where (ctrlfinfo_2 ...) (add_jump_next_bblock (ctrlfinfo_1 ...)))]

  [(add_jump_next_bblock (ctrlfinfo_1 ctrlfinfo_2 ...))
   (ctrlfinfo_1 ctrlfinfo_3 ...)

   (where (ctrlfinfo_3 ...) (add_jump_next_bblock (ctrlfinfo_2 ...)))]
  )

; extract jumps expressed by a (bblockid_1 C_1) ctrlfinfo that is not referred to
; a given point into the program, indicated by a given context C_2
(define-metafunction lang-data-flow
  filter_ctrlfinfo : (ctrlfinfo ...) C -> ((bblockid C) ...)

  [(filter_ctrlfinfo () C)
   ()]
  
  ; ctrlfinfo refers to the point indicated by C 
  [(filter_ctrlfinfo ((bblockid C) ctrlfinfo ...) C)
   (filter_ctrlfinfo (ctrlfinfo ...) C)]

  ; ctrlfinfo doesn't refer to the point indicated by C
  [(filter_ctrlfinfo ((bblockid_1 C_1) ctrlfinfo ...) C_2)
   ((bblockid_1 C_1) (bblockid_2 C_3) ...)

   (where ((bblockid_2 C_3) ...) (filter_ctrlfinfo (ctrlfinfo ...) C_2))]

  ; not a jump
  [(filter_ctrlfinfo (ctrlfinfo_1 ctrlfinfo_2 ...) C)
   (filter_ctrlfinfo (ctrlfinfo_2 ...) C)])

; extract jumps (ctrlfinfo like (bblockid C))
(define-metafunction lang-data-flow
  filter_bblockid_ctrlfinfo : (ctrlfinfo ...) -> ((bblockid C) ...)

  [(filter_bblockid_ctrlfinfo ())
   ()]

  ; ctrlfinfo doesn't refer to the point indicated by C
  [(filter_bblockid_ctrlfinfo ((bblockid_1 C_1) ctrlfinfo ...))
   ((bblockid_1 C_1) (bblockid_2 C_2) ...)

   (where ((bblockid_2 C_2) ...) (filter_bblockid_ctrlfinfo (ctrlfinfo ...)))]

  
  ; not a jump
  [(filter_bblockid_ctrlfinfo (ctrlfinfo_1 ctrlfinfo_2 ...))
   (filter_bblockid_ctrlfinfo (ctrlfinfo_2 ...))])

; decompose multiple var. assignment into multiple single var assign.
(define-metafunction lang-data-flow
  decompose_mult_assign : (var ... = e ...) -> ((var = e) ...)

  [(decompose_mult_assign ())
   ()]

  [(decompose_mult_assign (var_1 ... var_2 = e_1 ... e_2))
   ((var_2 = e_2) (var_3 = e_3) ...)

   (where ((var_3 = e_3) ...) (decompose_mult_assign (var_1 ... = e_1 ...)))]
  )

;                             
;                             
;                             
;                ;;;          
;               ;             
;               ;             
;     ;;;     ;;;;;;    ;;; ; 
;    ;   ;      ;      ;   ;; 
;   ;           ;     ;     ; 
;   ;           ;     ;     ; 
;   ;           ;     ;     ; 
;   ;           ;     ;     ; 
;    ;   ;      ;      ;   ;; 
;     ;;;       ;       ;;; ; 
;                           ; 
;                      ;   ;; 
;                       ;;;;  
;                             

; list of expressions of fun calls
(define-judgment-form
  lang-data-flow
  #:mode (basic_blocks_fun_call_param I I I I I O O)
  #:contract (basic_blocks_fun_call_param C C cfg e bblockid cfg
                                          (ctrlfinfo ...))

  [(basic_blocks (in-hole C_1 (prefixexp (e_2 ... hole))) cfg_1 e_3 bblockid
                 cfg_2 (ctrlfinfo ...))
   --------------------------------------------------------------------
   (basic_blocks_fun_call_param C_1 (prefixexp (e_2 ... hole)) cfg_1
                                e_3 bblockid cfg_2 (ctrlfinfo ...))]

  [(basic_blocks (in-hole C_1 (prefixexp (e_2 ... hole e_3 e_4 ...))) cfg_1 
                 e_5 bblockid cfg_2 (ctrlfinfo_1 ...))

   (basic_blocks_fun_call_param C_1 (prefixexp (e_2 ... e_5 hole e_4 ...)) cfg_2
                                e_3 bblockid cfg_3 (ctrlfinfo_2 ...))
   --------------------------------------------------------------------
   (basic_blocks_fun_call_param C_1 (prefixexp (e_2 ... hole e_3 e_4 ...)) cfg_1
                                e_5 bblockid cfg_3 (ctrlfinfo_1 ...
                                                    ctrlfinfo_2 ...))]
  
  )

; list of expressions of return
(define-judgment-form
  lang-data-flow
  #:mode (basic_blocks_return I I I I I O O)
  #:contract (basic_blocks_return C C cfg e bblockid cfg
                                  (ctrlfinfo ...))

  [(basic_blocks (in-hole C_1 (return e_2 ... hole)) cfg_1 e_3 bblockid
                 cfg_2 (ctrlfinfo ...))
   --------------------------------------------------------------------
   (basic_blocks_return C_1 (return e_2 ... hole) cfg_1
                        e_3 bblockid cfg_2 (ctrlfinfo ...))]

  [(basic_blocks (in-hole C_1 (return e_2 ... hole e_3 e_4 ...)) cfg_1 
                 e_5 bblockid cfg_2 (ctrlfinfo_1 ...))

   (basic_blocks_return C_1 (return e_2 ... e_5 hole e_4 ...) cfg_2
                        e_3 bblockid cfg_3 (ctrlfinfo_2 ...))
   --------------------------------------------------------------------
   (basic_blocks_return C_1 (return e_2 ... hole e_3 e_4 ...) cfg_1
                        e_5 bblockid cfg_3 (ctrlfinfo_1 ...
                                            ctrlfinfo_2 ...))]
  
  )

; list of lvalues and rvalues of assignments
(define-judgment-form
  lang-data-flow
  #:mode (basic_blocks_assign I I I I I O O)
  #:contract (basic_blocks_assign C C cfg e bblockid cfg
                                  (ctrlfinfo ...))

  ; last exp
  [(basic_blocks (in-hole C_1 (var ... = e_1 ... hole)) cfg_1 e_2 bblockid
                 cfg_2 (ctrlfinfo ...))
   --------------------------------------------------------------------
   (basic_blocks_assign C_1 (var ... = e_1 ... hole) cfg_1
                        e_2 bblockid cfg_2 (ctrlfinfo ...))]

  ; not the last exp
  [(basic_blocks (in-hole C_1 (var ... = e_1 ... hole e_2 e_3 ...)) cfg_1 
                 e_4 bblockid cfg_2 (ctrlfinfo_1 ...))

   (basic_blocks_assign C_1 (var ... = e_1 ... e_4 hole e_3 ...) cfg_2
                        e_2 bblockid cfg_3 (ctrlfinfo_2 ...))
   --------------------------------------------------------------------
   (basic_blocks_assign C_1 (var ... = e_1 ... hole e_2 e_3 ...) cfg_1
                        e_4 bblockid cfg_3 (ctrlfinfo_1 ...
                                            ctrlfinfo_2 ...))]

  ; var, not the last one
  [(basic_blocks (in-hole C_1 (var_1 ... hole var_2 var_3 ... = e ...)) cfg_1 
                 var_4 bblockid cfg_2 (ctrlfinfo_1 ...))

   (basic_blocks_assign C_1 (var_1 ... var_4 hole var_3 ... = e ...) cfg_2
                        var_2 bblockid cfg_3 (ctrlfinfo_2 ...))
   --------------------------------------------------------------------
   (basic_blocks_assign C_1 (var_1 ... hole var_2 var_3 ... = e ...) cfg_1
                        var_4 bblockid cfg_3 (ctrlfinfo_1 ...
                                              ctrlfinfo_2 ...))]

  ; last var
  [(basic_blocks (in-hole C_1 (var_1 ... hole = e_1 e_2 ...)) cfg_1 
                 var_2 bblockid cfg_2 (ctrlfinfo_1 ...))

   (basic_blocks_assign C_1 (var_1 ... var_2 = hole e_2 ...) cfg_2
                        e_1 bblockid cfg_3 (ctrlfinfo_2 ...))
   --------------------------------------------------------------------
   (basic_blocks_assign C_1 (var_1 ... hole = e_1 e_2 ...) cfg_1
                        var_2 bblockid cfg_3 (ctrlfinfo_1 ...
                                              ctrlfinfo_2 ...))]
  
  )

; list of lvalues and rvalues of assignments
(define-judgment-form
  lang-data-flow
  #:mode (basic_blocks_loc_var I I I I I O O)
  #:contract (basic_blocks_loc_var C C cfg e bblockid cfg
                                   (ctrlfinfo ...))

  ; last exp
  [(basic_blocks (in-hole C_1 (local (Name : t) ... = e_1 ... hole in s end))
                 cfg_1 e_2 bblockid
                 cfg_2 (ctrlfinfo_1 ...))

   ; new node
   (where node ((in-hole C_1 (local hole in s end)) (Name ... = e_1 ... e_2)))

   ; update cfg
   (where cfg_3 (add_node_bblock cfg_2 bblockid node))
   
   ; body
   (basic_blocks (in-hole C_1 (local (Name : t) ... = e_1 ... e_2 in hole end))
                 cfg_3 s bblockid
                 cfg_4 (ctrlfinfo_2 ...))
   --------------------------------------------------------------------
   (basic_blocks_loc_var C_1 (local (Name : t) ... = e_1 ... hole in s end)
                         cfg_1 e_2 bblockid cfg_4 (ctrlfinfo_1 ...
                                                   ctrlfinfo_2 ...))]

  ; not the last exp
  [(basic_blocks (in-hole C_1 (local (Name : t) ... = e_1 ... hole e_2 e_3 ...
                                in s end)) cfg_1 
                 e_4 bblockid cfg_2 (ctrlfinfo_1 ...))

   (basic_blocks_loc_var C_1 (local (Name : t) ... = e_1 ... e_4 hole e_3 ...
                                in s end) cfg_2
                        e_2 bblockid cfg_3 (ctrlfinfo_2 ...))
   --------------------------------------------------------------------
   (basic_blocks_loc_var C_1 (local (Name : t) ... = e_1 ... hole e_2 e_3 ...
                                in s end) cfg_1
                        e_4 bblockid cfg_3 (ctrlfinfo_1 ...
                                            ctrlfinfo_2 ...))]
  
  )

; params in stat. fun. call
(define-judgment-form
  lang-data-flow
  #:mode (basic_blocks_stat_fun_call_param I I I I I O O)
  #:contract (basic_blocks_stat_fun_call_param C C cfg e bblockid cfg
                                               (ctrlfinfo ...))

  [(basic_blocks (in-hole C_1 ($statFunCall prefixexp (e_2 ... hole))) cfg_1 e_3
                 bblockid cfg_2 (ctrlfinfo ...))
   --------------------------------------------------------------------
   (basic_blocks_stat_fun_call_param C_1 ($statFunCall prefixexp (e_2 ... hole))
                                     cfg_1
                                     e_3 bblockid cfg_2 (ctrlfinfo ...))]

  [(basic_blocks (in-hole C_1 ($statFunCall prefixexp
                                            (e_2 ... hole e_3 e_4 ...)))
                 cfg_1 
                 e_5 bblockid cfg_2 (ctrlfinfo_1 ...))

   (basic_blocks_stat_fun_call_param C_1
                                     ($statFunCall prefixexp
                                                   (e_2 ... e_5 hole e_4 ...))
                                     cfg_2
                                     e_3 bblockid cfg_3 (ctrlfinfo_2 ...))
   --------------------------------------------------------------------
   (basic_blocks_stat_fun_call_param C_1
                                     ($statFunCall prefixexp
                                                   (e_2 ... hole e_3 e_4 ...))
                                     cfg_1
                                     e_5 bblockid cfg_3 (ctrlfinfo_1 ...
                                                         ctrlfinfo_2 ...))]
  )

; params in stat. fun. call
(define-judgment-form
  lang-data-flow
  #:mode (basic_blocks_stat_method_call_param I I I I I O O)
  #:contract (basic_blocks_stat_method_call_param C C cfg e bblockid cfg
                                                  (ctrlfinfo ...))

  [(basic_blocks (in-hole C_1 ($statFunCall prefixexp : Name (e_2 ... hole)))
                 cfg_1 e_3
                 bblockid cfg_2 (ctrlfinfo ...))
   --------------------------------------------------------------------
   (basic_blocks_stat_method_call_param C_1
                                        ($statFunCall prefixexp : Name
                                                      (e_2 ... hole))
                                        cfg_1
                                        e_3 bblockid cfg_2 (ctrlfinfo ...))]

  [(basic_blocks (in-hole C_1 ($statFunCall prefixexp : Name 
                                            (e_2 ... hole e_3 e_4 ...)))
                 cfg_1 
                 e_5 bblockid cfg_2 (ctrlfinfo_1 ...))

   (basic_blocks_stat_method_call_param
    C_1
    ($statFunCall prefixexp : Name 
                  (e_2 ... e_5 hole e_4 ...))
    cfg_2
    e_3 bblockid cfg_3 (ctrlfinfo_2 ...))
   --------------------------------------------------------------------
   (basic_blocks_stat_method_call_param C_1
                                        ($statFunCall prefixexp : Name 
                                                      (e_2 ... hole e_3 e_4 ...))
                                        cfg_1
                                        e_5 bblockid cfg_3 (ctrlfinfo_1 ...
                                                            ctrlfinfo_2 ...))]
  )


; list of parameters in method calls
; TODO: any way of abstracting this pattern into a single formal system?
(define-judgment-form
  lang-data-flow
  #:mode (basic_blocks_method_call_param I I I I I O O)
  #:contract (basic_blocks_method_call_param C C cfg e bblockid cfg
                                             (ctrlfinfo ...))

  [(basic_blocks (in-hole C_1 (prefixexp : Name (e_2 ... hole))) cfg_1 e_3
                 bblockid
                 cfg_2 (ctrlfinfo ...))
   --------------------------------------------------------------------
   (basic_blocks_method_call_param C_1 (prefixexp : Name (e_2 ... hole)) cfg_1
                                   e_3 bblockid cfg_2 (ctrlfinfo ...))]

  [(basic_blocks (in-hole C_1 (prefixexp : Name (e_2 ... hole e_3 e_4 ...)))
                 cfg_1 e_5 bblockid cfg_2 (ctrlfinfo_1 ...))

   (basic_blocks_method_call_param
    C_1 (prefixexp : Name (e_2 ... e_5 hole e_4 ...)) cfg_2 e_3 bblockid cfg_3
    (ctrlfinfo_2 ...))
   --------------------------------------------------------------------
   (basic_blocks_method_call_param
    C_1 (prefixexp : Name (e_2 ... hole e_3 e_4 ...)) cfg_1 e_5 bblockid cfg_3
    (ctrlfinfo_1 ... ctrlfinfo_2 ...))]
  )

; list of params of builtIn
(define-judgment-form
  lang-data-flow
  #:mode (basic_blocks_builtin_param I I I I I O O)
  #:contract (basic_blocks_builtin_param C C cfg e bblockid cfg
                                         (ctrlfinfo ...))

  [(basic_blocks (in-hole C_1 ($builtIn builtinserv (e_2 ... hole))) cfg_1 e_3
                 bblockid
                 cfg_2 (ctrlfinfo ...))
   --------------------------------------------------------------------
   (basic_blocks_builtin_param C_1 ($builtIn builtinserv (e_2 ... hole))
                               cfg_1
                               e_3 bblockid cfg_2 (ctrlfinfo ...))]

  [(basic_blocks (in-hole C_1 ($builtIn builtinserv (e_2 ... hole e_3 e_4 ...)))
                 cfg_1 e_5 bblockid cfg_2 (ctrlfinfo_1 ...))

   (basic_blocks_builtin_param
    C_1 ($builtIn builtinserv (e_2 ... e_5 hole e_4 ...)) cfg_2 e_3 bblockid
    cfg_3 (ctrlfinfo_2 ...))
   --------------------------------------------------------------------
   (basic_blocks_builtin_param
    C_1 ($builtIn builtinserv (e_2 ... hole e_3 e_4 ...)) cfg_1 e_5 bblockid
    cfg_3 (ctrlfinfo_1 ... ctrlfinfo_2 ...))]
  )

; fields of table constructor
(define-judgment-form
  lang-data-flow
  #:mode (basic_blocks_fields I I I I I O O)
  #:contract (basic_blocks_fields C C cfg (\[ e \] = e) bblockid cfg
                                  (ctrlfinfo ...))

  [(basic_blocks (in-hole C_1 (\{ field ... (\[ hole \] = e_2) \})) cfg_1 e_1
                 bblockid
                 cfg_2 (ctrlfinfo_1 ...))

   (basic_blocks (in-hole C_1 (\{ field ... (\[ e_1 \] = hole) \})) cfg_2 e_2
                 bblockid
                 cfg_3 (ctrlfinfo_2 ...))
   --------------------------------------------------------------------
   (basic_blocks_fields C_1 (\{ field ... hole \})
                        cfg_1
                        (\[ e_1 \] = e_2) bblockid cfg_3
                        (ctrlfinfo_1 ... ctrlfinfo_2 ...))]

  [(basic_blocks (in-hole C_1 (\{ field_1 ... (\[ hole \] = e_2)
                                  field_2 field_3 ... \}))
                 cfg_1 e_1 bblockid cfg_2 (ctrlfinfo_1 ...))

   (basic_blocks (in-hole C_1 (\{ field_1 ... (\[ e_1 \] = hole) field_2
                                  field_3 ... \}))
                 cfg_2 e_2 bblockid cfg_3 (ctrlfinfo_2 ...))

   (basic_blocks_fields
    C_1 (\{ field_1 ... (\[ e_1 \] = e_2) hole field_3 ... \}) cfg_3
    field_2 bblockid cfg_4 (ctrlfinfo_3 ...))
   --------------------------------------------------------------------
   (basic_blocks_fields
    C_1 (\{ field_1 ... hole field_2 field_3 ... \}) cfg_1
    (\[ e_1 \] = e_2) bblockid
    cfg_4 (ctrlfinfo_1 ... ctrlfinfo_2 ... ctrlfinfo_3 ...))]
  )



; extends a given cfg, for a given concat stat
(define-judgment-form
  lang-data-flow
  #:mode (basic_blocks_concat_stats I I I I I O O)
  ; contract:
  ; C_1 : context of the concat stat
  ; C_2 : context of s into the concat stat
  ; cfg_1 : the actual cfg
  ; s : the actual s from the concat stat
  ; bblockid : the id of the actual bblock
  ; cfg_2 : the obtained cfg
  ; (ctrlfinfo ...) : id of the added bblocks, together with the points to
  ; which the control flow should jump, after aterm
  #:contract (basic_blocks_concat_stats C C cfg s bblockid cfg
                                        (ctrlfinfo ...))

  [(basic_blocks (in-hole C_1 C_2)
                 cfg_1
                 s_1
                 bblockid_1
                 cfg_2
                 ; continue in the same block
                 ((continue bblockid_1))
                 )

   ; at least one statement left
   (where (s_2 ... hole s_3 s_4 ...) C_2)

   (basic_blocks_concat_stats C_1 (s_2 ... s_1 hole s_4 ...)
                              cfg_2
                              s_3
                              bblockid_1 cfg_3
                              (ctrlfinfo_1 ...))

   ; filter ctrlfinfo that represent jumps to other points into the program
   ;   (where (ctrlfinfo_2 ...) (filter_ctrlfinfo (ctrlfinfo_1 ...)
   ;                                              (in-hole C_1 C_2)))
   --------------------------------------------------------------------
   (basic_blocks_concat_stats C_1 C_2 cfg_1 s_1 bblockid_1
                              cfg_3
                              (ctrlfinfo_1 ...))]


  [(basic_blocks (in-hole C_1 C_2)
                 cfg_1
                 s_1
                 bblockid_1
                 cfg_2
                 (ctrlfinfo_1 ...) ; continue in the same block
                 )

   ; no statement left
   (where (s_2 ... hole) C_2)

   ; transform (continue bblockid) to (newbblock bblockid)
   (where (ctrlfinfo_2 ...) (add_jump_next_bblock (ctrlfinfo_1 ...)))
   --------------------------------------------------------------------
   (basic_blocks_concat_stats C_1 C_2 cfg_1 s_1 bblockid_1
                              cfg_2
                              (ctrlfinfo_2 ...))]

  [(basic_blocks (in-hole C_1 C_2)
                 cfg_1
                 s_1
                 bblockid_1
                 cfg_2
                 (ctrlfinfo_1 ... (newbblock bblockid_2) ctrlfinfo_2 ...)
                 )
   
   ; at least one statement left
   (where (s_2 ... hole s_3 s_4 ...) C_2)

   ; new basic-block for the next stat
   (where (_ ...
           ((bblockid_3 _) _)) ; get last bblockid
          cfg_2)
   (where bblockid_4 ,(+ 1 (term bblockid_3)))
   (where bblock (bblockid_4 ()))
   ; add bblock
   (where cfg_3 (add_bblock cfg_2 bblock))
   ; update links between basic blocks:
   (where cfg_4 (add_bblock_edge cfg_3
                                 (ctrlfinfo_1 ...
                                  (newbblock bblockid_2)
                                  ctrlfinfo_2 ...)
                                 (bblockid_4)
                                 ((in-hole C_1 C_2))))

   (basic_blocks_concat_stats C_1 (s_2 ... s_1 hole s_4 ...)
                              cfg_4
                              s_3
                              bblockid_4 cfg_5
                              (ctrlfinfo_3 ...))

   ; filter ctrlfinfo that represent jumps to other points into the program
   (where (ctrlfinfo_4 ...) (filter_ctrlfinfo (ctrlfinfo_1 ...
                                               ctrlfinfo_2 ...)
                                              (in-hole C_1 C_2)))
   --------------------------------------------------------------------
   (basic_blocks_concat_stats C_1 C_2 cfg_1 s_1 bblockid_1
                              cfg_5
                              (ctrlfinfo_3 ... ctrlfinfo_4 ...))]

  )

(define-judgment-form
  lang-data-flow
  #:mode (basic_blocks I I I I O O)
  ; contract:
  ; C_1 : context of aterm
  ; cfg_1 : the actual cfg
  ; aterm : the actual aterm
  ; bblockid : the id of the actual bblock
  ; cfg_2 : the obtained cfg
  ; (ctrlfinfo ...) : in case of basic blocks opened, id of the added bblocks
  ; and point to which the control flow should change, after aterm, in case of
  ; a jump; an empty list means that there is no change in control flow

  ; protocol:
  ; blockid (blockid) : continue in the same basic block
  ; blockid_1 (blockid_2 ...) : continue in a new block, add edges from
  ; blockid_2 ... to the new block (if (blockid_2 ...) = () it also means "new
  ; block")
  #:contract (basic_blocks C cfg aterm bblockid cfg (ctrlfinfo ...))
  
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
  [; new node
   (where node_2 (C v))

   ; update cfg
   (where cfg_2 (add_node_bblock cfg_1 bblockid node_2))
   --------------------------------------------------------------------
   (basic_blocks C cfg_1 v bblockid cfg_2
                 ((continue bblockid))
                 )]
  ; var
  [; new node
   (where node_2 (C Name))

   ; update cfg
   (where cfg_2 (add_node_bblock cfg_1 bblockid node_2))
   --------------------------------------------------------------------
   (basic_blocks C cfg_1 Name bblockid cfg_2
                 ((continue bblockid))
                 )]

  [; table exp.
   (basic_blocks (in-hole C (hole \[ e_2 \])) cfg_1 e_1 bblockid cfg_2
                 (ctrlfinfo_1 ...)
                 )

   ; TODO: not considering changes in execution flow from the evaluation of e_1
   ; new node for index exp.
   (basic_blocks (in-hole C (e_1 \[ hole \])) cfg_2 e_2 bblockid cfg_3
                 (ctrlfinfo_2 ...)
                 )

   ; new node for the whole expr..
   (where node (C (e_1 \[ e_2 \])))

   ; update cfg
   (where cfg_4 (add_node_bblock cfg_3 bblockid node))
   --------------------------------------------------------------------
   (basic_blocks C cfg_1 (e_1 \[ e_2 \]) bblockid cfg_4
                 ((continue bblockid))
                 )]

  ; vararg
  [; new node
   (where node (C <<<))

   ; update cfg
   (where cfg_2 (add_node_bblock cfg_1 bblockid node))
   --------------------------------------------------------------------
   (basic_blocks C cfg_1 <<< bblockid cfg_2
                 ((continue bblockid))
                 )]

  ; functioncall
  [; function
   (basic_blocks (in-hole C (hole ())) cfg_1 prefixexp bblockid cfg_2
                 (ctrlfinfo_1 ...)
                 )
                   
   ; new node for the whole expr..
   (where node (C (prefixexp ())))

   ; update cfg
   (where cfg_3 (add_node_bblock cfg_2 bblockid node))
   --------------------------------------------------------------------
   (basic_blocks C cfg_1 (prefixexp ()) bblockid cfg_3
                 (; TODO: not allowing fcalls to change execution order
                  (continue bblockid)))]

  
  [; function
   (basic_blocks (in-hole C (hole (e_1 e_2 ...))) cfg_1 prefixexp bblockid cfg_2
                 (ctrlfinfo_1 ...)
                 )

   ; params
   (basic_blocks_fun_call_param C (prefixexp (hole e_2 ... )) cfg_2
                                e_1 bblockid cfg_3 (ctrlfinfo_2 ...))
                   
   ; new node for the whole expr..
   (where node (C (prefixexp (e_1 e_2 ...))))

   ; update cfg
   (where cfg_4 (add_node_bblock cfg_3 bblockid node))
   --------------------------------------------------------------------
   (basic_blocks C cfg_1 (prefixexp (e_1 e_2 ...)) bblockid cfg_4
                 (; TODO: not allowing fcalls to change execution order
                  (continue bblockid)
                  )
                 )]

  ; method call
  [; function
   (basic_blocks (in-hole C (hole : Name ())) cfg_1 prefixexp bblockid cfg_2
                 (ctrlfinfo_1 ...)
                 )
                   
   ; new node for the whole expr..
   (where node (C (prefixexp : Name ())))

   ; update cfg
   (where cfg_3 (add_node_bblock cfg_2 bblockid node))
   --------------------------------------------------------------------
   (basic_blocks C cfg_1 (prefixexp : Name ()) bblockid cfg_3
                 (; TODO: not allowing fcalls to change execution order
                  (continue bblockid)))]

  
  [; function
   (basic_blocks (in-hole C (hole : Name (e_1 e_2 ...))) cfg_1 prefixexp
                 bblockid cfg_2
                 (ctrlfinfo_1 ...)
                 )

   ; params
   (basic_blocks_method_call_param C (prefixexp : Name (hole e_2 ... )) cfg_2
                                   e_1 bblockid cfg_3 (ctrlfinfo_2 ...))
                   
   ; new node for the whole expr..
   (where node (C (prefixexp : Name (e_1 e_2 ...))))

   ; update cfg
   (where cfg_4 (add_node_bblock cfg_3 bblockid node))
   --------------------------------------------------------------------
   (basic_blocks C cfg_1 (prefixexp : Name (e_1 e_2 ...)) bblockid cfg_4
                 (; TODO: not allowing fcalls to change execution order
                  (continue bblockid)
                  )
                 )]

  ; builtIn
  [; new node for the whole expr..
   (where node (C ($builtIn builtinserv ())))

   ; update cfg
   (where cfg_2 (add_node_bblock cfg_1 bblockid node))
   --------------------------------------------------------------------
   (basic_blocks C cfg_1 ($builtIn builtinserv ()) bblockid cfg_2
                 (; TODO: not allowing fcalls to change execution order
                  (continue bblockid)))]

  
  [; params
   (basic_blocks_builtin_param C ($builtIn builtinserv (hole e_2 ...))
                               cfg_1
                               e_1 bblockid cfg_2 (ctrlfinfo_2 ...))
                   
   ; new node for the whole expr..
   (where node (C ($builtIn builtinserv (e_1 e_2 ...))))

   ; update cfg
   (where cfg_3 (add_node_bblock cfg_2 bblockid node))
   --------------------------------------------------------------------
   (basic_blocks C cfg_1 ($builtIn builtinserv (e_1 e_2 ...)) bblockid cfg_3
                 (; TODO: not allowing fcalls to change execution order
                  (continue bblockid)
                  )
                 )]

  ; parent. exp
  [(basic_blocks (in-hole C (\( hole \))) cfg_1 e bblockid cfg_2
                 (ctrlfinfo_1 ...)
                 )
   ; new node
   (where node (C (\( e \))))

   ; update cfg
   (where cfg_3 (add_node_bblock cfg_2 bblockid node))
   --------------------------------------------------------------------
   (basic_blocks C cfg_1 (\( e \)) bblockid cfg_3 ((continue bblockid)))]

  ; table constructor
  [; new node
   (where node (C (\{ \})))

   ; update cfg
   (where cfg_2 (add_node_bblock cfg_1 bblockid node))
   --------------------------------------------------------------------
   (basic_blocks C cfg_1 (\{ \}) bblockid cfg_2 ((continue bblockid)))]

  [(basic_blocks_fields C (\{ hole field ... \})
                        cfg_1 (\[ e_1 \] = e_2) bblockid cfg_2 (ctrlfinfo ...))
   ; new node
   (where node (C (\{ (\[ e_1 \] = e_2) field ... \})))

   ; update cfg
   (where cfg_3 (add_node_bblock cfg_2 bblockid node))
   --------------------------------------------------------------------
   (basic_blocks C cfg_1 (\{ (\[ e_1 \] = e_2) field ... \}) bblockid cfg_3
                 ((continue bblockid)))]

  ; binary op
  [(basic_blocks (in-hole C (hole binop e_2)) cfg_1 e_1 bblockid cfg_2
                 (ctrlfinfo_1 ...)
                 )

   (basic_blocks (in-hole C (e_1 binop hole)) cfg_2 e_2 bblockid cfg_3
                 (ctrlfinfo_2 ...)
                 )
   ; new node
   (where node (C (e_1 binop e_2)))

   ; update cfg
   (where cfg_4 (add_node_bblock cfg_3 bblockid node))
   --------------------------------------------------------------------
   (basic_blocks C cfg_1 (e_1 binop e_2) bblockid cfg_4 ((continue bblockid)))]

  ; unop
  [(basic_blocks (in-hole C (unop hole)) cfg_1 e bblockid cfg_2
                 (ctrlfinfo_1 ...)
                 )

   ; new node
   (where node (C (unop e)))

   ; update cfg
   (where cfg_3 (add_node_bblock cfg_2 bblockid node))
   --------------------------------------------------------------------
   (basic_blocks C cfg_1 (unop e) bblockid cfg_3 ((continue bblockid)))]

  ; functiondef
  [; new node
   (where node (C functiondef))

   ; update cfg
   (where cfg_2 (add_node_bblock cfg_1 bblockid node))
   --------------------------------------------------------------------
   (basic_blocks C cfg_1 functiondef bblockid cfg_2 ((continue bblockid)))]

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
  [; new node
   (where node_2 (C \;))

   ; update cfg
   (where cfg_2 (add_node_bblock cfg_1 bblockid node_2))
   --------------------------------------------------------------------
   (basic_blocks C cfg_1 \; bblockid cfg_2
                 ((continue bblockid))
                 )]

  
  ; conditional
  ; if e then s_1 else s_2 end ->
  ;               if e then jump if_branch
  ; else_branch : s_2
  ;               jump cont
  ; if_branch   : s_1
  ; cont        : ...
  [; guard
   (basic_blocks (in-hole C (if hole then s_1 else s_2 end))
                 cfg_1
                 e
                 bblockid_1 cfg_2
                 (ctrlfinfo_1 ...))
   
   ; if branch
   ; jump from guard to if branch: new basic block
   (where (_ ...
           ((bblockid_2 _) _)) ; get last bblockid
          cfg_2)
   (where bblockid_3 ,(+ 1 (term bblockid_2)))
   (where bblock_4 (bblockid_3 ()))
   ; add bblock
   (where cfg_3 (add_bblock cfg_2 bblock_4))
   ; jump from the guard to the if branch: edge from the guard to the if branch
   (where cfg_4 (add_bblock_edge cfg_3 ((newbblock bblockid_1))
                                 (bblockid_3)
                                 ((in-hole C (if e then hole else s_2 end)))))

   (basic_blocks (in-hole C (if e then hole else s_2 end))
                 cfg_4
                 s_1
                 bblockid_3 cfg_5
                 (ctrlfinfo_2 ...))

   ; transform (continue bblockid) to (newbblock bblockid)
   (where (ctrlfinfo_3 ...) (add_jump_next_bblock (ctrlfinfo_2 ...)))

   ; else branch
   ; jump from guard to else branch: new basic block new basic block
   (where (_ ...
           ((bblockid_4 _) _)) ; get last bblockid
          cfg_5)
   (where bblockid_5 ,(+ 1 (term bblockid_4)))
   (where bblock_5 (bblockid_5 ()))
   ; add bblock
   (where cfg_6 (add_bblock cfg_5 bblock_5))
   ; update links between basic blocks:
   (where cfg_7 (add_bblock_edge cfg_6 ((newbblock bblockid_1)) (bblockid_5)
                                 ((in-hole C (if e then s_1 else hole end)))))

   (basic_blocks (in-hole C (if e then s_1 else hole end))
                 cfg_7
                 s_2
                 bblockid_5 cfg_8
                 (ctrlfinfo_4 ...))
   
   ; transform (continue bblockid) to (newbblock bblockid)
   (where (ctrlfinfo_5 ...) (add_jump_next_bblock (ctrlfinfo_4 ...)))
   --------------------------------------------------------------------
   (basic_blocks C cfg_1 (if e then s_1 else s_2 end) bblockid_1
                 cfg_8
                 ; TODO: ctrlfinfo_1?
                 (ctrlfinfo_3 ... ctrlfinfo_5 ...))]
  
  ; concat stats
  [(basic_blocks_concat_stats C (hole s_2 s_3 ...)
                              cfg_1 s_1 bblockid_1 cfg_2
                              (ctrlfinfo ...))
   --------------------------------------------------------------------
   (basic_blocks C cfg_1 (s_1 s_2 s_3 ...) bblockid_1
                 cfg_2
                 (ctrlfinfo ...))]

  ; while loop
  ; while e do s end ->
  ; guard : if not e jump end
  ; body  : s
  ;         jump guard
  ; end   : ...
  [; new node for the guard
   (where C_2 (in-hole C (while hole do s end))) ; node id
   (where node_2 (C_2 e))

   ; add new node to the actual bblock
   (where cfg_2 (add_node_bblock cfg_1 bblockid_1 node_2))
   
   ; body
   ; new basic block
   (where (_ ...
           ((bblockid_2 _) _)) ; get last bblockid
          cfg_2)
   (where bblockid_3 ,(+ 1 (term bblockid_2)))
   (where bblock_1 (bblockid_3 ()))
   ; add bblock
   (where cfg_3 (add_bblock cfg_2 bblock_1))
   ; update links between basic blocks: edge from the guard to the body
   (where cfg_4 (add_bblock_edge cfg_3 ((newbblock bblockid_1)) (bblockid_3)
                                 ((in-hole C (while e do hole end)))))
   ; cfg of the body
   (basic_blocks (in-hole C (while e do hole end))
                 cfg_4
                 s
                 bblockid_3 cfg_5
                 (ctrlfinfo_1 ...))

   ; convert (continue bblockid) into jumps to the guard: (bblockid C)
   (where (ctrlfinfo_2 ...) (convert_into_loop
                             (ctrlfinfo_1 ...)
                             ; TODO: ugly fix to manage jumps to the body of
                             ; the loop
                             C
                             (while e do s end)
                             ))
   
   ; add edges from the body to the guard: in case of jump, add_bblock_edge
   ; compares the target with the evaluation context C; if target and C are
   ; the same, an edge is added
   (where cfg_6 (add_bblock_edge cfg_5 (ctrlfinfo_2 ...)
                                 (bblockid_1)
                                 ((in-hole C (while hole do s end)))))

   ; filter jumps outside the loop (that is, delete jumps of the form
   ; (bblockid (in-hole C (while hole do s end))))
   (where (ctrlfinfo_3 ...) (filter_ctrlfinfo
                             (ctrlfinfo_2 ...)
                             (in-hole C (while hole do s end))))
   --------------------------------------------------------------
   (basic_blocks C cfg_1 (while e do s end) bblockid_1
                 cfg_6
                 (; add a jump to the basic block following the guard of the
                  ; loop
                  (newbblock bblockid_1)
                  ctrlfinfo_3 ...))]
  
  ; break
  [; new node
   (where node (C break))

   ; add new node to the cfg
   (where cfg_2 (add_node_bblock cfg_1 bblockid_1 node))

   ; determine target of the unconditional jump
   (where (in-hole
           C_2
           (while e do
                  (side-condition
                   C_3
                   (not
                    (redex-match? lang-data-flow
                                  (in-hole C_4
                                           (while e do
                                                  C_5
                                                  end))
                                  (term C_3))))
                  end)) C)
   --------------------------------------------------------------------
   (basic_blocks C cfg_1 break bblockid_1
                 cfg_2
                 (; return a change in the execution flow: jump to C_2
                  (bblockid_1 C_2)
                  ))]

  ; funcall
  [; function
   (basic_blocks (in-hole C ($statFunCall hole ())) cfg_1 prefixexp bblockid
                 cfg_2
                 (ctrlfinfo_1 ...)
                 )
                   
   ; new node for the whole expr..
   (where node (C ($statFunCall prefixexp ())))

   ; update cfg
   (where cfg_3 (add_node_bblock cfg_2 bblockid node))
   --------------------------------------------------------------------
   (basic_blocks C cfg_1 ($statFunCall prefixexp ()) bblockid cfg_3
                 (; TODO: not allowing fcalls to change execution order
                  (continue bblockid)))]

  
  [; function
   (basic_blocks (in-hole C ($statFunCall hole (e_1 e_2 ...))) cfg_1 prefixexp
                 bblockid cfg_2 (ctrlfinfo_1 ...)
                 )

   ; params
   (basic_blocks_stat_fun_call_param C ($statFunCall prefixexp (hole e_2 ... ))
                                     cfg_2
                                     e_1 bblockid cfg_3 (ctrlfinfo_2 ...))
                   
   ; new node for the whole expr..
   (where node (C ($statFunCall prefixexp (e_1 e_2 ...))))

   ; update cfg
   (where cfg_4 (add_node_bblock cfg_3 bblockid node))
   --------------------------------------------------------------------
   (basic_blocks C cfg_1 ($statFunCall prefixexp (e_1 e_2 ...)) bblockid cfg_4
                 (; TODO: not allowing fcalls to change execution order
                  (continue bblockid)
                  )
                 )]

  [; new node
   (where node_2 (C ($statFunCall : Name prefixexp (e ...))))

   ; update cfg
   (where cfg_2 (add_node_bblock cfg_1 bblockid node_2))
   --------------------------------------------------------------------
   (basic_blocks C cfg_1 ($statFunCall : Name prefixexp (e ...)) bblockid cfg_2
                 ((continue bblockid)))]

  ; return
  [; new node
   (where node (C (return)))

   ; update cfg
   (where cfg_2 (add_node_bblock cfg_1 bblockid node))
   --------------------------------------------------------------------
   (basic_blocks C cfg_1 (return) bblockid cfg_2
                 (
                  (continue bblockid)
                  ))]
  
  [(basic_blocks_return C (return hole e_2 ...) cfg_1 e_1 bblockid cfg_2
                        (ctrlfinfo ...))
   
   ; new node
   (where node (C (return e_1 e_2 ...)))

   ; update cfg
   (where cfg_3 (add_node_bblock cfg_2 bblockid node))
   --------------------------------------------------------------------
   (basic_blocks C cfg_1 (return e_1 e_2 ...) bblockid cfg_3
                 (
                  (continue bblockid)
                  ))]
  
  ; assign
  [(basic_blocks_assign C (hole var_2 ... = e ...) cfg_1 var_1 bblockid
                        cfg_2 (ctrlfinfo_1 ...))
   ; new node
   (where node (C (var_1 var_2 ... = e ...)))

   ; update cfg
   (where cfg_3 (add_node_bblock cfg_2 bblockid node))
   --------------------------------------------------------------------
   (basic_blocks C cfg_1 (var_1 var_2 ... = e ...) bblockid cfg_3
                 (;ctrlfinfo_1 ...
                  (continue bblockid)))]

  ; local var
  [(basic_blocks_loc_var C (local (Name : t) ... = hole e_2 ... in s end) cfg_1
                         e_1 bblockid cfg_2 (ctrlfinfo_1 ...))

   ; filter jumps of the form (bblockid C)
   (where (ctrlfinfo_2 ...) (filter_bblockid_ctrlfinfo
                             (ctrlfinfo_1 ...
                              ;ctrlfinfo_2 ...
                              )))
   --------------------------------------------------------------------
   (basic_blocks C cfg_1 (local (Name : t) ... = e_1 e_2 ... in s end) bblockid
                 cfg_2
                 (;ctrlfinfo_2 ...
                  ; TODO: what's with the last stat in s that could ask for the
                  ; continuation into the same basic block?
                  (newbblock bblockid) ; TODO: newbblock?
                  ))]

  ; do-end
  [; new node
   (where node (C (do s end)))

   ; update cfg
   (where cfg_2 (add_node_bblock cfg_1 bblockid node))

   ; body
   (basic_blocks (in-hole C (do hole end))
                 cfg_2
                 s
                 bblockid cfg_3
                 (ctrlfinfo_1 ...))

   ; filter jumps of the form (bblockid C)
   (where (ctrlfinfo_2 ...) (filter_bblockid_ctrlfinfo (ctrlfinfo_1 ...)))
   --------------------------------------------------------------------
   (basic_blocks C cfg_1 (do s end) bblockid cfg_3
                 (ctrlfinfo_2 ...
                  ; TODO: what's with the last stat in s that could ask for the
                  ; continuation into the same basic block?
                  (continue bblockid) ; TODO: continue?
                  ))]
  
  )

(provide basic_blocks)


;                                               
;                                               
;                                               
;                                       ;       
;                                       ;       
;                                       ;       
;     ;;; ;    ; ;;;    ;;;;   ; ;;;    ; ;;;;  
;    ;   ;;    ;;   ;  ;    ;  ;;   ;   ;;   ;; 
;   ;     ;    ;            ;  ;     ;  ;     ; 
;   ;     ;    ;       ;;;;;;  ;     ;  ;     ; 
;   ;     ;    ;      ;;    ;  ;     ;  ;     ; 
;   ;     ;    ;      ;     ;  ;     ;  ;     ; 
;    ;   ;;    ;      ;    ;;  ;;   ;   ;     ; 
;     ;;; ;    ;       ;;;; ;  ; ;;;    ;     ; 
;         ;                    ;                
;    ;   ;;                    ;                
;     ;;;;                     ;                
;                                               

; relation that walks a given cfg
(define-metafunction lang-data-flow

  [(build-cfg-graph cfg)
   ,(reduction-relation
     lang-data-flow                                             
     #:domain (bblockid (node ...))
   
     [--> (bblockid_2 (node_1 ...))
          (bblockid_4 (node_2 ...))
         
          (where
           ((bblock_1 (bblockid_1 ...)) ...
            ((bblockid_2 (node_1 ...)) (bblockid_3 ... bblockid_4
                                                   bblockid_5 ...))
            (bblock_2 (bblockid_6 ...)) ...)
           ,(term cfg))
         
          (where
           (_ ...
            ((bblockid_4 (node_2 ...)) _)
            _ ...)
          
           ,(term cfg)
           )
          ])])

(provide build-cfg-graph)