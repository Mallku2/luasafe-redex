#lang racket

(require redex
         "./grammar.rkt"
         "./typing_lang_theory.rkt")

(define-extended-language lang-data-flow core-lang-typed

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
   (bblockid $nopos)
   ; continue from bblockid to the next instruction, that
   ; should be in a new basic block (add an edge from bblockid to the
   ; next basic block)
   (newbblock bblockid)
             
   ; continue adding instructions in the same bblock
   (continue bblockid)
   ]

  [jumpoint C $nopos]
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
                            $nopos
                            ()
                            aterm
                            1
                            cfg
                            (ctrlfinfo ...))
                           (cfg (ctrlfinfo ...))))
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

; adds an edge from bblocks that are closed, to another bblock.
; PARAMS:
; cfg : the original cfg
; (ctrlinfo ...): the origin of the edges are identified by ctrlfinfo
; (bblockid ...) and (C ...): The end is identified by the bblock id and a
; context C that specifies the position into the program of the origin of said
; bblock 
; RETURNS:
; a new cfg with the added edges
(define-metafunction lang-data-flow

  add_bblock_edge : cfg (ctrlfinfo ...) (bblockid ...)
  ;TODO: cannot specify (C ...)
  any -> (cfg (ctrlfinfo ...))

  [(add_bblock_edge cfg (ctrlfinfo ...) () ())
   (cfg (ctrlfinfo ...))]

  [(add_bblock_edge cfg_1 (ctrlfinfo_1 ...) (bblockid_1 bblockid_2 ...)
                    (C_1 C_2 ...))

   (add_bblock_edge cfg_2 (ctrlfinfo_2 ...) (bblockid_2 ...)
                    (C_2 ...))

   (where (cfg_2 (ctrlfinfo_2 ...))
          (add_bblock_edge_aux cfg_1 (ctrlfinfo_1 ...) bblockid_1
                               C_1 ()))]
  )


(define-metafunction lang-data-flow

  add_bblock_edge_aux : cfg (ctrlfinfo ...) bblockid C (ctrlfinfo ...)
  -> (cfg (ctrlfinfo ...))

  [(add_bblock_edge_aux cfg () bblockid C (ctrlfinfo ...))
   (cfg (ctrlfinfo ...))]

  ; bblockid_2 is closed and ends in a jump to bblockid_5:
  [(add_bblock_edge_aux ((bblock_1 (bblockid_1 ...)) ...
                         ((bblockid_2 (node ...)) (bblockid_3 ...))
                         (bblock_2 (bblockid_4 ...)) ...)
                        ; jump to position C: same position as beginning of
                        ; bblockid_5
                        ((bblockid_2 C)
                         ctrlfinfo_1 ...)
                        bblockid_5
                        C
                        (ctrlfinfo_2 ...))

   (add_bblock_edge_aux ((bblock_1 (bblockid_1 ...)) ...
                         ((bblockid_2 (node ...)) (bblockid_3 ... bblockid_5))
                         (bblock_2 (bblockid_4 ...)) ...)
                        (ctrlfinfo_1 ...)
                        bblockid_5
                        C
                        (ctrlfinfo_2 ...))]

  ; bblockid_2 is not closed or it ends in a jump to another position,
  ; different than C
  [(add_bblock_edge_aux cfg (ctrlfinfo_1 ctrlfinfo_2 ...) bblockid C
                        (ctrlfinfo_3 ...))
   (add_bblock_edge_aux cfg (ctrlfinfo_2 ...) bblockid C
                        (ctrlfinfo_3 ... ctrlfinfo_1 ))]
  )

; add new bblock to a given cfg
(define-metafunction lang-data-flow

  add_bblock : cfg bblock -> cfg

  [(add_bblock ((bblock_1 (bblockid_1 ...)) ...)
               bblock_2)
   ((bblock_1 (bblockid_1 ...)) ... (bblock_2 ()))]
  )

; functions to manipulate execution flow info

; to model execution flow from the body of a while loop to its guard or jumps
; outside of the loop (because of break statements):
; converts (continue bblockid) or (newbblock bblockid) into a jump of the form
; (bblockid C), for a given context C that points to the guard of a loop.
; Converts jumps from the body, of the form C_1[while e do C_2 end],
; PARAMS:
; (ctrlfinfo ...) : ctrlfinfo obtained from constructing the cfg of the body of
;                   the loop
; C : context pointing to the guard of the loop
; 
(define-metafunction lang-data-flow

  convert_to_jump : (ctrlfinfo ...) jumpoint -> (ctrlfinfo ...)

  [(convert_to_jump () jumpoint)
   ()]
  
  [(convert_to_jump ((continue bblockid) ctrlfinfo_1 ...) jumpoint)
   ((bblockid jumpoint) ctrlfinfo_2 ...)

   (where (ctrlfinfo_2 ...) (convert_to_jump (ctrlfinfo_1 ...) jumpoint))]

  [(convert_to_jump ((bblockid jumpoint_1) ctrlfinfo_1 ...) jumpoint_2)
   ((bblockid jumpoint_1) ctrlfinfo_2 ...)

   (where (ctrlfinfo_2 ...) (convert_to_jump (ctrlfinfo_1 ...) jumpoint_2))]
  )


; converts (continue bblockid) into a jump to the following basic block:
; (newbblock bblockid)
(define-metafunction lang-data-flow
  add_jump_next_bblock : (ctrlfinfo ...) -> (ctrlfinfo ...)

  [(add_jump_next_bblock ())
   ()]

  [(add_jump_next_bblock (;(continue bblockid)
                          (continue bblockid)
                          ctrlfinfo_1 ...))
   (;(newbblock bblockid)
    (newbblock bblockid)
    ctrlfinfo_2 ...)

   (where (ctrlfinfo_2 ...) (add_jump_next_bblock (ctrlfinfo_1 ...)))]

  [(add_jump_next_bblock (ctrlfinfo_1 ctrlfinfo_2 ...))
   (ctrlfinfo_1 ctrlfinfo_3 ...)

   (where (ctrlfinfo_3 ...) (add_jump_next_bblock (ctrlfinfo_2 ...)))]
  )

(define-metafunction lang-data-flow
  remove_cont : (ctrlfinfo ...) -> (ctrlfinfo ...)

  [(remove_cont ())
   ()]
  
  [(remove_cont ((continue bblockid) ctrlfinfo ...))
   (remove_cont (ctrlfinfo ...))]

  [(remove_cont (ctrlfinfo_1 ctrlfinfo_2 ...))
   (ctrlfinfo_1 ctrlfinfo_3 ...)

   (where (ctrlfinfo_3 ...) (remove_cont (ctrlfinfo_2 ...)))])


;; extract jumps expressed by a (bblockid_1 C_1) ctrlfinfo that are not referred to
;; a given point into the program, indicated by a given context C_2
;(define-metafunction lang-data-flow
;  filter_ctrlfinfo : (ctrlfinfo ...) C -> ((bblockid C) ...) ;((bblockid C) ...)
;
;  [(filter_ctrlfinfo () C)
;   ()]
;  
;  ; ctrlfinfo refers to the point indicated by C 
;  [(filter_ctrlfinfo (;(bblockid C)
;                      (bblockid C)
;                      ctrlfinfo ...) C)
;   (filter_ctrlfinfo (ctrlfinfo ...) C)]
;
;  ; ctrlfinfo doesn't refer to the point indicated by C
;  [(filter_ctrlfinfo ((bblockid_1 C_1) ctrlfinfo ...) C_2)
;   ((bblockid_1 C_1) (bblockid_2 C_3) ...)
;
;   (where ;((bblockid_2 C_3) ...)
;    ((bblockid_2 C_3) ...)
;    (filter_ctrlfinfo (ctrlfinfo ...) C_2))]
;
;  ; not a jump
;  [(filter_ctrlfinfo (ctrlfinfo_1 ctrlfinfo_2 ...) C)
;   (filter_ctrlfinfo (ctrlfinfo_2 ...) C)])
;
;; extract jumps (ctrlfinfo like (bblockid C))
;(define-metafunction lang-data-flow
;  filter_bblockid_ctrlfinfo : (ctrlfinfo ...) -> (;(bblockid C)
;                                                  (bblockid C)
;                                                  ...)
;
;  [(filter_bblockid_ctrlfinfo ())
;   ()]
;
;  ; ctrlfinfo doesn't refer to the point indicated by C
;  [(filter_bblockid_ctrlfinfo (;(bblockid_1 C_1)
;                               (bblockid_1 C_1)
;                               ctrlfinfo ...))
;   ;((bblockid_1 C_1) (bblockid_2 C_2) ...)
;   ((bblockid_1 C_1) (bblockid_2 C_2) ...)
;
;   (where ;((bblockid_2 C_2) ...)
;    ((bblockid_2 C_2) ...)
;    (filter_bblockid_ctrlfinfo (ctrlfinfo ...)))]
;
;  
;  ; not a jump
;  [(filter_bblockid_ctrlfinfo (ctrlfinfo_1 ctrlfinfo_2 ...))
;   (filter_bblockid_ctrlfinfo (ctrlfinfo_2 ...))])

; given a position into a program specified by a context C, returns the next
; instruction that must be executed in an execution
(define-metafunction lang-data-flow
  next_pos : C s -> jumpoint

  [(next_pos (in-hole C (score_1 ... hole score_2 score_3 ...)) s)
   (in-hole C (score_1 ... s hole score_3 ...))]

  ;((while 1 do (while 1 do hole end) end) |;|) s)
  [(next_pos (in-hole C (while e do hole end)) s)
   (in-hole C (while hole do s end))]

  [(next_pos _ _)
   $nopos])

; find the innermost while loop,
(define-metafunction lang-data-flow
  break_jump : C -> jumpoint

  [(break_jump (in-hole C (while e do Cnw end)))
   (next_pos C (while e do (in-hole Cnw break) end))])

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
;
; list of expressions of fun calls
(define-judgment-form
  lang-data-flow
  #:mode (basic_blocks_fun_call_param I I I I I I O O)
  #:contract (basic_blocks_fun_call_param C C jumpoint cfg e bblockid cfg
                                          (ctrlfinfo ...))

  [(basic_blocks (in-hole C_1 (prefixexp (e_2 ... hole))) jumpoint cfg_1 e_3
                 bblockid cfg_2 (ctrlfinfo ...))
   --------------------------------------------------------------------
   (basic_blocks_fun_call_param C_1 (prefixexp (e_2 ... hole)) jumpoint cfg_1
                                e_3 bblockid cfg_2 (ctrlfinfo ...))]

  [(basic_blocks (in-hole C_1 (prefixexp (e_2 ... hole e_3 e_4 ...))) jumpoint
                 cfg_1 e_5 bblockid cfg_2 (ctrlfinfo_1 ...))

   (basic_blocks_fun_call_param C_1 (prefixexp (e_2 ... e_5 hole e_4 ...))
                                jumpoint cfg_2
                                e_3 bblockid cfg_3 (ctrlfinfo_2 ...))
   --------------------------------------------------------------------
   (basic_blocks_fun_call_param C_1 (prefixexp (e_2 ... hole e_3 e_4 ...))
                                jumpoint cfg_1
                                e_5 bblockid cfg_3 (ctrlfinfo_1 ...
                                                    ctrlfinfo_2 ...))]
  
  )

; list of expressions of return
(define-judgment-form
  lang-data-flow
  #:mode (basic_blocks_return I I I I I I O O)
  #:contract (basic_blocks_return C C jumpoint cfg e bblockid cfg
                                  (ctrlfinfo ...))

  [(basic_blocks (in-hole C_1 (return e_2 ... hole)) jumpoint cfg_1 e_3 bblockid
                 cfg_2 (ctrlfinfo ...))
   --------------------------------------------------------------------
   (basic_blocks_return C_1 (return e_2 ... hole) jumpoint cfg_1
                        e_3 bblockid cfg_2 (ctrlfinfo ...))]

  [(basic_blocks (in-hole C_1 (return e_2 ... hole e_3 e_4 ...)) jumpoint cfg_1 
                 e_5 bblockid cfg_2 (ctrlfinfo_1 ...))

   (basic_blocks_return C_1 (return e_2 ... e_5 hole e_4 ...) jumpoint cfg_2
                        e_3 bblockid cfg_3 (ctrlfinfo_2 ...))
   --------------------------------------------------------------------
   (basic_blocks_return C_1 (return e_2 ... hole e_3 e_4 ...) jumpoint cfg_1
                        e_5 bblockid cfg_3 (ctrlfinfo_1 ...
                                            ctrlfinfo_2 ...))]
  
  )

;list of lvalues and rvalues of assignments
(define-judgment-form
  lang-data-flow
  #:mode (basic_blocks_assign I I I I I I O O)
  #:contract (basic_blocks_assign C C jumpoint cfg e bblockid cfg
                                  (ctrlfinfo ...))

  ; last exp
  [(basic_blocks (in-hole C_1 (var ... = e_1 ... hole)) jumpoint cfg_1 e_2
                 bblockid cfg_2 (ctrlfinfo ...))
   --------------------------------------------------------------------
   (basic_blocks_assign C_1 (var ... = e_1 ... hole) jumpoint cfg_1
                        e_2 bblockid cfg_2 (ctrlfinfo ...))]

  ; not the last exp
  [(basic_blocks (in-hole C_1 (var ... = e_1 ... hole e_2 e_3 ...)) jumpoint
                 cfg_1 e_4 bblockid cfg_2 (ctrlfinfo_1 ...))

   (basic_blocks_assign C_1 (var ... = e_1 ... e_4 hole e_3 ...) jumpoint cfg_2
                        e_2 bblockid cfg_3 (ctrlfinfo_2 ...))
   --------------------------------------------------------------------
   (basic_blocks_assign C_1 (var ... = e_1 ... hole e_2 e_3 ...) jumpoint cfg_1
                        e_4 bblockid cfg_3 (ctrlfinfo_1 ...
                                            ctrlfinfo_2 ...))]

  ; var, not the last one
  [(basic_blocks (in-hole C_1 (var_1 ... hole var_2 var_3 ... = e ...))
                 jumpoint cfg_1 var_4 bblockid cfg_2 (ctrlfinfo_1 ...))

   (basic_blocks_assign C_1 (var_1 ... var_4 hole var_3 ... = e ...) jumpoint
                        cfg_2 var_2 bblockid cfg_3 (ctrlfinfo_2 ...))
   --------------------------------------------------------------------
   (basic_blocks_assign C_1 (var_1 ... hole var_2 var_3 ... = e ...) jumpoint
                        cfg_1 var_4 bblockid cfg_3 (ctrlfinfo_1 ...
                                                    ctrlfinfo_2 ...))]

  ; last var
  [(basic_blocks (in-hole C_1 (var_1 ... hole = e_1 e_2 ...)) jumpoint cfg_1 
                 var_2 bblockid cfg_2 (ctrlfinfo_1 ...))

   (basic_blocks_assign C_1 (var_1 ... var_2 = hole e_2 ...) jumpoint cfg_2
                        e_1 bblockid cfg_3 (ctrlfinfo_2 ...))
   --------------------------------------------------------------------
   (basic_blocks_assign C_1 (var_1 ... hole = e_1 e_2 ...) jumpoint cfg_1
                        var_2 bblockid cfg_3 (ctrlfinfo_1 ...
                                              ctrlfinfo_2 ...))]
  
  )

; list of lvalues and rvalues of assignments
(define-judgment-form
  lang-data-flow
  #:mode (basic_blocks_loc_var I I I I I I O O)
  #:contract (basic_blocks_loc_var C C jumpoint cfg e bblockid cfg
                                   (ctrlfinfo ...))

  ; last exp
  [(basic_blocks (in-hole C_1 (local (Name : t) ... = e_1 ... hole in s end))
                 jumpoint cfg_1 e_2 bblockid
                 cfg_2 (ctrlfinfo_1 ...))

   ; new node
   (where node ((in-hole C_1 (local hole in s end)) (Name ... = e_1 ... e_2)))

   ; update cfg
   (where cfg_3 (add_node_bblock cfg_2 bblockid node))
   
   ; body
   (basic_blocks (in-hole C_1 (local (Name : t) ... = e_1 ... e_2 in hole end))
                 jumpoint cfg_3 s bblockid
                 cfg_4 (ctrlfinfo_2 ...))
   --------------------------------------------------------------------
   (basic_blocks_loc_var C_1 (local (Name : t) ... = e_1 ... hole in s end)
                         jumpoint
                         cfg_1 e_2 bblockid cfg_4 (ctrlfinfo_1 ...
                                                   ctrlfinfo_2 ...))]

  ; not the last exp
  [(basic_blocks (in-hole C_1 (local (Name : t) ... = e_1 ... hole e_2 e_3 ...
                                in s end)) jumpoint cfg_1 
                 e_4 bblockid cfg_2 (ctrlfinfo_1 ...))

   (basic_blocks_loc_var C_1 (local (Name : t) ... = e_1 ... e_4 hole e_3 ...
                                in s end) jumpoint cfg_2
                        e_2 bblockid cfg_3 (ctrlfinfo_2 ...))
   --------------------------------------------------------------------
   (basic_blocks_loc_var C_1 (local (Name : t) ... = e_1 ... hole e_2 e_3 ...
                                in s end) jumpoint cfg_1
                        e_4 bblockid cfg_3 (ctrlfinfo_1 ...
                                            ctrlfinfo_2 ...))]
  
  )

; params in stat. fun. call
(define-judgment-form
  lang-data-flow
  #:mode (basic_blocks_stat_fun_call_param I I I I I I O O)
  #:contract (basic_blocks_stat_fun_call_param C C jumpoint cfg e bblockid cfg
                                               (ctrlfinfo ...))

  [(basic_blocks (in-hole C_1 ($statFunCall prefixexp (e_2 ... hole))) jumpoint
                 cfg_1 e_3 bblockid cfg_2 (ctrlfinfo ...))
   --------------------------------------------------------------------
   (basic_blocks_stat_fun_call_param C_1 ($statFunCall prefixexp (e_2 ... hole))
                                     jumpoint cfg_1 e_3 bblockid cfg_2
                                     (ctrlfinfo ...))]

  [(basic_blocks (in-hole C_1 ($statFunCall prefixexp
                                            (e_2 ... hole e_3 e_4 ...)))
                 jumpoint cfg_1 e_5 bblockid cfg_2 (ctrlfinfo_1 ...))

   (basic_blocks_stat_fun_call_param C_1
                                     ($statFunCall prefixexp
                                                   (e_2 ... e_5 hole e_4 ...))
                                     jumpoint cfg_2
                                     e_3 bblockid cfg_3 (ctrlfinfo_2 ...))
   --------------------------------------------------------------------
   (basic_blocks_stat_fun_call_param C_1
                                     ($statFunCall prefixexp
                                                   (e_2 ... hole e_3 e_4 ...))
                                     jumpoint
                                     cfg_1
                                     e_5 bblockid cfg_3 (ctrlfinfo_1 ...
                                                         ctrlfinfo_2 ...))]
  )

; params in stat. method. call
(define-judgment-form
  lang-data-flow
  #:mode (basic_blocks_stat_method_call_param I I I I I I O O)
  #:contract (basic_blocks_stat_method_call_param C C jumpoint cfg e bblockid
                                                  cfg (ctrlfinfo ...))

  [(basic_blocks (in-hole C_1 ($statFunCall prefixexp : Name (e_2 ... hole)))
                 jumpoint cfg_1 e_3 bblockid cfg_2 (ctrlfinfo ...))
   --------------------------------------------------------------------
   (basic_blocks_stat_method_call_param C_1
                                        ($statFunCall prefixexp : Name
                                                      (e_2 ... hole))
                                        jumpoint cfg_1 e_3 bblockid cfg_2
                                        (ctrlfinfo ...))]

  [(basic_blocks (in-hole C_1 ($statFunCall prefixexp : Name 
                                            (e_2 ... hole e_3 e_4 ...)))
                 jumpoint cfg_1 e_5 bblockid cfg_2 (ctrlfinfo_1 ...))

   (basic_blocks_stat_method_call_param
    C_1
    ($statFunCall prefixexp : Name 
                  (e_2 ... e_5 hole e_4 ...))
    jumpoint cfg_2 e_3 bblockid cfg_3 (ctrlfinfo_2 ...))
   --------------------------------------------------------------------
   (basic_blocks_stat_method_call_param C_1
                                        ($statFunCall prefixexp : Name 
                                                      (e_2 ... hole e_3 e_4 ...)
                                                      )
                                        jumpoint
                                        cfg_1
                                        e_5 bblockid cfg_3 (ctrlfinfo_1 ...
                                                            ctrlfinfo_2 ...))]
  )


; list of parameters in method calls
; TODO: any way of abstracting this pattern into a single formal system?
(define-judgment-form
  lang-data-flow
  #:mode (basic_blocks_method_call_param I I I I I I O O)
  #:contract (basic_blocks_method_call_param C C jumpoint cfg e bblockid cfg
                                             (ctrlfinfo ...))

  [(basic_blocks (in-hole C_1 (prefixexp : Name (e_2 ... hole))) jumpoint cfg_1
                 e_3 bblockid
                 cfg_2 (ctrlfinfo ...))
   --------------------------------------------------------------------
   (basic_blocks_method_call_param C_1 (prefixexp : Name (e_2 ... hole))
                                   jumpoint cfg_1
                                   e_3 bblockid cfg_2 (ctrlfinfo ...))]

  [(basic_blocks (in-hole C_1 (prefixexp : Name (e_2 ... hole e_3 e_4 ...)))
                 jumpoint cfg_1 e_5 bblockid cfg_2 (ctrlfinfo_1 ...))

   (basic_blocks_method_call_param
    C_1 (prefixexp : Name (e_2 ... e_5 hole e_4 ...)) jumpoint cfg_2 e_3
    bblockid cfg_3 (ctrlfinfo_2 ...))
   --------------------------------------------------------------------
   (basic_blocks_method_call_param
    C_1 (prefixexp : Name (e_2 ... hole e_3 e_4 ...)) jumpoint cfg_1 e_5
    bblockid cfg_3 (ctrlfinfo_1 ... ctrlfinfo_2 ...))]
  )

; list of params of builtIn
(define-judgment-form
  lang-data-flow
  #:mode (basic_blocks_builtin_param I I I I I I O O)
  #:contract (basic_blocks_builtin_param C C jumpoint cfg e bblockid cfg
                                         (ctrlfinfo ...))

  [(basic_blocks (in-hole C_1 ($builtIn builtinserv (e_2 ... hole))) jumpoint
                 cfg_1 e_3 bblockid
                 cfg_2 (ctrlfinfo ...))
   --------------------------------------------------------------------
   (basic_blocks_builtin_param C_1 ($builtIn builtinserv (e_2 ... hole))
                               jumpoint cfg_1
                               e_3 bblockid cfg_2 (ctrlfinfo ...))]

  [(basic_blocks (in-hole C_1 ($builtIn builtinserv (e_2 ... hole e_3 e_4 ...)))
                 jumpoint
                 cfg_1 e_5 bblockid cfg_2 (ctrlfinfo_1 ...))

   (basic_blocks_builtin_param
    C_1 ($builtIn builtinserv (e_2 ... e_5 hole e_4 ...)) jumpoint cfg_2 e_3
    bblockid cfg_3 (ctrlfinfo_2 ...))
   --------------------------------------------------------------------
   (basic_blocks_builtin_param
    C_1 ($builtIn builtinserv (e_2 ... hole e_3 e_4 ...)) jumpoint cfg_1 e_5
    bblockid cfg_3 (ctrlfinfo_1 ... ctrlfinfo_2 ...))]
  )

; fields of table constructor
(define-judgment-form
  lang-data-flow
  #:mode (basic_blocks_fields I I I I I I O O)
  #:contract (basic_blocks_fields C C jumpoint cfg (\[ e \] = e) bblockid cfg
                                  (ctrlfinfo ...))

  [(basic_blocks (in-hole C_1 (\{ field ... (\[ hole \] = e_2) \}))
                 jumpoint cfg_1 e_1 bblockid
                 cfg_2 (ctrlfinfo_1 ...))

   (basic_blocks (in-hole C_1 (\{ field ... (\[ e_1 \] = hole) \}))
                 jumpoint cfg_2 e_2
                 bblockid
                 cfg_3 (ctrlfinfo_2 ...))
   --------------------------------------------------------------------
   (basic_blocks_fields C_1 (\{ field ... hole \}) jumpoint
                        cfg_1
                        (\[ e_1 \] = e_2) bblockid cfg_3
                        (ctrlfinfo_1 ... ctrlfinfo_2 ...))]

  [(basic_blocks (in-hole C_1 (\{ field_1 ... (\[ hole \] = e_2)
                                  field_2 field_3 ... \}))
                 jumpoint cfg_1 e_1 bblockid cfg_2 (ctrlfinfo_1 ...))

   (basic_blocks (in-hole C_1 (\{ field_1 ... (\[ e_1 \] = hole) field_2
                                  field_3 ... \}))
                 jumpoint cfg_2 e_2 bblockid cfg_3 (ctrlfinfo_2 ...))

   (basic_blocks_fields
    C_1 (\{ field_1 ... (\[ e_1 \] = e_2) hole field_3 ... \}) jumpoint cfg_3
    field_2 bblockid cfg_4 (ctrlfinfo_3 ...))
   --------------------------------------------------------------------
   (basic_blocks_fields
    C_1 (\{ field_1 ... hole field_2 field_3 ... \}) jumpoint cfg_1
    (\[ e_1 \] = e_2) bblockid
    cfg_4 (ctrlfinfo_1 ... ctrlfinfo_2 ... ctrlfinfo_3 ...))]
  )



; extends a given cfg, for a given concat stat
(define-judgment-form
  lang-data-flow
  #:mode (basic_blocks_concat_stats I I I I I I O O )
  ; contract:
  ; C_1 : context of the concat stat
  ; C_2 : context of s into the concat stat
  ; any : 
  ; cfg_1 : the actual cfg
  ; s : the actual s from the concat stat
  ; bblockid : the id of the actual bblock
  ; cfg_2 : the obtained cfg
  ; (ctrlfinfo ...) : id of the added bblocks, together with the points to
  ; which the control flow should jump, after aterm
  #:contract (basic_blocks_concat_stats C C jumpoint cfg score bblockid cfg
                                        (ctrlfinfo ...))

  ; at least one statement left
  [(where (score_2 ... hole score_3 score_4 ...) C_2)
   (where score_1 s)
   
   (basic_blocks (in-hole C_1 (score_2 ... hole score_3 score_4 ...))
                 jumpoint
                 cfg_1
                 score_1
                 bblockid_1
                 cfg_2
                 ; continue in the same block
                 ((continue bblockid_1)))

   (basic_blocks_concat_stats C_1 (score_2 ... score_1 hole score_4 ...)
                              jumpoint
                              cfg_2
                              score_3
                              bblockid_1
                              cfg_3
                              (ctrlfinfo_1 ...))

   ; filter ctrlfinfo that represent jumps to other points into the program
   ;   (where (ctrlfinfo_2 ...) (filter_ctrlfinfo (ctrlfinfo_1 ...)
   ;                                              (in-hole C_1
   ;                                                       (score_2 ... hole score_3
   ;                                                                score_4 ...)
   ;                                                       )))
   --------------------------------------------------------------------
   (basic_blocks_concat_stats C_1
                              C_2
                              jumpoint
                              cfg_1
                              s
                              bblockid_1
                              cfg_3
                              (ctrlfinfo_1 ...
                               ;ctrlfinfo_2 ...
                               ))]


  ; no statement left
  [(where (score_2 ... hole) C_2)
   (where score_1 s)
   (basic_blocks (in-hole C_1 (score_2 ... hole))
                 jumpoint
                 cfg_1
                 score_1
                 bblockid_1
                 cfg_2
                 (ctrlfinfo_1 ...) ; continue in the same block
                 )

   ; transform (continue bblockid) to (newbblock bblockid)
   ;(where (ctrlfinfo_2 ...) (add_jump_next_bblock (ctrlfinfo_1 ...)))
   --------------------------------------------------------------------
   (basic_blocks_concat_stats C_1
                              C_2
                              jumpoint
                              cfg_1
                              s bblockid_1
                              cfg_2
                              (ctrlfinfo_1 ...)
                              )]

  ; at least one statement left
  [(where (score_2 ... hole score_3 score_4 ...) C_2)
   (where score_1 s)
   (where C_3 (score_2 ... score_1 hole score_4 ...))
   
   (basic_blocks (in-hole C_1 (score_2 ... hole score_3 score_4 ...))
                 jumpoint
                 cfg_1
                 score_1
                 bblockid_1
                 cfg_2
                 (ctrlfinfo_1 ...
                  (bblockid_2 C_4;(in-hole C_1 C_3)
                              )
                  ctrlfinfo_2 ...
                  )
                 )
   
   ; new basic-block for the next stat
   (where ((bblock_1 (bblockid_3 ...)) ...
           ((bblockid_4 (node ...)) (bblockid_5 ...))) ; get last bblockid
          cfg_2)
   (where bblockid_6 ,(+ 1 (term bblockid_4)))
   (where bblock (bblockid_6 ()))
   ; add bblock
   (where cfg_3 (add_bblock cfg_2 bblock))
   ; update links between basic blocks:
   (where (cfg_4 (ctrlfinfo_3 ...))
          (add_bblock_edge cfg_3
                           (ctrlfinfo_1 ...
                            (bblockid_2 C_4)
                            ctrlfinfo_2 ...)
                           (bblockid_6)
                           ((in-hole C_1 C_3))))

   (basic_blocks_concat_stats C_1 C_3
                              jumpoint
                              cfg_4
                              score_3
                              bblockid_6
                              cfg_5
                              (ctrlfinfo_4 ...))

   ; filter ctrlfinfo that represent jumps to other points into the program
   ;   (where (ctrlfinfo_4 ...)
   ;          (filter_ctrlfinfo (ctrlfinfo_1 ...
   ;                             ctrlfinfo_2 ...)
   ;                            
   ;                            (in-hole C_1 (score_2 ... hole score_3
   ;                                                  score_4 ...))))
   --------------------------------------------------------------------
   (basic_blocks_concat_stats C_1 C_2
                              jumpoint
                              cfg_1
                              s
                              bblockid_1
                              cfg_5
                              (ctrlfinfo_3 ...
                               ctrlfinfo_4 ...
                               ))]

  )

(define-judgment-form
  lang-data-flow
  #:mode (basic_blocks I I I I I O O)
  ; contract:
  ; C_1 : context of aterm
  ; jumpoint : position of the instruction of the next bblock (for return, break,
  ;       etc) 
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
  #:contract (basic_blocks C jumpoint cfg aterm bblockid cfg (ctrlfinfo ...))
  
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
   (basic_blocks C jumpoint
                 cfg_1
                 v bblockid
                 cfg_2
                 ((continue bblockid)))]
  ; var
  [; new node
   (where node_2 (C Name))
  
   ; update cfg
   (where cfg_2 (add_node_bblock cfg_1 bblockid node_2))
   --------------------------------------------------------------------
   (basic_blocks C jumpoint cfg_1 Name bblockid cfg_2
                 ((continue bblockid)))]
  
  [; table exp.
   (basic_blocks (in-hole C (hole \[ e_2 \])) jumpoint cfg_1 e_1 bblockid
                 cfg_2 (ctrlfinfo_1 ...))
  
   ; index exp.
   (basic_blocks (in-hole C (e_1 \[ hole \])) jumpoint cfg_2 e_2 bblockid cfg_3
                 (ctrlfinfo_2 ...))
  
   ; new node for the whole expr..
   (where node (C (e_1 \[ e_2 \])))
  
   ; update cfg
   (where cfg_4 (add_node_bblock cfg_3 bblockid node))

   (where (ctrlfinfo_4 ...)
          ,(remove-duplicates (term (; ctrlfinfo: continue after this
                                     ; instruction
                                     (continue bblockid)
                                     ctrlfinfo_1 ...
                                     ctrlfinfo_2 ...))))
   --------------------------------------------------------------------
   (basic_blocks C jumpoint cfg_1 (e_1 \[ e_2 \]) bblockid cfg_4
                 (ctrlfinfo_4 ...))]
  
  ; vararg
  [; new node
   (where node (C <<<))
  
   ; update cfg
   (where cfg_2 (add_node_bblock cfg_1 bblockid node))
   --------------------------------------------------------------------
   (basic_blocks C jumpoint cfg_1 <<< bblockid cfg_2
                 ((continue bblockid)))]
  
  ; functioncall
  [; function
   (basic_blocks (in-hole C (hole ())) jumpoint cfg_1 prefixexp bblockid
                 cfg_2 (ctrlfinfo_1 ...)
                 )
                     
   ; new node for the whole expr..
   (where node (C (prefixexp ())))
  
   ; update cfg
   (where cfg_3 (add_node_bblock cfg_2 bblockid node))
   --------------------------------------------------------------------
   (basic_blocks C jumpoint cfg_1 (prefixexp ()) bblockid cfg_3
                 (; TODO: not allowing fcalls to change execution order
                  (continue bblockid)))]
  
    
  [; function
   (basic_blocks (in-hole C (hole (e_1 e_2 ...))) jumpoint cfg_1 prefixexp
                 bblockid cfg_2 (ctrlfinfo_1 ...)
                 )
  
   ; params
   (basic_blocks_fun_call_param C (prefixexp (hole e_2 ... )) jumpoint cfg_2
                                e_1 bblockid cfg_3 (ctrlfinfo_2 ...))
                     
   ; new node for the whole expr..
   (where node (C (prefixexp (e_1 e_2 ...))))
  
   ; update cfg
   (where cfg_4 (add_node_bblock cfg_3 bblockid node))
   --------------------------------------------------------------------
   (basic_blocks C jumpoint cfg_1 (prefixexp (e_1 e_2 ...)) bblockid cfg_4
                 (; TODO: not allowing fcalls to change execution order
                  (continue bblockid)
                  )
                 )]
  
  ; method call
  [; function
   (basic_blocks (in-hole C (hole : Name ())) jumpoint cfg_1 prefixexp
                 bblockid cfg_2 (ctrlfinfo_1 ...)
                 )
                     
   ; new node for the whole expr..
   (where node (C (prefixexp : Name ())))
  
   ; update cfg
   (where cfg_3 (add_node_bblock cfg_2 bblockid node))
   --------------------------------------------------------------------
   (basic_blocks C jumpoint cfg_1 (prefixexp : Name ()) bblockid cfg_3
                 (; TODO: not allowing fcalls to change execution order
                  (continue bblockid)))]
  
    
  [; function
   (basic_blocks (in-hole C (hole : Name (e_1 e_2 ...)))
                 jumpoint cfg_1 prefixexp
                 bblockid cfg_2
                 (ctrlfinfo_1 ...)
                 )
  
   ; params
   (basic_blocks_method_call_param C (prefixexp : Name (hole e_2 ... ))
                                   jumpoint cfg_2
                                   e_1 bblockid cfg_3 (ctrlfinfo_2 ...))
                     
   ; new node for the whole expr..
   (where node (C (prefixexp : Name (e_1 e_2 ...))))
  
   ; update cfg
   (where cfg_4 (add_node_bblock cfg_3 bblockid node))
   --------------------------------------------------------------------
   (basic_blocks C jumpoint cfg_1 (prefixexp : Name (e_1 e_2 ...)) bblockid
                 cfg_4
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
   (basic_blocks C jumpoint cfg_1 ($builtIn builtinserv ()) bblockid cfg_2
                 (; built-in calls do not change execution order
                  (continue bblockid)))]
  
    
  [; params
   (basic_blocks_builtin_param C ($builtIn builtinserv (hole e_2 ...))
                               jumpoint cfg_1
                               e_1 bblockid cfg_2 (ctrlfinfo_1 ...))
                     
   ; new node for the whole expr..
   (where node (C ($builtIn builtinserv (e_1 e_2 ...))))
  
   ; update cfg
   (where cfg_3 (add_node_bblock cfg_2 bblockid node))

   (where (ctrlfinfo_2 ...)
          ,(remove-duplicates
            (term (; built-in calls do not change execution order
                   (continue bblockid)
                   ctrlfinfo_1 ...))))
   --------------------------------------------------------------------
   (basic_blocks C jumpoint cfg_1 ($builtIn builtinserv (e_1 e_2 ...))
                 bblockid cfg_3
                 (ctrlfinfo_2 ...))]
  
  ; parent. exp
  [(basic_blocks (in-hole C (\( hole \))) jumpoint cfg_1 e bblockid cfg_2
                 (ctrlfinfo_1 ...)
                 )
   ; new node
   (where node (C (\( e \))))
  
   ; update cfg
   (where cfg_3 (add_node_bblock cfg_2 bblockid node))

   (where (ctrlfinfo_2 ...)
          ,(remove-duplicates
            (term (; parenthesized exps. do not change execution order
                   (continue bblockid)
                   ctrlfinfo_1 ...))))
   --------------------------------------------------------------------
   (basic_blocks C jumpoint cfg_1 (\( e \)) bblockid cfg_3
                 (ctrlfinfo_2 ...))]
  
  ; table constructor
  [; new node
   (where node (C (\{ \})))
  
   ; update cfg
   (where cfg_2 (add_node_bblock cfg_1 bblockid node))
   --------------------------------------------------------------------
   (basic_blocks C jumpoint cfg_1 (\{ \}) bblockid cfg_2
                 (; table cons. do not change execution order
                  (continue bblockid)))]
  
  [(basic_blocks_fields C (\{ hole field ... \}) jumpoint
                        cfg_1 (\[ e_1 \] = e_2) bblockid cfg_2
                        (ctrlfinfo_1 ...))
   ; new node
   (where node (C (\{ (\[ e_1 \] = e_2) field ... \})))
  
   ; update cfg
   (where cfg_3 (add_node_bblock cfg_2 bblockid node))

   (where (ctrlfinfo_2 ...)
          ,(remove-duplicates
            (term (; table cons. do not change execution order
                   (continue bblockid)
                   ctrlfinfo_1 ...))))
   --------------------------------------------------------------------
   (basic_blocks C jumpoint cfg_1 (\{ (\[ e_1 \] = e_2) field ... \})
                 bblockid cfg_3 (ctrlfinfo_2 ...))]
  
  ; binary op
  [(basic_blocks (in-hole C (hole binop e_2)) jumpoint cfg_1 e_1 bblockid
                 cfg_2 (ctrlfinfo_1 ...)
                 )
  
   (basic_blocks (in-hole C (e_1 binop hole)) jumpoint cfg_2 e_2 bblockid
                 cfg_3 (ctrlfinfo_2 ...)
                 )
   ; new node
   (where node (C (e_1 binop e_2)))
  
   ; update cfg
   (where cfg_4 (add_node_bblock cfg_3 bblockid node))

   (where (ctrlfinfo_3 ...)
          ,(remove-duplicates
            (term (; binop does not change execution order
                   (continue bblockid)
                   ctrlfinfo_1 ...
                   ctrlfinfo_2 ...))))
   --------------------------------------------------------------------
   (basic_blocks C jumpoint cfg_1 (e_1 binop e_2) bblockid cfg_4
                 (ctrlfinfo_3 ...))]
  
  ; unop
  [(basic_blocks (in-hole C (unop hole)) jumpoint cfg_1 e bblockid cfg_2
                 (ctrlfinfo_1 ...)
                 )
  
   ; new node
   (where node (C (unop e)))
  
   ; update cfg
   (where cfg_3 (add_node_bblock cfg_2 bblockid node))

   (where (ctrlfinfo_2 ...)
          ,(remove-duplicates
            (term (; unop does not change execution order
                   (continue bblockid)
                   ctrlfinfo_1 ...))))
   --------------------------------------------------------------------
   (basic_blocks C jumpoint cfg_1 (unop e) bblockid cfg_3
                 (ctrlfinfo_2 ...))]
  
  ; functiondef
  [; new node
   (where node (C functiondef))
  
   ; update cfg
   (where cfg_2 (add_node_bblock cfg_1 bblockid node))
   --------------------------------------------------------------------
   (basic_blocks C jumpoint cfg_1 functiondef bblockid cfg_2
                 (; fdef does not change execution order
                  (continue bblockid)))]

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
   (basic_blocks C jumpoint
                 cfg_1
                 \;
                 bblockid
                 cfg_2
                 ((continue bblockid)))]

  ; return
  [; new node
   (where node (C (return)))
  
   ; update cfg
   (where cfg_2 (add_node_bblock cfg_1 bblockid node))
   --------------------------------------------------------------------
   (basic_blocks C jumpoint cfg_1 (return) bblockid cfg_2
                 (; return a jump to jumpoint
                  ; TODO: implement correct jumpoint
                  (bblockid jumpoint)
                  ))]
    
  [(basic_blocks_return C (return hole e_2 ...) jumpoint cfg_1 e_1 bblockid
                        cfg_2 (ctrlfinfo_1 ...))
     
   ; new node
   (where node (C (return e_1 e_2 ...)))
  
   ; update cfg
   (where cfg_3 (add_node_bblock cfg_2 bblockid node))

   (where (ctrlfinfo_2 ...)
          ,(remove-duplicates
            (term (; return a jump to jumpoint
                   ; TODO: implement correct jumpoint
                   (bblockid jumpoint)
                   ctrlfinfo_1 ...))))
   --------------------------------------------------------------------
   (basic_blocks C jumpoint cfg_1 (return e_1 e_2 ...) bblockid cfg_3
                 (ctrlfinfo_2 ...))]
    

  ; funcall
  [; function
   (basic_blocks (in-hole C ($statFunCall hole ())) jumpoint cfg_1 prefixexp
                 bblockid cfg_2
                 (ctrlfinfo_1 ...)
                 )
                     
   ; new node for the whole expr..
   (where node (C ($statFunCall prefixexp ())))
  
   ; update cfg
   (where cfg_3 (add_node_bblock cfg_2 bblockid node))

   (where (ctrlfinfo_2 ...)
          ,(remove-duplicates
            (term (; TODO: not allowing fcalls to change execution order
                   (continue bblockid)
                   ctrlfinfo_1 ...))))
   --------------------------------------------------------------------
   (basic_blocks C jumpoint cfg_1 ($statFunCall prefixexp ()) bblockid cfg_3
                 (ctrlfinfo_2 ...))]
  
    
  [; function
   (basic_blocks (in-hole C ($statFunCall hole (e_1 e_2 ...))) jumpoint cfg_1
                 prefixexp bblockid cfg_2 (ctrlfinfo_1 ...)
                 )
  
   ; params
   (basic_blocks_stat_fun_call_param C ($statFunCall prefixexp (hole e_2 ... ))
                                     jumpoint cfg_2
                                     e_1 bblockid cfg_3 (ctrlfinfo_2 ...))
                     
   ; new node for the whole expr..
   (where node (C ($statFunCall prefixexp (e_1 e_2 ...))))
  
   ; update cfg
   (where cfg_4 (add_node_bblock cfg_3 bblockid node))

   (where (ctrlfinfo_3 ...)
          ,(remove-duplicates
            (term (; TODO: not allowing fcalls to change execution order
                   (continue bblockid)
                   ctrlfinfo_1 ...
                   ctrlfinfo_2 ...))))
   --------------------------------------------------------------------
   (basic_blocks C jumpoint cfg_1 ($statFunCall prefixexp (e_1 e_2 ...))
                 bblockid cfg_4
                 (ctrlfinfo_3 ...)
                 )]
  ; method call
  [; function
   (basic_blocks (in-hole C ($statFunCall hole : Name ())) jumpoint
                 cfg_1 prefixexp bblockid cfg_2 (ctrlfinfo_1 ...))
   
   ; new node
   (where node_2 (C ($statFunCall prefixexp : Name ())))
  
   ; update cfg
   (where cfg_3 (add_node_bblock cfg_2 bblockid node_2))

   (where (ctrlfinfo_2 ...)
          ,(remove-duplicates
            (term (; TODO: not allowing method call to change execution order
                   (continue bblockid)
                   ctrlfinfo_1 ...))))
   --------------------------------------------------------------------
   (basic_blocks C jumpoint cfg_1 ($statFunCall prefixexp : Name ())
                 bblockid cfg_2 (ctrlfinfo_2 ...)
                 )]
  
  [; method
   (basic_blocks (in-hole C ($statFunCall hole : Name (e_1 e_2 ...))) jumpoint
                 cfg_1 prefixexp bblockid cfg_2 (ctrlfinfo_1 ...)
                 )
  
   ; params
   (basic_blocks_stat_method_call_param C ($statFunCall prefixexp : Name
                                                        (hole e_2 ... ))
                                        jumpoint cfg_2
                                        e_1 bblockid cfg_3 (ctrlfinfo_2 ...))
   
   ; new node
   (where node_2 (C ($statFunCall prefixexp : Name (e_1 e_2 ...))))
  
   ; update cfg
   (where cfg_4 (add_node_bblock cfg_3 bblockid node_2))

   (where (ctrlfinfo_3 ...)
          ,(remove-duplicates
            (term (; TODO: not allowing method call to change execution order
                   (continue bblockid)
                   ctrlfinfo_1 ...
                   ctrlfinfo_2 ...))))
   --------------------------------------------------------------------
   (basic_blocks C jumpoint cfg_1 ($statFunCall prefixexp : Name (e_1 e_2 ...))
                 bblockid cfg_4 (ctrlfinfo_3 ...))]
  

  ; assign
  [(basic_blocks_assign C (hole var_2 ... = e ...) jumpoint cfg_1 var_1
                        bblockid cfg_2 (ctrlfinfo_1 ...))
   ; new node
   (where node (C (var_1 var_2 ... = e ...)))
  
   ; update cfg
   (where cfg_3 (add_node_bblock cfg_2 bblockid node))

   (where (ctrlfinfo_2 ...)
          ,(remove-duplicates
            (term (; assign does not change execution order
                   (continue bblockid)
                   ctrlfinfo_1 ...))))
   --------------------------------------------------------------------
   (basic_blocks C jumpoint cfg_1 (var_1 var_2 ... = e ...) bblockid cfg_3
                 (ctrlfinfo_2 ...))]


  ; do-end
  [; new node
   (where node (C (do s end)))
  
   ; update cfg
   (where cfg_2 (add_node_bblock cfg_1 bblockid node))
  
   ; body
   (basic_blocks (in-hole C (do hole end)) jumpoint
                 cfg_2
                 s
                 bblockid cfg_3
                 (ctrlfinfo_1 ...))

   (where (ctrlfinfo_2 ...)
          ,(remove-duplicates
            (term (; do-end does not change execution order
                   (continue bblockid)
                   ctrlfinfo_1 ...))))
   --------------------------------------------------------------------
   (basic_blocks C jumpoint cfg_1 (do s end) bblockid cfg_3
                 (ctrlfinfo_2 ...))]
    

  
  ; conditional
  ; if e then s_1 else s_2 end ->
  ;               if e then jump if_branch
  ; else_branch : s_2
  ;               jump cont
  ; if_branch   : s_1
  ; cont        : ...
  [; guard
   (basic_blocks (in-hole C (if hole then s_1 else s_2 end))
                 jumpoint_1
                 cfg_1
                 e
                 bblockid_1 cfg_2
                 (ctrlfinfo_1 ...))

   (where (ctrlfinfo_2 ...) (remove_cont (ctrlfinfo_1 ...)))

   ; delete every ctrlfinfo of the form (continue ...): the guard has a
   ; conditional jump

   ; get pos of the next instruction
   (where jumpoint_2 (next_pos C (if e then s_1 else s_2 end)))
     
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
   (where (cfg_4 ())
          (add_bblock_edge cfg_3
                           ((bblockid_1
                             (in-hole C (if e then hole else s_2 end))))
                           (bblockid_3)
                           ((in-hole C (if e then hole else s_2 end)))))
  
   (basic_blocks (in-hole C (if e then hole else s_2 end))
                 jumpoint_2
                 cfg_4
                 s_1
                 bblockid_3 cfg_5
                 (ctrlfinfo_3 ...))
  
   ; transform (continue bblockid) into a jump to the next block 
   (where (ctrlfinfo_4 ...) (convert_to_jump (ctrlfinfo_3 ...) jumpoint_2))
  
   ; else branch
   ; jump from guard to else branch: new basic block new basic block
   (where (_ ...
           ((bblockid_4 _) _)) ; get last bblockid
          cfg_5)
   (where bblockid_5 ,(+ 1 (term bblockid_4)))
   (where bblock_5 (bblockid_5 ()))
   ; add bblock
   (where cfg_6 (add_bblock cfg_5 bblock_5))
   ; update links between basic blocks: add a jump from the guard to the else
   ; branch
   (where (cfg_7 ())
          (add_bblock_edge cfg_6
                           ((bblockid_1
                             (in-hole C (if e then s_1 else hole end))))
                           (bblockid_5)
                           ((in-hole C (if e then s_1 else hole end)))))
  
   (basic_blocks (in-hole C (if e then s_1 else hole end))
                 jumpoint_2
                 cfg_7
                 s_2
                 bblockid_5 cfg_8
                 (ctrlfinfo_5 ...))
     
   ; transform (continue bblockid) into a jump to the next block 
   (where (ctrlfinfo_6 ...) (convert_to_jump (ctrlfinfo_5 ...) jumpoint_2))

   (where (ctrlfinfo_7 ...)
          ,(remove-duplicates (term (ctrlfinfo_2 ...
                                     ctrlfinfo_4 ...
                                     ctrlfinfo_6 ...))))
   --------------------------------------------------------------------
   (basic_blocks C jumpoint_1 cfg_1 (if e then s_1 else s_2 end) bblockid_1
                 cfg_8
                 (ctrlfinfo_7 ...))]
    
  ; concat stats
  [(where (score_1 score_2 score_3 ...) s)
   (basic_blocks_concat_stats C
                              (hole score_2 score_3 ...)
                              jumpoint
                              cfg_1
                              score_1
                              bblockid_1
                              cfg_2
                              (ctrlfinfo ...))
   --------------------------------------------------------------------
   (basic_blocks C jumpoint
                 cfg_1
                 s
                 bblockid_1
                 cfg_2
                 (ctrlfinfo ...))]

  ; while loop
  ; while e do s end ->
  ; guard : if not e jump end
  ; body  : s
  ;         jump guard
  ; end   : ...
  [; new node for the guard
;   (where C_2 (in-hole C (while hole do s end))) ; node id
;   (where node_2 (C_2 e))
;
;   ; add new node to the actual bblock
;   (where cfg_2 (add_node_bblock cfg_1 bblockid_1 node_2))

   (basic_blocks (in-hole C (while hole do s end))
                 jumpoint_1
                 cfg_1
                 e
                 bblockid_1
                 cfg_2
                 (ctrlfinfo_1 ...))
   
   ; body
   ; new basic block
   (where ((bblock (bblockid_2 ...)) ...
           ((bblockid_3 (node ...)) (bblockid_4 ...))) ; get last bblockid
          cfg_2)
   (where bblockid_5 ,(+ 1 (term bblockid_3)))
   (where bblock_1 (bblockid_5 ()))
   ; add bblock
   (where cfg_3 (add_bblock cfg_2 bblock_1))
   ; update links between basic blocks: edge from the guard to the body
   (where (cfg_4 ())
          (add_bblock_edge cfg_3
                           ((bblockid_1 (in-hole C (while e do hole end))))
                           (bblockid_5)
                           ((in-hole C (while e do hole end)))))

   ; get pos of the next instruction
   (where jumpoint_2 (next_pos C (while e do s end)))
   ; cfg of the body
   (basic_blocks (in-hole C (while e do hole end))
                 jumpoint_2
                 cfg_4
                 s
                 bblockid_5
                 cfg_5
                 (ctrlfinfo_2 ...))

   ; convert (continue bblockid) into jumps to the guard: (bblockid C)
   (where (ctrlfinfo_3 ...)
          (convert_to_jump
           (ctrlfinfo_2 ...)
           (in-hole C (while hole do s end))
           )
          )
   
   ; add edges from the body to the guard: in case of jump, add_bblock_edge
   ; compares the target with the evaluation context C; if target and C are
   ; the same, an edge is added
   (where (cfg_6 (ctrlfinfo_4 ...))
          (add_bblock_edge cfg_5 (ctrlfinfo_3 ...)
                           (bblockid_1)
                           ((in-hole C (while hole do s end)))))

   ; filter jumps outside the loop (that is, delete jumps of the form
   ; (bblockid (in-hole C (while hole do s end))))
   ;   (where (ctrlfinfo_3 ...) (filter_ctrlfinfo
   ;                             (ctrlfinfo_2 ...)
   ;                             (in-hole C (while hole do s end))))
   --------------------------------------------------------------
   (basic_blocks C jumpoint_1
                 cfg_1
                 (while e do s end) bblockid_1
                 cfg_6
                 (; add a jump to the basic block following the guard of the
                  ; loop
                  (bblockid_1 jumpoint_2)
                  ctrlfinfo_4 ...
                  ))]
  
  ; break
  [; new node
   (where node (C break))

   ; add new node to the cfg
   (where cfg_2 (add_node_bblock cfg_1 bblockid_1 node))

   ; determine the point to which break must jump
   (where jumpoint_2 (break_jump C))
   --------------------------------------------------------------------
   (basic_blocks C jumpoint_1
                 cfg_1
                 break bblockid_1
                 cfg_2
                 (; return a change in the execution flow: jump to jumpoint
                  (bblockid_1 jumpoint_2)
                  ))]

  
  
    ; local var
    [(basic_blocks_loc_var C (local (Name : t) ... = hole e_2 ... in s end)
                           jumpoint cfg_1
                           e_1 bblockid cfg_2 (ctrlfinfo_1 ...))
  
;     ; filter jumps of the form (bblockid C)
;     (where (ctrlfinfo_2 ...) (filter_bblockid_ctrlfinfo
;                               (ctrlfinfo_1 ...
;                                ;ctrlfinfo_2 ...
;                                )))
     (where (ctrlfinfo_2 ...)
            ,(remove-duplicates (term (; local var def does not change execution
                                       ; flow
                                       (continue bblockid)
                                       ctrlfinfo_1 ...))))
     --------------------------------------------------------------------
     (basic_blocks C jumpoint cfg_1
                   (local (Name : t) ... = e_1 e_2 ... in s end)
                   bblockid cfg_2
                   (ctrlfinfo_2 ...))]
  
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