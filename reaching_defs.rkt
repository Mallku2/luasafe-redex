#lang racket

(require redex
         "./grammar.rkt"
         "./data_flow_analysis.rkt")

(define-extended-language lang-reach-defs lang-data-flow

  ; id - nodes of the block - in defs - out defs
  [bblockKG (bblockid (node ...) ((Name = e) ...) ((Name = e) ...))]
  
  ; cfg enriched with kill and gen sets
  [cfgKG ((bblockKG (bblockid ...)) ...)]

  ; in-out sets, with a tree-like rep
  [ioTree (((Name = e) ...) ioTree)
          empty]
  
  [bblockIO (bblockid (node ...) ioTree ioTree)]
  
  ; cfg enriched with in and out sets, represented in a tree-like form
  [cfgIO ((bblockIO (bblockid ...)) ...)]

  )

(provide lang-reach-defs)

;                                                                          
;                                                                          
;                                                                          
;   ;           ;    ;;;;     ;;;;                                         
;   ;           ;       ;        ;                                         
;   ;                   ;        ;                                         
;   ;    ;    ;;;       ;        ;                 ;;; ;    ;;;    ; ;;;;  
;   ;  ;;       ;       ;        ;                ;   ;;   ;   ;   ;;   ;; 
;   ; ;         ;       ;        ;               ;     ;  ;     ;  ;     ; 
;   ;;;         ;       ;        ;               ;     ;  ;     ;  ;     ; 
;   ;  ;        ;       ;        ;               ;     ;  ;;;;;;;  ;     ; 
;   ;   ;       ;       ;        ;               ;     ;  ;        ;     ; 
;   ;    ;      ;       ;        ;                ;   ;;   ;    ;  ;     ; 
;   ;     ;  ;;;;;;;     ;;;      ;;;              ;;; ;    ;;;;   ;     ; 
;                                                      ;                   
;                                                 ;   ;;                   
;                                                  ;;;;                    
;                                                                          

(define-metafunction lang-reach-defs
  compute_KG : cfg -> cfgKG

  [(compute_KG ((bblock_1 (bblockid_1 ...)) ...
                ((bblockid_2 (node ...)) (bblockid_3 ...))))
   cfgKG_2

   ; to increase the precision of reaching defs.: decompose a single bblock into
   ; several bblocks, one per node
   (where cfg_1 (rename_ids_bblock ((bblock_1 (bblockid_1 ...)) ...
                                    ((bblockid_2 (node ...)) (bblockid_3 ...)))
                                   ()
                                   bblockid_2))

   (where cfg_2 (flatten_cfg cfg_1 ()))
   
   ; first, compute genB
   (where cfgKG_1 (compute_gen cfg_2 cfg_2 ()))

   ; then, compute killB
   (where cfgKG_2 (compute_kill cfgKG_1 cfgKG_1 ()))
   ])

(provide compute_KG)


(define-metafunction lang-reach-defs
  rename_id : cfg bblockid bblockid cfg -> cfg

  [(rename_id () _ _ cfg)
   cfg]
  
   
  [(rename_id (((bblockid_1 (node ...))
                (bblockid_2 ... bblockid_3 bblockid_4 ...))
               (bblock_1 (bblockid_5 ...)) ...)
              bblockid_3 bblockid_7
              ((bblock_2 (bblockid_8 ...)) ...)
              )

   (rename_id ((bblock_1 (bblockid_5 ...)) ...)
              bblockid_3 bblockid_7
              ((bblock_2 (bblockid_8 ...)) ...
               ((bblockid_1 (node ...))
                (bblockid_2 ... bblockid_7 bblockid_4 ...)))
              )
   ]

  ; no edge to bblockid
  [(rename_id ((bblock_1 (bblockid_1 ...))
               (bblock_2 (bblockid_2 ...)) ...)
              bblockid_3 bblockid_4
              ((bblock_5 (bblockid_6 ...)) ...)
              )

   (rename_id ((bblock_2 (bblockid_2 ...)) ...)
              bblockid_3 bblockid_4
              ((bblock_5 (bblockid_6 ...)) ...
               (bblock_1 (bblockid_1 ...)))
              )
   ]
  )

(define-metafunction lang-reach-defs
  rename_ids_bblock : cfg cfg Number -> cfg
  
  [(rename_ids_bblock () cfg _)
   cfg
   ]
  
  [(rename_ids_bblock (((bblockid_1 (node ...))
                        (bblockid_2 ...))
                       (bblock_1 (bblockid_3 ...)) ...)
                      ((bblock_2 (bblockid_4 ...)) ...)
                      Number_1)

   (rename_ids_bblock ((bblock_3 (bblockid_5 ...)) ...)
                      ((bblock_5 (bblockid_6 ...)) ...
                       ((Number_1 (node ...))
                        (bblockid_2 ...)))
                      Number_2)

   (where Number_2 ,(+ (term Number_1)
                       (length (term (node ...)))
                       ))

   ; rename id in remaining bblockKGs
   (where ((bblock_3 (bblockid_5 ...)) ...)
          (rename_id ((bblock_1 (bblockid_3 ...)) ...) bblockid_1 Number_1
                     ()))

   (where ((bblock_5 (bblockid_6 ...)) ...)
          (rename_id ((bblock_2 (bblockid_4 ...)) ...) bblockid_1 Number_1
                     ()))
   ]
  )


(define-metafunction lang-reach-defs
  flatten_cfg : cfg cfg -> cfg

  [(flatten_cfg () cfg)
   cfg
   ]

  [(flatten_cfg (((bblockid_1 (node ...))
                  (bblockid_2 ...))
                 (bblock_1 (bblockid_3 ...)) ...)

                ((bblock_2 (bblockid_4 ...)) ...))

   (flatten_cfg ((bblock_1 (bblockid_3 ...)) ...)

                ((bblock_2 (bblockid_4 ...)) ...
                 (bblock_3 (bblockid_5 ...)) ...))

   (where ((bblock_3 (bblockid_5 ...)) ...)
          
          (flatten_bblock  ((bblockid_1 (node ...))
                            (bblockid_2 ...))
                           ()
                           bblockid_1))

   ]
  )


; to increase the precision of reaching defs.: decompose a single bblock into
; several bblocks, one per node
(define-metafunction lang-reach-defs
  flatten_bblock : (bblock (bblockid ...)) cfg bblockid -> cfg

  [(flatten_bblock ((_ ()) _) cfg _)
   cfg
   ]

  ; not the last node
  [(flatten_bblock ((bblockid_1 (node_1))
                    (bblockid_2 ...))
                   ((bblock (bblockid_3 ...)) ...)
                   bblockid_4)
   
   ((bblock (bblockid_3 ...)) ...
    ((bblockid_4 (node_1))
     (bblockid_2 ...)))
   ]


  ; not the last node
  [(flatten_bblock ((bblockid_1 (node_1 node_2 ...))
                    (bblockid_2 ...))
                   ((bblock (bblockid_3 ...)) ...)
                   bblockid_4)
   
   (flatten_bblock ((bblockid_1 (node_2 ...))
                    (bblockid_2 ...))
                   ((bblock (bblockid_3 ...)) ...
                    ((bblockid_4 (node_1))
                     (bblockid_5)))
                   bblockid_5
                   )

   (where bblockid_5 ,(+ 1 (term bblockid_4)))
   ]
  )



(define-metafunction lang-reach-defs
  compute_kill : cfgKG cfgKG cfgKG -> cfgKG

  [(compute_kill cfgKG_1 () cfgKG_2)
   cfgKG_2
   ]

  [(compute_kill cfgKG (((bblockid_1 (node_1 ...)
                                     ((Name_1 = e_1) ...)
                                     ((Name_2 = e_2) ...))
                         (bblockid_2 ...))
                        (bblockKG_1 (bblockid_3 ...)) ...)
                 
                 ((bblockKG_2 (bblockid_4 ...)) ...))
   
   (compute_kill cfgKG ((bblockKG_1 (bblockid_3 ...)) ...)
                 ((bblockKG_2 (bblockid_4 ...)) ...
                  ((bblockid_1 (node_1 ...)
                               ((Name_3 = e_3) ...)
                               ((Name_2 = e_2) ...))
                   (bblockid_2 ...))))
   
   (where ((Name_3 = e_3) ...)
          (compute_kill_bblock cfgKG bblockid_1
                               ((Name_1 = e_1) ...)
                               ((Name_2 = e_2) ...)))]
  )

(define-metafunction lang-reach-defs
  compute_kill_bblock : cfgKG bblockid ((Name = e) ...) ((Name = e) ...) ->
  ((Name = e) ...)

  [(compute_kill_bblock () _ ((Name = e) ...) _)
   ((Name = e) ...)]

  ; var def. in gen kills another var def. from bblockid_1
  [(compute_kill_bblock (((bblockid_1 (node_1 ...)
                                      ((Name_1 = e_1) ...)
                                      ((Name_2 = e_2)
                                       (Name_3 = e_3) ...))
                          (bblockid_2 ...))
                         (bblockKG (bblockid_3 ...)) ...)

                        bblockid_4

                        ((Name_4 = e_4) ...)
                        ((Name_5 = e_5) ...
                         (Name_2 = e_6)
                         (Name_6 = e_7) ...))

   (compute_kill_bblock (((bblockid_1 (node_1 ...)
                                      ((Name_1 = e_1) ...)
                                      ((Name_3 = e_3) ...))
                          (bblockid_2 ...))
                         (bblockKG (bblockid_3 ...)) ...)

                        bblockid_4

                        ((Name_4 = e_4) ... (Name_2 = e_2))
                        ((Name_5 = e_5) ...
                         (Name_2 = e_6)
                         (Name_6 = e_7) ...))

   (side-condition (not (equal? (term bblockid_1)
                                (term bblockid_4))))
   ]

  [(compute_kill_bblock (((bblockid_1 (node_1 ...)
                                      ((Name_1 = e_1) ...)
                                      ((Name_2 = e_2) (Name_3 = e_3) ...))
                          (bblockid_2 ...))
                         (bblockKG (bblockid_3 ...)) ...)

                        bblockid_4

                        ((Name_4 = e_4) ...)
                        ((Name_5 = e_5) ...))

   (compute_kill_bblock (((bblockid_1 (node_1 ...)
                                      ((Name_1 = e_1) ...)
                                      ((Name_3 = e_3) ...))
                          (bblockid_2 ...))
                         (bblockKG (bblockid_3 ...)) ...)

                        bblockid_4

                        ((Name_4 = e_4) ...)
                        ((Name_5 = e_5) ...))

   (side-condition (not (equal? (term bblockid_1)
                                (term bblockid_4))))
   ]

  [(compute_kill_bblock (((bblockid_1 (node_1 ...)
                                      ((Name_1 = e_1) ...)
                                      ())
                          (bblockid_2 ...))
                         (bblockKG (bblockid_3 ...)) ...)

                        bblockid_4

                        ((Name_2 = e_2) ...)
                        ((Name_3 = e_3) ...))

   (compute_kill_bblock ((bblockKG (bblockid_3 ...)) ...)

                        bblockid_4

                        ((Name_2 = e_2) ...)
                        ((Name_3 = e_3) ...))

   (side-condition (not (equal? (term bblockid_1)
                                (term bblockid_4))))
   ]

  [(compute_kill_bblock (((bblockid_1 (node_1 ...)
                                      ((Name_1 = e_1) ...)
                                      ((Name_2 = e_2) ...))
                          (bblockid_2 ...))
                         (bblockKG (bblockid_3 ...)) ...)

                        bblockid_1

                        ((Name_3 = e_3) ...)
                        ((Name_4 = e_4) ...))

   (compute_kill_bblock ((bblockKG (bblockid_3 ...)) ...)

                        bblockid_1

                        ((Name_3 = e_3) ...)
                        ((Name_4 = e_4) ...))
   ]
  )

; Generate killB and genB for a given cfg
; PARAMS:
; cfg_1 : the cfg for which we want to compute kill and gen
; cfg_2 : instance of cfg_1 upon which the function iterates
;         (initially, call compute_gen with cfg_2 == cfg_1)
; cfgKG : the partial result (initially, call compute_gen with cfgKG == ())
(define-metafunction lang-reach-defs
  compute_gen : cfg cfg cfgKG -> cfgKG

  [(compute_gen cfg () cfgKG)
   cfgKG
   ]

  [(compute_gen cfg (((bblockid_1 (node_1 ...)) (bblockid_2 ...))
                     (bblock (bblockid_3 ...)) ...)
                ((bblockKG (bblockid_4 ...)) ...))
   
   (compute_gen cfg ((bblock (bblockid_3 ...)) ...)
                ((bblockKG (bblockid_4 ...)) ...
                 ((bblockid_1 (node_1 ...)
                              ((Name_1 = e_1) ...)
                              ((Name_2 = e_2) ...))
                  (bblockid_2 ...))))
   
   (where (bblockid_1 (node_1 ...)
                      ((Name_1 = e_1) ...)
                      ((Name_2 = e_2) ...))
          (compute_gen_bblock cfg (bblockid_1 (node_1 ...)) () ()))]
  )

; Generate killB and genB for a given bblock B
; PARAMS:
; cfg : the cfg to which the bblock belongs
; any : the bblock for which we want to compute the killB and genB sets; to
;       generate mult. single var def from a single mult. var def.
;       (Name ... = e ...), we iterate over it, eventually genrating ill formed
;       need: we need the pattern any instead of bblock
; ((Name = e) ...) : list of definitions killed by bblock
; ((Name = e) ...) : list of definitions gen by bblock
(define-metafunction lang-reach-defs
  compute_gen_bblock : cfg any ((Name = e) ...) ((Name = e) ...) -> bblockKG

  [(compute_gen_bblock (_ ...
                        ((bblockid_1 (node ...)) _)
                        _ ...)
                       (bblockid_1 ())
                       ((Name_1 = e_1) ...)
                       ((Name_2 = e_2) ...))
   (bblockid_1 (node ...)
               ((Name_1 = e_1) ...)
               ((Name_2 = e_2) ...))
   ]

  ; the new definition kills another def of the same var in the bblock
  [(compute_gen_bblock cfg (bblockid ((nodeId (Name_1 Name_2 ... =
                                                      e_1 e_2 ...))
                                      node ...))
                       ((Name_3 = e_3) ...)
                       ((Name_4 = e_4) ... (Name_1 = e_5) (Name_5 = e_6) ...))
   
   (compute_gen_bblock cfg (bblockid ((nodeId (Name_2 ... = e_2 ...))
                                      node ...))
                       ((Name_3 = e_3) ...
                        (Name_1 = e_5) ; this def is killed
                        )
                       ((Name_4 = e_4) ...
                        (Name_5 = e_6) ...
                        (Name_1 = e_1) ; this def is gen
                        ))]

  ; new definition, doesn't kill another def from the bblock
  [(compute_gen_bblock cfg (bblockid ((nodeId (Name_1 Name_2 ... = e_1 e_2 ...))
                                      node ...))
                       ((Name_3 = e_3) ...)
                       ((Name_4 = e_4) ...))
   
   (compute_gen_bblock cfg (bblockid ((nodeId (Name_2 ... = e_2 ...))
                                      node ...))
                       ((Name_3 = e_3) ...)
                       ((Name_4 = e_4) ...
                        (Name_1 = e_1) ; this def is gen
                        ))]

  ; a little bit of dynamic semantics: var without rvalue is bounded to nil
  ; the new definition kills another def of the same var in the bblock
  [(compute_gen_bblock cfg (bblockid ((nodeId (Name_1 Name_2 ... = ))
                                      node ...))
                       ((Name_3 = e_1) ...)
                       ((Name_4 = e_2) ... (Name_1 = e_3) (Name_5 = e_4) ...))
   
   (compute_gen_bblock cfg (bblockid ((nodeId (Name_2 ... = ))
                                      node ...))
                       ((Name_3 = e_1) ...
                        (Name_1 = e_3) ; this def is killed
                        )
                       ((Name_4 = e_2) ...
                        (Name_5 = e_4) ...
                        (Name_1 = nil) ; this def is gen
                        ))]

  ; new definition, doesn't kill another def from the bblock
  [(compute_gen_bblock cfg (bblockid ((nodeId (Name_1 Name_2 ... = ))
                                      node ...))
                       ((Name_3 = e_1) ...)
                       ((Name_4 = e_2) ...))
   
   (compute_gen_bblock cfg (bblockid ((nodeId (Name_2 ... = ))
                                      node ...))
                       ((Name_3 = e_1) ...)
                       ((Name_4 = e_2) ...
                        (Name_1 = nil) ; this def is gen
                        ))]


  ; node is not a var def, or it is an ill-formed var def
  [(compute_gen_bblock cfg (bblockid (any node ...))
                       ((Name_2 = e_2) ...)
                       ((Name_3 = e_3) ...))
   
   (compute_gen_bblock cfg (bblockid (node ...))
                       ((Name_2 = e_2) ...)
                       ((Name_3 = e_3) ...))]
  )


;                                                                                   
;                                                                                   
;                                                                                   
;                                       ;                       ;               ;;; 
;                                       ;                       ;              ;    
;                                       ;                       ;              ;    
;     ; ;;;    ;;;      ;;;;     ;;;    ; ;;;;              ;;; ;    ;;;     ;;;;;; 
;     ;;   ;  ;   ;    ;    ;   ;   ;   ;;   ;;            ;   ;;   ;   ;      ;    
;     ;      ;     ;        ;  ;        ;     ;           ;     ;  ;     ;     ;    
;     ;      ;     ;   ;;;;;;  ;        ;     ;           ;     ;  ;     ;     ;    
;     ;      ;;;;;;;  ;;    ;  ;        ;     ;           ;     ;  ;;;;;;;     ;    
;     ;      ;        ;     ;  ;        ;     ;           ;     ;  ;           ;    
;     ;       ;    ;  ;    ;;   ;   ;   ;     ;            ;   ;;   ;    ;     ;    
;     ;        ;;;;    ;;;; ;    ;;;    ;     ;             ;;; ;    ;;;;      ;    
;                                                                                   
;                                                                                   
;                                                                                   
;
; returns the cfgKG for a given term
(define-metafunction lang-reach-defs
  reach_defs : aterm -> cfgKG
  [(reach_defs aterm)
   (reach_defs_aux cfgKG (init_cfgKG cfgKG ()))

   (where cfg (build_cfg aterm))
   (where cfgKG (compute_KG cfg))])

(provide reach_defs)

; applies the main loop while there are observable changes between the in-out
; sets
(define-metafunction lang-reach-defs
  reach_defs_aux : cfgKG cfgKG -> cfgKG
  
  [(reach_defs_aux cfgKG_1 cfgKG_2)
   (reach_defs_aux cfgKG_1 cfgKG_3)
   
   (where cfgKG_3 (main_loop cfgKG_1 cfgKG_1 cfgKG_2))

   (side-condition (not (equal? (term cfgKG_3)
                                (term cfgKG_2))))]

  ; {no further changes on cfgKG_2}
  [(reach_defs_aux cfgKG_1 cfgKG_2)
   cfgKG_2]
  )

; receives a cfgKG and returns a copy, but without its gen-kill sets 
(define-metafunction lang-reach-defs
  init_cfgKG : cfgKG cfgKG -> cfgKG

  [(init_cfgKG () cfgKG)
   cfgKG
   ]

  [(init_cfgKG (((bblockid_1 (node)
                             ((Name_1 = e_1) ...)
                             ((Name_2 = e_2) ...))
                 (bblockid_2 ...))
                (bblockKG_1 (bblockid_3 ...)) ...)

               ((bblockKG_2 (bblockid_4 ...)) ...))

   (init_cfgKG ((bblockKG_1 (bblockid_3 ...)) ...)

               ((bblockKG_2 (bblockid_4 ...)) ...
                ((bblockid_1 (node)
                             ()
                             ())
                 (bblockid_2 ...))))
   ]
  )

; repeats the main loop of reach defs from
(define-metafunction lang-reach-defs
  main_loop : cfgKG cfgKG cfgKG -> cfgKG

  [(main_loop _ () cfgKG)
   cfgKG]

  [(main_loop cfgKG
              ; 
              (((bblockid_1 (node)
                            ((Name_1 = e_1) ...)
                            ((Name_2 = e_2) ...)
                            )
                (bblockid_2 ...))
               (bblockKG_1 (bblockid_3 ...)) ...)

              ; actual reach defs
              ((bblockKG_2 (bblockid_4 ...)) ...
               ((bblockid_1 (node)
                            ((Name_3 = e_3) ...)
                            ((Name_4 = e_4) ...)
                            )
                (bblockid_2 ...))
               (bblockKG_3 (bblockid_5 ...)) ...)
              )

   (main_loop cfgKG
              ; 
              ((bblockKG_1 (bblockid_3 ...)) ...)

              ; actual reach defs
              ((bblockKG_2 (bblockid_4 ...)) ...
               ((bblockid_1 (node)
                            ((Name_5 = e_5) ...)
                            ((Name_6 = e_6) ...)
                            )
                (bblockid_2 ...))
               (bblockKG_3 (bblockid_5 ...)) ...)
              )

   (where (((Name_5 = e_5) ...) ((Name_6 = e_6) ...))
          (update_in_out_bblock cfgKG
                                ((bblockKG_2 (bblockid_4 ...)) ...
                                 ((bblockid_1 (node)
                                              ((Name_3 = e_3) ...)
                                              ((Name_4 = e_4) ...)
                                              )
                                  (bblockid_2 ...))
                                 (bblockKG_3 (bblockid_5 ...)) ...)
                                bblockid_1))
   ])

(define-metafunction lang-reach-defs
  update_in_out_bblock : cfgKG cfgKG bblockid ->
  (((Name = e) ...) ((Name = e) ...))

  [(update_in_out_bblock (_ ...
                          ((bblockid_1 ((nodeId aterm))
                                       ((Name_1 = e_1) ...) ;kill
                                       ((Name_2 = e_2) ...) ;gen
                                       )
                           (bblockid_2 ...))
                          _ ...)

                         ((bblockKG_1 (bblockid_3 ...)) ...
                          ((bblockid_1 ((nodeId aterm))
                                       ((Name_3 = e_3) ...) ;IN
                                       ((Name_4 = e_4) ...) ;OUT
                                       )
                           (bblockid_2 ...))
                          (bblockKG_2 (bblockid_4 ...)) ...)

                         bblockid_1)

   (((Name_5 = e_5) ...) ((Name_6 = e_6) ...))

   (where ((Name_5 = e_5) ...) (update_in ((bblockKG_1 (bblockid_3 ...)) ...
                                           ((bblockid_1 ((nodeId aterm))
                                                        ((Name_3 = e_3) ...)
                                                        ((Name_4 = e_4) ...))
                                            (bblockid_2 ...))
                                           (bblockKG_2 (bblockid_4 ...)) ...)
                                          bblockid_1
                                          nodeId))

   ; compute out, taking into account last update to in
   (where ((Name_6 = e_6) ...) ,(remove-duplicates
                                 (append (term ((Name_2 = e_2) ...)) ; genB
                                         (remove* ; killB
                                          (term ((Name_1 = e_1) ...))
                                          ; IN B
                                          (term ((Name_5 = e_5) ...)) 
                                          ))))
   ]
  )

(define-metafunction lang-reach-defs
  update_in : cfgKG bblockid C -> ((Name = e) ...)

  [(update_in () _ _)
   ()]

  ; predecessor: position into the same scope of local var def
  [(update_in
    (((_
       (((in-hole C_1 (local (Name : t) ... = e ... in
                        ; C_2 does not contain local var defs
                        (side-condition
                         C_2
                         (not (redex-match?
                               lang-reach-defs
                               (in-hole C_6 (local (Name_2 : t_2) ... =
                                              e_2 ... in C_5 end))
                               (term C_2))))
                        end)) _))
                   _
                   ((Name_1 = e_1) ...))
                (bblockid_1 ... bblockid_2 bblockid_3 ...))
               (bblockKG (bblockid_4 ...)) ...)
              bblockid_2
              (in-hole C_1 (local (Name : t) ... = e ... in
                             ; C_3 does not contain local var defs
                             (side-condition
                              C_3
                              (not (redex-match?
                                    lang-reach-defs
                                    (in-hole C_6 (local (Name_2 : t_2) ... =
                                              e_2 ... in C_5 end))
                                    (term C_3))))
                             end)))

   ((Name_1 = e_1) ... (Name_2 = e_2) ...)

   
   (where ((Name_2 = e_2) ...)
          (update_in ((bblockKG (bblockid_4 ...)) ...)
                     bblockid_2
                     (in-hole C_1 (local (Name : t) ... = e ... in C_3 end))))]

  ; predecessor: position outside of scope of some local var defs
  [(update_in (((_
                 (((in-hole C_1 (local (Name : t) ... = e ... in
                                  ; C_2 does not contain local var defs
                                  (side-condition
                                   C_2
                                   (not
                                    (redex-match?
                                     lang-reach-defs
                                     (in-hole C_6 (local (Name_2 : t_2) ... =
                                                    e_2 ... in C_5 end))
                                     (term C_2))))
                                  end)) _))
                 _
                 ((Name_1 = e_1) ...))
                (bblockid_1 ... bblockid_2 bblockid_3 ...))
               (bblockKG (bblockid_4 ...)) ...)
              bblockid_2
              C_3)
   
   ((Name_3 = e_3) ... (Name_2 = e_2) ...)

   ; Context C_3 does not refer to the body of the local var def
   (side-condition
    (not (redex-match? lang-reach-defs
                       (in-hole C_1 (local (Name : t) ... = e ... in
                                      C_4 end))
                       (term C_3))))
   
   (where ((Name_2 = e_2) ...)
          (update_in ((bblockKG (bblockid_4 ...)) ...)
                     bblockid_2 C_3))

   ; remove definitions of variables that fall out of scope
   (where ((Name_3 = e_3) ...) (remove_out_scope ((Name : t) ...)
                                                 ((Name_1 = e_1) ...)))]
  
  ; predecessor 
  [(update_in (((_ _ _ ((Name_1 = e_1) ...))
                (bblockid_1 ... bblockid_2 bblockid_3 ...))
               (bblockKG (bblockid_4 ...)) ...)
              bblockid_2 C)

   ((Name_1 = e_1) ... (Name_2 = e_2) ...)

   (where ((Name_2 = e_2) ...)
          (update_in ((bblockKG (bblockid_4 ...)) ...)
                     bblockid_2 C))]

  ; not a predecessor
  [(update_in ((bblockKG_1 (bblockid_1 ...))
               (bblockKG_2 (bblockid_2 ...)) ...)
              bblockid_3 C)

   (update_in ((bblockKG_2 (bblockid_2 ...)) ...)
              bblockid_3 C)]
  )

; given a list of var defs that are outside of scope, remove their corresponding
; definitions, provided they are not bound to ctes and reachable from other
; variables
(define-metafunction lang-reach-defs
  remove_out_scope : ((Name : t) ...) ((Name_1 = e_1) ...) -> ((Name_2 = e_2)
                                                               ...)

  [(remove_out_scope () ((Name = e) ...))
   ((Name = e) ...)]

  [(remove_out_scope ((Name : t) ...) ())
   ()]

  ; Name_1 is bound to a cte, and appears as an rvalue
  [(remove_out_scope ((Name_1 : ctet) (Name_2 : t) ...) ((Name_3 = e_1) ...))
   ((Name_7 = e_4) ...)

   (where ((Name_4 = e_2) ... (Name_5 = (in-hole C Name_1)) (Name_6 = e_3) ...)
          ((Name_3 = e_1) ...))

   ; do not remove assignment of Name_1 nor (Name_5 = (in-hole C Name_1))
   (where ((Name_7 = e_4) ...) (remove_out_scope ((Name_2 : t) ...)
                                                 ((Name_3 = e_1) ...)))]

  ; {Name_1 is bound to a cte, but doesn't appear as an rvalue OR
  ; Name_1 is not bound to a cte}
  [(remove_out_scope ((Name_1 : t_1) (Name_2 : t_2) ...)
                     ((Name_3 = e_1) ... (Name_1 = e_2) (Name_4 = e_3) ...))
   ; remove one assignment, repeat remove_out_scope
   (remove_out_scope ((Name_1 : t_1) (Name_2 : t_2) ...)
                     ((Name_3 = e_1) ...
                      (Name_4 = e_3) ...))]

  ; No assignment of Name_1
  [(remove_out_scope ((Name_1 : t_1) (Name_2 : t_2) ...)
                     ((Name_3 = e) ... ))
   (remove_out_scope ((Name_2 : t_2) ...)
                     ((Name_3 = e) ...))]
  )
  
  

;                                                                                                              
;                                                                                                              
;                                                                                                              
;                                       ;                 ;           ;    ;;;;         ;                      
;                                       ;                 ;           ;       ;         ;       ;              
;                                       ;                 ;                   ;                 ;              
;     ; ;;;    ;;;      ;;;;     ;;;    ; ;;;;     ;;;;   ; ;;;     ;;;       ;       ;;;     ;;;;;;   ;     ; 
;     ;;   ;  ;   ;    ;    ;   ;   ;   ;;   ;;   ;    ;  ;;   ;      ;       ;         ;       ;       ;   ;  
;     ;      ;     ;        ;  ;        ;     ;        ;  ;     ;     ;       ;         ;       ;       ;   ;  
;     ;      ;     ;   ;;;;;;  ;        ;     ;   ;;;;;;  ;     ;     ;       ;         ;       ;       ;   ;  
;     ;      ;;;;;;;  ;;    ;  ;        ;     ;  ;;    ;  ;     ;     ;       ;         ;       ;        ; ;   
;     ;      ;        ;     ;  ;        ;     ;  ;     ;  ;     ;     ;       ;         ;       ;        ; ;   
;     ;       ;    ;  ;    ;;   ;   ;   ;     ;  ;    ;;  ;;   ;      ;       ;         ;       ;         ;;   
;     ;        ;;;;    ;;;; ;    ;;;    ;     ;   ;;;; ;  ; ;;;    ;;;;;;;     ;;;   ;;;;;;;     ;;;      ;    
;                                                                                                         ;    
;                                                                                                         ;    
;                                                                                                       ;;     
;                                                                                                              
(define-metafunction lang-reach-defs
  get_reach_defs : cfgKG nodeId -> ((Name = e) ...)

  [(get_reach_defs  ((bblockKG_1 (bblockid_1 ...)) ...
                     ((bblockid_2 (node_1 ... (nodeId aterm) node_2 ...)
                                  ((Name = e) ...) ; in defs
                                  _ ; out defs
                                  ) (bblockid_3 ...))
                     (bblockKG_2 (bblockid_4 ...)) ...)
                    nodeId)
   ((Name = e) ...)
   ])

(provide get_reach_defs)

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
(define-metafunction lang-reach-defs

  [(build-cfgKG-graph cfgKG)
   ,(reduction-relation
     lang-reach-defs                                   
     #:domain (bblockid (node ...) ((Name = e) ...) ((Name = e) ...))
   
     [--> (bblockid_2 (node_1 ...) _ _)
          (bblockid_4 (node_2 ...) ((Name_1 = e_1) ...) ((Name_2 = e_2) ...))
         
          (where
           ((bblockKG_1 (bblockid_1 ...)) ...
            ((bblockid_2 (node_1 ...) _ _) (bblockid_3 ... bblockid_4
                                                       bblockid_5 ...))
            (bblockKG_2 (bblockid_6 ...)) ...)
           ,(term cfgKG))
         
          (where
           (_ ...
            ((bblockid_4 (node_2 ...) ((Name_1 = e_1) ...) ((Name_2 = e_2) ...)
                         ) _)
            _ ...)
          
           ,(term cfgKG)
           )
          ])])

(provide build-cfgKG-graph)