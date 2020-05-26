#lang racket

(require redex
         "./grammar.rkt"
         "./desugar/parser.rkt"
         "./typing_lang_theory.rkt"
         "./type_inference.rkt"
         "./data_flow_analysis.rkt"
         "./reaching_defs.rkt")


;                                  
;                                  
;                                  
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

; extension to type_e, to manage generation of contexts for fun call params. 
(define-judgment-form
  lang-reach-defs
  #:mode (type_fun_call_param I I I I I I O O)
  #:contract (type_fun_call_param Γ Π cfgKG C C any Γ any)

  [(type_e Γ_1 Π cfgKG (in-hole C_1 (prefixexp (e_2 ... hole)))
           e_3 Γ_2 t_1)
   --------------------------------------------------------------------
   (type_fun_call_param Γ_1 Π cfgKG C_1 (prefixexp (e_2 ... hole)) e_3 Γ_2
                        (t_1))]

  [(type_e Γ_1 Π cfgKG (in-hole C_1 (prefixexp (e_2 ... hole e_3 e_4 ...)))
           e_5 Γ_2 t_1)

   (type_fun_call_param Γ_2 Π cfgKG C_1 (prefixexp (e_2 ... e_5 hole e_4 ...))
                        e_3 Γ_3 (t_2 ...))
   --------------------------------------------------------------------
   (type_fun_call_param Γ_1 Π cfgKG C_1 (prefixexp (e_2 ... hole e_3 e_4 ...))
                        e_5 Γ_3 (t_1 t_2 ...))]
  
  )

(define-judgment-form
  lang-reach-defs
  #:mode (type_method_call_param I I I I I I O O)
  #:contract (type_method_call_param Γ Π cfgKG C C any Γ any)

  [(type_e Γ_1 Π cfgKG (in-hole C_1 (prefixexp : Name (e_2 ... hole)))
           e_3 Γ_2 t_1)
   --------------------------------------------------------------------
   (type_method_call_param Γ_1 Π cfgKG C_1 (prefixexp : Name (e_2 ... hole))
                           e_3 Γ_2 (t_1))]

  [(type_e Γ_1 Π cfgKG (in-hole C_1
                                (prefixexp : Name (e_2 ... hole e_3 e_4 ...)))
           e_5 Γ_2 t_1)

   (type_method_call_param Γ_2 Π cfgKG C_1
                           (prefixexp : Name (e_2 ... e_5 hole e_4 ...))
                           e_3 Γ_3 (t_2 ...))
   --------------------------------------------------------------------
   (type_method_call_param Γ_1 Π cfgKG C_1
                           (prefixexp : Name (e_2 ... hole e_3 e_4 ...))
                           e_5 Γ_3 (t_1 t_2 ...))]
  
  )
; extension to type_e, to manage generation of contexts for table fields. 
(define-judgment-form
  lang-reach-defs
  #:mode (type_table_fields I I I I I I O O)
  #:contract (type_table_fields Γ Π cfgKG C C any Γ any)

  [(type_e Γ_1 Π cfgKG (in-hole C_1
                                (\{ (\[ e_3 \] = e_4) ...
                                    (\[ hole \] = e_2)
                                    (\[ e_5 \] = e_6)
                                    (\[ e_7 \] = e_8) ... \})
                                ) e_1 Γ_2
                                  t_1)

   (type_e Γ_2 Π cfgKG (in-hole C_1
                                (\{ (\[ e_3 \] = e_4) ...
                                    (\[ e_1 \] = hole)
                                    (\[ e_5 \] = e_6)
                                    (\[ e_7 \] = e_8) ... \})) e_2 Γ_3
                                                               t_2)

   ; there are fields left
   
   (type_table_fields Γ_3 Π cfgKG C_1
                      (\{ (\[ e_3 \] = e_4) ... (\[ e_1 \] = e_2)
                          hole
                          (\[ e_7 \] = e_8) ... \})
                      (\[ e_5 \] = e_6) Γ_4
                      ((\[ t_3 \] : t_4) ...))
   --------------------------------------------------------------------
   (type_table_fields Γ_1 Π cfgKG C_1
                      (\{ (\[ e_3 \] = e_4) ... hole (\[ e_5 \] = e_6)
                          (\[ e_7 \] = e_8) ... \})
                      (\[ e_1 \] = e_2) Γ_3
                      ((\[ t_1 \] : t_2) (\[ t_3 \] : t_4) ... ))]

  ; no fields left
  [(type_e Γ_1 Π cfgKG (in-hole C_1
                                (\{ (\[ e_3 \] = e_4) ...
                                    (\[ hole \] = e_2) \})) e_1 Γ_2
                                                            t_1)

   (type_e Γ_2 Π cfgKG (in-hole C_1
                                (\{ (\[ e_3 \] = e_4) ...
                                    (\[ e_1 \] = hole) \})) e_2 Γ_3
                                                            t_2)
   --------------------------------------------------------------------
   (type_table_fields Γ_1 Π cfgKG C_1 (\{ (\[ e_3 \] = e_4) ... hole \})
                      (\[ e_1 \] = e_2) Γ_3
                      ((\[ t_1 \] : t_2)))]
  )

(define-judgment-form
  lang-reach-defs
  #:mode (type_e I I I I I O O)
  ; TODO: define a typing rel for table fields, change any for t
  #:contract (type_e Γ Π cfgKG C any Γ any)
  
  [-----------------------------
   (type_e Γ Π cfgKG C nil Γ (nil : nil))]
  
  [-----------------------------
   (type_e Γ Π cfgKG C Boolean Γ (Boolean : bool))]

  [--------------------------
   (type_e Γ Π cfgKG C Number Γ (Number : num))]
  
  [-------------------------------
   (type_e Γ Π cfgKG C String Γ (String : str))]

  [-----------------------------------
   (type_e Γ Π cfgKG C Name Γ (index Γ Name))]

  ; TODO: I'm assuming that the variables in the environment of the closure
  ; have the same type
  [--------------------------------------------------------
   (type_e Γ_1 Π cfgKG C (t_1 function Name_1 () s end) Γ_1
           (($tup) -> t_1))]
  
  [--------------------------------------------------------
   (type_e Γ_1 Π cfgKG C (t_1 function Name_1
                              ((Name_2 : t_2) (Name_3 : t_3) ...)
                              s end) Γ_1
                                     (($tup t_2 t_3 ...) -> t_1))]

  [; TODO: better way to assert that we are not calling setmetatable?
   (side-condition ,(not (equal? (term ($ENV |[| "setmetatable" |]|))
                                 (term prefixexp))))
   ; TODO: recursive function?
   (type_e Γ_1 Π cfgKG C prefixexp Γ_2 (($tup) -> t_2))
   -------------------------------------------------
   (type_e Γ_1 Π cfgKG C (prefixexp ()) Γ_2 t_2)]

  [; TODO: better way to assert that we are not calling setmetatable?
   (side-condition ,(not (equal? (term ($ENV |[| "setmetatable" |]|))
                                 (term prefixexp))))
   ; TODO: recursive function?
   (type_e Γ_1 Π cfgKG C prefixexp Γ_2 (($tup t_1 ...) -> t_2))
   (type_fun_call_param Γ_2 Π cfgKG C (prefixexp (hole e_2 ...)) e_1 Γ_3
                        (t_3 ...))

   ; actual parameters are a subtype of formal parameters
   (side-condition (subtyping_rel ($tup t_3 ...) ($tup t_1 ...)))
   -----------------------------------------------------------------------
   (type_e Γ_1 Π cfgKG C (prefixexp (e_1 e_2 ...)) Γ_3 t_2)]

  ; Extract info about the weakness of a table assumptions:
  ; - first argument, Name, is bound to a table
  ; - a call to setmetatable with an expression that can be typed as a table
  ; - strings as key and value of the __mode field
  ; - _ENV is populated with library services, bound to the expected
  ; identifiers
  [(type_e Γ_1 Π cfgKG (in-hole C (($ENV |[| "setmetatable" |]|) (hole e)))
           Name Γ_2 ((\{ (\[ t_1 \] : t_2) ... \}) _))
   (type_e Γ_2 Π cfgKG (in-hole C (($ENV |[| "setmetatable" |]|) (Name hole)))
           e Γ_3 ((\{ _ ... (\[ ("__mode" : str) \] : (String : str)) _ ...
                      \}) _))
   (side-condition ,(string-contains? (term String) "k"))
   (where Γ_4 (set Γ_3 Name ((\{ (\[ t_1 \] : t_2) ... \}) wk))) 
   ---------------------------------------------------------------------------
   (type_e Γ_1 Π cfgKG C
           (($ENV |[| "setmetatable" |]|) (Name e))
           Γ_4
           ((\{ (\[ t_1 \] : t_2) ... \}) wk))]

  [(type_e Γ_1 Π cfgKG (in-hole C (($ENV |[| "setmetatable" |]|) (hole e)))
           Name Γ_2 ((\{ (\[ t_1 \] : t_2) ... \}) _))
   (type_e Γ_2 Π cfgKG (in-hole C (($ENV |[| "setmetatable" |]|) (Name hole)))
           e Γ_3 ((\{ _ ... (\[ ("__mode" : str) \] : (String : str)) _ ...
                      \}) _))
   (side-condition ,(string-contains? (term String) "v"))
   (where Γ_4 (set Γ_3 Name ((\{ (\[ t_1 \] : t_2) ... \}) wv))) 
   ---------------------------------------------------------------------------
   (type_e Γ_1 Π cfgKG C
           (($ENV |[| "setmetatable" |]|) (Name e))
           Γ_4
           ((\{ (\[ t_1 \] : t_2) ... \}) wv))]

  [(type_e Γ_1 Π cfgKG (in-hole C (($ENV |[| "setmetatable" |]|) (hole e)))
           Name Γ_2 ((\{ (\[ t_1 \] : t_2) ... \}) weakness))
   (type_e Γ_2 Π cfgKG (in-hole C (($ENV |[| "setmetatable" |]|) (Name hole)))
           e Γ_3 ((\{ _ ... (\[ ("__mode" : str) \] : (String : str)) _ ...
                      \}) _))
   (side-condition ,(string-contains? (term String) "k"))
   (side-condition ,(string-contains? (term String) "v"))
   (where Γ_4 (set Γ_3 Name ((\{ (\[ t_1 \] : t_2) ... \}) wkv))) 
   ---------------------------------------------------------------------------
   (type_e Γ_1 Π cfgKG C
           (($ENV |[| "setmetatable" |]|) (Name e))
           Γ_4
           ((\{ (\[ t_1 \] : t_2) ... \}) wkv))]

  ; meta-table with not weakness information
  [(type_e Γ_1 Π cfgKG (in-hole C (($ENV |[| "setmetatable" |]|) (hole e)))
           Name Γ_2 ((\{ (\[ t_1 \] : t_2) ... \}) weakness_1))
   (type_e Γ_2 Π cfgKG (in-hole C (($ENV |[| "setmetatable" |]|) (Name hole)))
           e Γ_3 ((\{ (\[ t_3 \] : t_4) ... \}) weakness_2))
   
   (side-condition
    ,(not (redex-match? lang-reach-defs
                        (side-condition
                         ((\{ _ ... (\[ ("__mode" : str) \] : (String : str))
                              _ ... \}) _)
                         (not (or (string-contains? (term String) "k")
                                  (string-contains? (term String) "v"))))
                        (term ((\{ (\[ t_3 \] : t_4) ... \})
                               weakness_2)))))
   
   ; dynamic semantics dictates that the table weakness changes to strong
   (where Γ_4 (set Γ_3 Name ((\{ (\[ t_1 \] : t_2) ... \}) strong))) 
   ---------------------------------------------------------------------------
   (type_e Γ_1 Π cfgKG C
           (($ENV |[| "setmetatable" |]|) (Name e))
           Γ_4
           ((\{ (\[ t_1 \] : t_2) ... \}) strong))]

  ; TODO: not checking for membership of Name to prefixexp
  [; TODO: better way to assert that we are not calling setmetatable?
   (side-condition ,(or (not (equal? (term $ENV)
                                     (term prefixexp)))
                        (not (equal? (term Name)
                                     (term setmetatable)))))
   ; TODO: recursive function?
   ; prefixexp is an object with the corresponding method
   (type_e Γ_1 Π cfgKG C prefixexp Γ_2 ((\{ any_1 ...
                                            (\[ t_1 \] : (($tup) -> t_2))
                                            any_2 ... \}) weakness))
   (side-condition (subtyping_rel t_1 str))
   -------------------------------------------------
   (type_e Γ_1 Π cfgKG C (prefixexp : Name ()) Γ_2 t_2)]

  [; TODO: better way to assert that we are not calling setmetatable?
   (side-condition ,(not (equal? (term ($ENV |[| "setmetatable" |]|))
                                 (term prefixexp))))
   ; TODO: recursive function?
   ; prefixexp is an object with the corresponding method
   (type_e Γ_1 Π cfgKG C prefixexp Γ_2
           ((\{ any_1 ...
                (\[ t_1 \] : (($tup t_2 ...) -> t_3))
                any_2 ... \}) weakness))
   
   (side-condition (subtyping_rel t_1 str))

   ; actual parameters are a subtype of formal parameters
   (type_method_call_param Γ_2 Π cfgKG C (prefixexp : Name (hole e_2 ...)) e_1
                           Γ_3 (t_4 ...))
   (side-condition (subtyping_rel ($tup t_4 ...) ($tup t_2 ...)))
   -----------------------------------------------------------------------
   (type_e Γ_1 Π cfgKG C (prefixexp : Name (e_1 e_2 ...)) Γ_3 t_3)]

  ; It doesn't matter if <<< is bound to weak values: into the closure, there
  ; will be a list of strong references bound to the weak values, mapped to
  ; <<< by the environment
  [---------------------------------
   (type_e Γ Π cfgKG C <<< Γ (index Π <<<))]

  ; TODO: would we need to give a typing rule for every service?
  ;  [(type_e Γ_1 Π e Γ_2 ncte) ...
  ;   -------------------------------------------------------
  ;   (type_e Γ_1 Π ($builtIn builtinserv (e ...)) Γ_1 num)]

  ; TODO: I'm not considering tuple types
  [(type_e Γ_1 Π cfgKG (in-hole C (\( hole \))) e Γ_2 t)
   ---------------------------------------
   (type_e Γ_1 Π cfgKG C (\( e \)) Γ_2 t)]

  ; TOOD: compute correct contexts for each field
  [(type_table_fields Γ_1 Π cfgKG C (\{ hole field_2 ... \}) field_1 Γ_2
                      (any ...))
   ----------------------------------------------------------------------
   (type_e Γ_1 Π cfgKG C (\{ field_1 field_2 ... \}) Γ_2
           ((\{ any ... \}) strong))]

  [-------------------------------------------------
   (type_e Γ Π cfgKG C (\{ \}) Γ ((\{ \}) strong))]

  ; TODO: coercion?
  [(type_e Γ_1 Π cfgKG (in-hole C (hole arithop e_2)) e_1 Γ_2 t_1)
   (side-condition (subtyping_rel t_1 num))
   (type_e Γ_2 Π cfgKG (in-hole C (e_1 arithop hole)) e_2 Γ_3 t_2)
   (side-condition (subtyping_rel t_2 num))
   ----------------------------------------------------------------
   (type_e Γ_1 Π cfgKG C (e_1 arithop e_2) Γ_3 num)]
  
  [(type_e Γ_1 Π cfgKG (in-hole C (hole .. e_2)) e_1 Γ_2 t_1)
   (type_e Γ_2 Π cfgKG (in-hole C (e_1 .. hole)) e_2 Γ_3 t_2)
   
   (side-condition ,(and (term (subtyping_rel t_1 str))
                         (term (subtyping_rel t_2 str))))

   ; supremum of both string types
   (where t_3 (supremum_type t_1 t_2))
   ----------------------------------------------------------------
   (type_e Γ_1 Π cfgKG C (e_1 .. e_2) Γ_3 t_3)]

  [(type_e Γ_1 Π cfgKG (in-hole C (hole relop e_2)) e_1 Γ_2 t_1)
   (type_e Γ_2 Π cfgKG (in-hole C (e_1 relop hole)) e_2 Γ_3 t_2)

   ; numeric or string subtype
   (side-condition ,(or (and (term (subtyping_rel t_1 num))
                             (term (subtyping_rel t_2 num)))
                        (and (term (subtyping_rel t_1 str))
                             (term (subtyping_rel t_2 str)))))
   ----------------------------------------------------------------
   (type_e Γ_1 Π cfgKG C (e_1 relop e_2) Γ_3 bool)]

  [(type_e Γ_1 Π cfgKG (in-hole C (hole and e_2)) e_1 Γ_2 t_1)
   (type_e Γ_2 Π cfgKG (in-hole C (e_1 and hole)) e_2 Γ_3 t_2)

   ; supremum of both types
   (where t_3 (supremum_type t_1 t_2))
   ----------------------------------------------------------------
   (type_e Γ_1 Π cfgKG C (e_1 and e_2) Γ_3 t_3)]

  [(type_e Γ_1 Π cfgKG (in-hole C (hole or e_2)) e_1 Γ_2 t_1)
   (type_e Γ_2 Π cfgKG (in-hole C (e_1 or hole)) e_2 Γ_3 t_2)

   ; supremum of both types
   (where t_3 (supremum_type t_1 t_2))
   ----------------------------------------------------------------
   (type_e Γ_1 Π cfgKG C (e_1 or e_2) Γ_3 t_3)]

  [(type_e Γ_1 Π cfgKG (in-hole C (hole or e_2)) e_1 Γ_2 t)
   (type_e Γ_2 Π cfgKG (in-hole C (e_1 or hole)) e_2 Γ_3 t)
   ----------------------------------------------------------------
   (type_e Γ_1 Π cfgKG C (e_1 == e_2) Γ_3 bool)]

  ; TODO: coercion?
  [(type_e Γ_1 Π cfgKG (in-hole C (- hole)) e Γ_2 t)
   (side-condition (subtyping_rel t num))
   --------------------------------------
   (type_e Γ_1 Π cfgKG C (- e) Γ_2 num)]

  [(type_e Γ_1 Π cfgKG (in-hole C (not hole)) e Γ_2 t)
   ----------------------------------------------------
   (type_e Γ_1 Π cfgKG C (not e) Γ_2 bool)]

  [(type_e Γ_1 Π cfgKG (in-hole C (\# hole)) e Γ_2 t)
   
   ; table type or string subtype
   (side-condition ,(or (is_tablet? (term t))
                        (term (subtyping_rel t str))))
   ----------------------------------------------------
   (type_e Γ_1 Π cfgKG C (\# e) Γ_2 num)] 
  
    
  ;                                                                  
  ;                                                                  
  ;                                                      ;;          
  ;                                                     ;            
  ;                     ;                               ;            
  ;                     ;                               ;            
  ;   ; ;;;    ;;;;   ;;;;;;           ;;;;     ;;;   ;;;;;    ;;;;  
  ;   ;;   ;  ;;  ;;    ;             ;    ;   ;   ;    ;     ;;  ;; 
  ;   ;    ;  ;    ;    ;             ;            ;    ;     ;    ; 
  ;   ;    ;  ;    ;    ;              ;;;;    ;;;;;    ;     ;;;;;; 
  ;   ;    ;  ;    ;    ;                  ;  ;    ;    ;     ;      
  ;   ;    ;  ;;  ;;    ;             ;    ;  ;   ;;    ;     ;;   ; 
  ;   ;    ;   ;;;;      ;;;           ;;;;    ;;; ;    ;      ;;;;  
  ;                                                                  
  ;                                                                  
  ;                                                                  
  ;        
  
  ;                                                                                          
  ;                                                                                          
  ;      ;;     ;             ;;;          ;            ;                  ;                 
  ;     ;                       ;          ;                               ;                 
  ;     ;                       ;          ;                               ;                 
  ;     ;                       ;          ;                               ;                 
  ;   ;;;;;   ;;;      ;;;;     ;      ;;;;;          ;;;     ; ;;;    ;;;;;   ;;;;   ;;  ;; 
  ;     ;       ;     ;;  ;;    ;     ;;  ;;            ;     ;;   ;  ;;  ;;  ;;  ;;   ;  ;  
  ;     ;       ;     ;    ;    ;     ;    ;            ;     ;    ;  ;    ;  ;    ;    ;;   
  ;     ;       ;     ;;;;;;    ;     ;    ;            ;     ;    ;  ;    ;  ;;;;;;    ;;   
  ;     ;       ;     ;         ;     ;    ;            ;     ;    ;  ;    ;  ;         ;;   
  ;     ;       ;     ;;   ;    ;     ;;  ;;            ;     ;    ;  ;;  ;;  ;;   ;   ;  ;  
  ;     ;     ;;;;;    ;;;;      ;;;   ;;;;;          ;;;;;   ;    ;   ;;;;;   ;;;;   ;;  ;; 
  ;                                                                                          
  ;                                                                                          
  ;                                                                                          
  ;                                                                                          

  
  ; assumption: we can type check only the case where the field is accessed
  ; through the same expression that appears into the table's keys, and for a
  ; table with keys with different types
  [(type_e Γ_1 Π cfgKG (in-hole C (hole \[ e_2 \]))
           e_1 Γ_2 ((\{ (\[ t_1 \] : t_2) ... \}) strong))
   
   (type_e Γ_2 Π cfgKG (in-hole C (e_1 \[ hole \]))
           e_2 Γ_3 t_3)

   ; access the field with key of type t_3
   (where (\{ _ ...
              (\[ t_3 \] : t_4)
              _ ... \}) (\{ (\[ t_1 \] : t_2) ... \}))
   ----------------------------------------------------------------
   (type_e Γ_1 Π cfgKG C (e_1 \[ e_2 \]) Γ_3 t_4)]

  ; assumption: tables indexed only through a variable bound to them
  [; it is a weak table
   (type_e Γ_1 Π cfgKG (in-hole C (hole \[ e_1 \]))
           Name_1 Γ_2 ((\{ (\[ t_1 \] : t_2) ... \}) wv))
   
   (type_e Γ_2 Π cfgKG (in-hole C (Name_1 \[ hole \]))
           e_1 Γ_3 t_3)

   ; the field contains a cte; we will need to check for reachability
   (where (_ ...
           (\[ t_3 \] : ctet)
           _ ...) ((\[ t_1 \] : t_2) ...))

   ; determine reach. of the value, since it is referred by a weak ref.
   ; get. reach defs.
   (where ((Name_2 = e_2) ...) (get_reach_defs cfgKG C))

   (where ioTree (build_tree ((Name_2 = e_2) ...)))

   (side-condition ,(if (term (reachCte_stat ((Name_1 \[ e_1 \]) ioTree Γ_3)))
                        #t
                        (begin
                          (println "Access to: ")
                          (println (term (Name_1 \[ e_1 \])))
                          ; TODO: too verbose, change for line number
                          ;(println "from: ")
                          ;(println (term C))
                          (println "may exhibit non-deterministic behavior")
                          #t)))
   ----------------------------------------------------------------
   (type_e Γ_1 Π cfgKG C (Name_1 \[ e_1 \]) Γ_3 ctet)]

  [(type_e Γ_1 Π cfgKG (in-hole C (hole \[ e_1 \]))
           Name_1 Γ_2 ((\{ (\[ t_1 \] : t_2) ... \}) wv))
   
   (type_e Γ_2 Π cfgKG (in-hole C (Name_1 \[ hole \]))
           e_1 Γ_3 t_3)

   ; the field does not contain a cte; no need to check for reachability
   (where (\{ _ ...
              (\[ t_3 \] : t_4)
              _ ... \}) (\{ (\[ t_1 \] : t_2) ... \}))

   (side-condition ,(not (redex-match? lang-reach-defs
                                       ctet
                                       (term t_4))))
   ----------------------------------------------------------------
   (type_e Γ_1 Π cfgKG C (Name_1 \[ e_1 \]) Γ_3 t_4)]
  
  ; ephemerons
  [; it is a weak table
   (type_e Γ_1 Π cfgKG (in-hole C (hole \[ e_1 \]))
           Name_1 Γ_2 ((\{ (\[ t_1 \] : t_2) ... \}) wk))
     
   (type_e Γ_2 Π cfgKG (in-hole C (Name_1 \[ hole \]))
           e_1 Γ_3 t_3)
  
   ; the key is not a cte
   (side-condition ,(not (redex-match? lang-reach-defs
                                       ctet
                                       (term t_3))))
   ; look for the type of the value
   (where (\{ _ ...
              (\[ t_3 \] : t_4)
              _ ... \}) (\{ (\[ t_1 \] : t_2) ... \}))
   ----------------------------------------------------------------
   (type_e Γ_1 Π cfgKG C (Name_1 \[ e_1 \]) Γ_3 t_4)]
  
  [(type_e Γ_1 Π cfgKG (in-hole C (hole \[ e_1 \]))
           Name_1 Γ_2 ((\{ (\[ t_1 \] : t_2) ... \}) wk))
           
   ; the key is a cte
   (type_e Γ_2 Π cfgKG (in-hole C (Name_1 \[ hole \]))
           e_1 Γ_3 ctet)
           
   ; the reachability of the value depends on the reachability of the key
   ; determine reach. of the key, since it is referred by a weak ref.
   ; get. reach defs.
   (where ((Name_2 = e_2) ...) (get_reach_defs cfgKG C))

   (where ioTree (build_tree ((Name_2 = e_2) ...)))

   (side-condition ,(if (term (reachCte_stat (e_1 ioTree Γ_3)))
                        #t
                        (begin
                          (println "Access to: ")
                          (println (term (Name_1 \[ e_1 \])))
                          ; TODO: too verbose, change for line number
                          ;(println "from: ")
                          ;(println (term C))
                          (println "may exhibit non-deterministic behavior")
                          )))
           
   ; extract the type the value
   (where (\{ _ ...
              (\[ ctet \] : t_3)
              _ ... \}) (\{ (\[ t_1 \] : t_2) ... \}))
   ----------------------------------------------------------------
   (type_e Γ_1 Π cfgKG C (Name_1 \[ e_1 \]) Γ_3 t_3)]
  
  ; key does not belong to table
  ; assumption: table does not have a meta-table who will take care of
  ; the indexing
  [(type_e Γ_1 Π cfgKG (in-hole C (hole \[ e_2 \]))
           e_1 Γ_2 ((\{ (\[ t_1 \] : t_2) ... \}) weakness))
  
   (type_e Γ_2 Π cfgKG (in-hole C (e_1 \[ hole \]))
           e_2 Γ_3 t_3)
  
     
   (side-condition
    ,(not (redex-match?
           lang-reach-defs
           (_ ...
            (\[ (side-condition t_4 (equal? (term t_4) (term t_3))) \] : t_5)
            _ ...)
           (term ((\[ t_1 \] : t_2) ...)))))
   ----------------------------------------------------------------
   (type_e Γ_1 Π cfgKG C (e_1 \[ e_2 \]) Γ_3 (nil : nil))]

  
  )


(provide type_e)


  
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
; extension to type_s, to manage generation of contexts for concat. stats. 
(define-judgment-form
  lang-reach-defs
  #:mode (type_conc_stats I I I I I I O)
  #:contract (type_conc_stats Γ Π cfgKG C C any Γ)

  [(type_s Γ_1 Π cfgKG (in-hole C_1 (s_2 ... hole s_3 s_4 ...)) s_1 Γ_2)
   
   (type_conc_stats Γ_2 Π cfgKG C_1 (s_2 ... s_1 hole s_4 ...) s_3 Γ_3)
   --------------------------------------------------------------------
   (type_conc_stats Γ_1 Π cfgKG C_1 (s_2 ... hole s_3 s_4 ...) s_1 Γ_3)]

  [(type_s Γ_1 Π cfgKG (in-hole C_1 (s_2 ... hole)) s_1 Γ_2)
   --------------------------------------------------------------------
   (type_conc_stats Γ_1 Π cfgKG C_1 (s_2 ... hole) s_1 Γ_2)]
  )

; type mult. assignment

; before typing assigment: type lvalues and rvalues
(define-judgment-form
  lang-reach-defs
  #:mode (type_lrvalues I I I I I I O O O)
  #:contract (type_lrvalues Γ Π cfgKG C C any Γ
                            (any ...) ; lvalues: Name ... (Name \[ t \]) ... 
                            (t ...) ; rvalues
                            )

  [(type_lrvalues Γ_1 Π cfgKG C
                  (var_1 ... Name hole var_3 ... = e ...)
                  var_2 Γ_2
                  (any ...)
                  (t_2 ...))
   --------------------------------------------------------------------
   (type_lrvalues Γ_1 Π cfgKG C (var_1 ... hole var_2 var_3 ... = e ...)
                  Name Γ_2
                  (Name any ...)
                  (t_2 ...))]

  [(type_e Γ_1 Π cfgKG
           (in-hole C
                    (var_1 ... (Name \[ hole \]) var_2  var_3 ... = e_1 ...))
           e_2 Γ_2 t_1)

   (type_lrvalues Γ_2 Π cfgKG C
                  (var_1 ... (Name \[ e_2 \]) hole var_3 ... = e_1 ...)
                  var_2 Γ_3
                  (any ...)
                  (t_2 ...))
   --------------------------------------------------------------------
   (type_lrvalues Γ_1 Π cfgKG C (var_1 ... hole var_2 var_3 ... = e_1 ...)
                  (Name \[ e_2 \]) Γ_2
                  ((Name \[ t_1 \]) any ...)
                  (t_2 ...))]
  
  [(type_lrvalues Γ_1 Π cfgKG C
                  (var_1 ... Name = hole e_2 ...)
                  e_1 Γ_2
                  (any ...)
                  (t ...))
   --------------------------------------------------------------------
   (type_lrvalues Γ_1 Π cfgKG C (var_1 ... hole = e_1 e_2 ...)
                  Name Γ_2
                  (Name any ...)
                  (t ...))]

  [(type_e Γ_1 Π cfgKG
           (in-hole C
                    (var_1 ... (Name \[ hole \]) = e_1 e_2 ...))
           e_3 Γ_2 t_1)

   (type_lrvalues Γ_2 Π cfgKG C
                  (var_1 ... (Name \[ e_3 \]) = hole e_2 ...)
                  e_1 Γ_3
                  (any ...)
                  (t_2 ...))
   --------------------------------------------------------------------
   (type_lrvalues Γ_1 Π cfgKG C (var_1 ... hole = e_1 e_2 ...)
                  (Name \[ e_3 \]) Γ_2
                  ((Name \[ t_1 \]) any ...)
                  (t_2 ...))]

  [(type_e Γ_1 Π cfgKG
           (in-hole C_1
                    (var ... = e_1 ... hole e_2 e_3 ...))
           e_4 Γ_2 t_1)

   (type_lrvalues Γ_2 Π cfgKG C_1
                  (var ... = e_1 ... e_4 hole e_3 ...)
                  e_2 Γ_3
                  (any ...)
                  (t_2 ...))
   --------------------------------------------------------------------
   (type_lrvalues Γ_1 Π cfgKG C_1 (var ... = e_1 ... hole e_2 e_3 ...)
                  e_4 Γ_3
                  (any ...)
                  (t_1 t_2 ...))]

  [(type_e Γ_1 Π cfgKG
           (in-hole C_1
                    (var ... = e_1 ... hole))
           e_2 Γ_2 t_1)
   --------------------------------------------------------------------
   (type_lrvalues Γ_1 Π cfgKG C_1 (var ... = e_1 ... hole)
                  e_2 Γ_2
                  ()
                  (t_1))]       
  )

; after typing lvalues and rvalues, performs type assignment
(define-judgment-form
  lang-reach-defs
  #:mode (type_assign I I I I I O)
  #:contract (type_assign Γ 
                          ; lvalues: Name ... (Name \[ t \]) ...
                          (any ... hole any ...) 
                          any
                          ; rvalues
                          (t ... hole ... t ...)
                          t
                          Γ)

  ; table field
  ; assign field
  [; var. bound to table
   (where ((\{ (\[ t_6 \] : t_7) ... \}) weakness)
          (index Γ_1 Name))

   ; index type
   (where (any_4 ... (\[ t_1 \] : t_8) any_5 ...) ((\[ t_6 \] : t_7) ...))

   ; type assigned is a subtype of the values' type
   (side-condition (subtyping_rel t_5 t_8))

   (type_assign Γ_1
                (any_1 ... (Name \[ t_1 \]) hole any_3 ...)
                any_2
                (t_2 ... t_5 hole t_4 ...)
                t_3
                Γ_2)
   --------------------------------------------------------------------
   (type_assign Γ_1
                (any_1 ... hole any_2 any_3 ...)
                (Name \[ t_1 \])
                (t_2 ... hole t_3 t_4 ...)
                t_5
                Γ_2)]

  [; var. bound to table
   (where ((\{ (\[ t_5 \] : t_6) ... \}) weakness)
          (index Γ Name))

   ; index type
   (where (any_1 ... (\[ t_1 \] : t_7) any_2 ...) ((\[ t_5 \] : t_6) ...))

   ; type assigned is a subtype of the values' type
   (side-condition (subtyping_rel t_4 t_7))
   --------------------------------------------------------------------
   (type_assign Γ
                (any_1 ... hole)
                (Name \[ t_1 \])
                (t_2 ... hole t_3 ...)
                t_4
                Γ)]

  ; new field
  [; var. bound to table
   (where ((\{ (\[ t_6 \] : t_7) ... \}) weakness)
          (index Γ_1 Name))

   ;new field
   (side-condition
    ,(not (redex-match? lang-reach-defs
                        (any_1 ... (\[ (side-condition t_8
                                                       (equal? (term t_8)
                                                               (term t_1)))
                                       \] : t_9) any_2 ...)
                        (term ((\[ t_6 \] : t_7) ...)))))

   ; update environment
   (where Γ_2 (set Γ_1 Name ((\{ (\[ t_1 \] : t_5) (\[ t_6 \] : t_7) ... \})
                             weakness)))

   (type_assign Γ_2
                (any_1 ... (Name \[ t_1 \]) hole any_3 ...)
                any_2
                (t_2 ... t_5 hole t_4 ...)
                t_3
                Γ_3)
   --------------------------------------------------------------------
   (type_assign Γ_1
                (any_1 ... hole any_2 any_3 ...)
                (Name \[ t_1 \])
                (t_2 ... hole t_3 t_4 ...)
                t_5
                Γ_3)]

  [; var. bound to table
   (where ((\{ (\[ t_6 \] : t_7) ... \}) weakness)
          (index Γ_1 Name))

   ;new field
   (side-condition
    ,(not (redex-match? lang-reach-defs
                        (any_1 ... (\[ (side-condition t_8
                                                       (equal? (term t_8)
                                                               (term t_1)))
                                       \] : t_9) any_2 ...)
                        (term ((\[ t_6 \] : t_7) ...)))))

   ; update environment
   (where Γ_2 (set Γ_1 Name ((\{ (\[ t_1 \] : t_5) (\[ t_6 \] : t_7) ... \})
                             weakness)))
   --------------------------------------------------------------------
   (type_assign Γ_1
                (any_1 ... hole)
                (Name \[ t_1 \])
                (t_2 ... hole t_3 ...)
                t_5
                Γ_2)]
  
  ; local var
  [(where t_5 (index Γ_1 Name))

   ; type assigned is a subtype 
   (side-condition (subtyping_rel t_4 t_5))

   (type_assign Γ_1
                (any_1 ... Name hole any_3 ...)
                any_2
                (t_1 ... t_4 hole t_3 ...)
                t_2
                Γ_2)
   --------------------------------------------------------------------
   (type_assign Γ_1
                (any_1 ... hole any_2 any_3 ...)
                Name
                (t_1 ... hole t_2 t_3 ...)
                t_4
                Γ_2)]

  [(where t_4 (index Γ Name))

   ; type assigned is a subtype 
   (side-condition (subtyping_rel t_3 t_4))
   --------------------------------------------------------------------
   (type_assign Γ
                (any_1 ... hole)
                Name
                (t_1 ... hole t_2 ...)
                t_3
                Γ)]

  ; no rvalues left: variables are assigned nil values; no need to check
  ; table field
  ; field assign
  [; var. bound to table
   (where ((\{ (\[ t_4 \] : t_5) ... \}) weakness)
          (index Γ Name))

   ; index type
   (where (any_4 ... (\[ t_1 \] : t_6) any_5 ...) ((\[ t_4 \] : t_5) ...))

   ; type assigned is a subtype of the values' type
   (side-condition (subtyping_rel t_3 t_6))
   --------------------------------------------------------------------
   (type_assign Γ
                (any_1 ... hole any_2 any_3 ...)
                (Name \[ t_1 \])
                (t_2 ... hole)
                t_3
                Γ)]

  ; new field
  [; var. bound to table
   (where ((\{ (\[ t_4 \] : t_5) ... \}) weakness)
          (index Γ_1 Name))

   ;new field
   (side-condition
    ,(not (redex-match? lang-reach-defs
                        (any_1 ... (\[ (side-condition t_6
                                                       (equal? (term t_6)
                                                               (term t_1)))
                                       \] : t_7) any_2 ...)
                        (term ((\[ t_4 \] : t_5) ...)))))

   ; update environment
   (where Γ_2 (set Γ_1 Name ((\{ (\[ t_1 \] : t_3) (\[ t_4 \] : t_5) ... \})
                             weakness)))
   --------------------------------------------------------------------
   (type_assign Γ_1
                (any_1 ... hole any_2 any_3 ...)
                (Name \[ t_1 \])
                (t_2 ... hole)
                t_3
                Γ_2)]

  ; loc. var
  [(where t_3 (index Γ Name))

   ; type assigned is a subtype 
   (side-condition (subtyping_rel t_2 t_3))
   --------------------------------------------------------------------
   (type_assign Γ
                (any_1 ... hole any_2 any_3 ...)
                Name
                (t_1 ... hole)
                t_2
                Γ)]
  )

; extension to type_s, to manage generation of contexts for return stat. 
(define-judgment-form
  lang-reach-defs
  #:mode (type_return I I I I I I O)
  #:contract (type_return Γ Π cfgKG C C any Γ)

  [(type_e Γ_1 Π cfgKG (in-hole C_1 (return e_2 ... hole e_3 e_4 ...)) e_1 Γ_2
           _)
   
   (type_return Γ_2 Π cfgKG C_1 (return e_2 ... e_1 hole e_4 ...) e_3 Γ_3)
   --------------------------------------------------------------------
   (type_return Γ_1 Π cfgKG C_1 (return e_2 ... hole e_3 e_4 ...) e_1 Γ_3)]

  [(type_e Γ_1 Π cfgKG (in-hole C_1 (return e_2 ... hole)) e_1 Γ_2 _)
   --------------------------------------------------------------------
   (type_return Γ_1 Π cfgKG C_1 (return e_2 ... hole) e_1 Γ_2)]
  )

; typecheck rvalues in local var. def.
(define-judgment-form
  lang-reach-defs
  #:mode (type_loc_rval I I I I I I O O)
  #:contract (type_loc_rval Γ Π cfgKG C C any Γ (t ...))

  [(type_e Γ_1 Π cfgKG
           (in-hole C
                    (local (Name : t) ... = e_1 ... hole e_2 e_3 ... in
                      s end))
           e_4 Γ_2 t_1)

   (type_loc_rval Γ_2 Π cfgKG C
                  (local (Name : t) ... = e_1 ... e_4 hole e_3 ... in
                    s end)
                  e_2 Γ_3 (t_2 ...))
   --------------------------------------------------------------------
   (type_loc_rval Γ_1 Π cfgKG C
                  (local (Name : t) ... = e_1 ... hole e_2 e_3 ... in
                    s end)
                  e_4 Γ_3 (t_1 t_2 ...))]

  [(type_e Γ_1 Π cfgKG
           (in-hole C_1
                    (local (Name : t_1) ... = e_1 ... hole in
                      s end))
           e_2 Γ_2 t_2)
   --------------------------------------------------------------------
   (type_loc_rval Γ_1 Π cfgKG C_1
                  (local (Name : t_1) ... = e_1 ... hole in s end)
                  e_2 Γ_2 (t_2))]
  )

; bind type of rvalues with local vars, in Γ
(define-judgment-form
  lang-reach-defs
  #:mode (bind_loc_vars I I I I I O)
  #:contract (bind_loc_vars Γ
                            ((Name : t)... hole (Name : t) ...)
                            (Name : t)
                            (t ... hole ... t ...)
                            t
                            Γ)

  ; rvalues left to bind with Name
  ; Not the last var
  [; check subtyping
   (side-condition (subtyping_rel t_8 t_4))
   
   ; bind Name_4 with t_4
   (where Γ_2 (set Γ_1 Name_4 t_4))
   
   ; bind remaining loc. vars.
   (bind_loc_vars Γ_2 ((Name_1 : t_1) ... (Name_4 : t_4) hole (Name_3 : t_3)
                                      ...) (Name_2 : t_2)
                                           (t_5 ... t_8 hole t_7 ...)
                                           t_6 Γ_3)
   --------------------------------------------------------------------
   (bind_loc_vars Γ_1
                  ((Name_1 : t_1) ... hole (Name_2 : t_2) (Name_3 : t_3)  ...)
                  (Name_4 : t_4)
                  (t_5 ... hole t_6 t_7 ...)
                  t_8 Γ_3)]

  ; Last var
  [; check subtyping
   (side-condition (subtyping_rel t_5 t_2))
   
   ; bind Name_4 with t_4
   (where Γ_2 (set Γ_1 Name_2 t_2))
   --------------------------------------------------------------------
   (bind_loc_vars Γ_1
                  ((Name_1 : t_1) ... hole)
                  (Name_2 : t_2)
                  (t_3 ... hole t_4 ...)
                  t_5 Γ_2)]

  ; last rvalues
  ; Not the last var
  [; check subtyping
   (side-condition (subtyping_rel t_6 t_4))
   
   ; bind Name_4 with t_4
   (where Γ_2 (set Γ_1 Name_4 t_4))
   
   ; bind remaining loc. vars.
   (bind_loc_vars Γ_2 ((Name_1 : t_1) ... (Name_4 : t_4) hole (Name_3 : t_3)
                                      ...) (Name_2 : t_2)
                                           (t_5 ... t_6)
                                           t_6 Γ_3)
   --------------------------------------------------------------------
   (bind_loc_vars Γ_1
                  ((Name_1 : t_1) ... hole (Name_2 : t_2) (Name_3 : t_3)  ...)
                  (Name_4 : t_4)
                  (t_5 ... hole)
                  t_6 Γ_3)]

  ; last var
  [; check subtyping
   (side-condition (subtyping_rel t_4 t_2))
   
   ; bind Name_4 with t_4
   (where Γ_2 (set Γ_1 Name_2 t_2))
   --------------------------------------------------------------------
   (bind_loc_vars Γ_1
                  ((Name_1 : t_1) ... hole)
                  (Name_2 : t_2)
                  (t_3 ... hole)
                  t_4 Γ_2)]

  ; no rvalues left
  ; assign nil, no need to check for subtyping
  [; bind Name_4 with t_4
   (where Γ_2 (set Γ_1 Name_4 t_4))
   
   ; bind remaining loc. vars.
   (bind_loc_vars Γ_2 ((Name_1 : t_1) ... (Name_4 : t_4) hole (Name_3 : t_3)
                                      ...) (Name_2 : t_2)
                                           (t_5 ...)
                                           t_6 Γ_3)
   --------------------------------------------------------------------
   (bind_loc_vars Γ_1
                  ((Name_1 : t_1) ... hole (Name_2 : t_2) (Name_3 : t_3)  ...)
                  (Name_4 : t_4)
                  (t_5 ...)
                  t_6 Γ_3)]

  [; bind Name_2 with t_2
   (where Γ_2 (set Γ_1 Name_2 t_2))
   --------------------------------------------------------------------
   (bind_loc_vars Γ_1
                  ((Name_1 : t_1) ... hole)
                  (Name_2 : t_2)
                  (t_3 ...)
                  t_4 Γ_2)]
  )

; extension to type_e, to manage generation of contexts for fun call params. 
(define-judgment-form
  lang-reach-defs
  #:mode (type_stat_fun_call_param I I I I I I O O)
  #:contract (type_stat_fun_call_param Γ Π cfgKG C C any Γ (t ...))

  [(type_e Γ_1 Π cfgKG
           (in-hole C_1 ($statFunCall prefixexp (e_2 ... hole)))
           e_3 Γ_2 t_1)
   --------------------------------------------------------------------
   (type_stat_fun_call_param Γ_1 Π cfgKG C_1
                             ($statFunCall prefixexp (e_2 ... hole)) e_3 Γ_2
                             (t_1))]

  [(type_e Γ_1 Π cfgKG
           (in-hole C_1 ($statFunCall prefixexp (e_2 ... hole e_3 e_4 ...)))
           e_5 Γ_2 t_1)

   (type_stat_fun_call_param Γ_2 Π cfgKG C_1
                             ($statFunCall prefixexp (e_2 ... e_5 hole e_4 ...))
                             e_3 Γ_3 (t_2 ...))
   --------------------------------------------------------------------
   (type_stat_fun_call_param Γ_1 Π cfgKG C_1
                             ($statFunCall prefixexp (e_2 ... hole e_3 e_4 ...))
                             e_5 Γ_3 (t_1 t_2 ...))]
  
  )

(define-judgment-form
  lang-reach-defs
  #:mode (type_stat_method_call_param I I I I I I O O)
  #:contract (type_stat_method_call_param Γ Π cfgKG C C any Γ any)

  [(type_e Γ_1 Π cfgKG (in-hole C_1
                                ($statFunCall prefixexp : Name (e_2 ... hole)))
           e_3 Γ_2 t_1)
   --------------------------------------------------------------------
   (type_stat_method_call_param Γ_1 Π cfgKG C_1
                                ($statFunCall prefixexp : Name (e_2 ... hole))
                                e_3 Γ_2 (t_1))]

  [(type_e Γ_1 Π cfgKG
           (in-hole C_1
                    ($statFunCall prefixexp : Name (e_2 ... hole e_3 e_4 ...)))
           e_5 Γ_2 t_1)

   (type_stat_method_call_param
    Γ_2 Π cfgKG C_1
    ($statFunCall prefixexp : Name (e_2 ... e_5 hole e_4 ...))
    e_3 Γ_3 (t_2 ...))
   --------------------------------------------------------------------
   (type_stat_method_call_param
    Γ_1 Π cfgKG C_1
    ($statFunCall prefixexp : Name (e_2 ... hole e_3 e_4 ...))
    e_5 Γ_3 (t_1 t_2 ...))]
  )

(define-judgment-form
  lang-reach-defs
  #:mode (type_s I I I I I O)
  #:contract (type_s Γ Π cfgKG C any Γ)           

  [-------------------------
   (type_s Γ Π cfgKG C \; Γ)]
  
  [-------------------------
   (type_s Γ Π cfgKG C break Γ)]

  [------------------------------------------------------------------
   (type_s Γ Π cfgKG C (return) Γ)]

  [(type_return Γ_1 Π cfgKG C (return hole e_2 ...) e_1 Γ_2)
   ------------------------------------------------------------------
   (type_s Γ_1 Π cfgKG C (return e_1 e_2 ...) Γ_2)]

  [; TODO: better way to assert that we are not calling setmetatable?
   (side-condition ,(not (equal? (term ($ENV |[| "setmetatable" |]|))
                                 (term prefixexp))))
   ; TODO: recursive function?
   (type_e Γ_1 Π cfgKG C prefixexp Γ_2 (($tup) -> t_2))
   -------------------------------------------------
   (type_s Γ_1 Π cfgKG C ($statFunCall prefixexp ()) Γ_2)]

  [; TODO: better way to assert that we are not calling setmetatable?
   (side-condition ,(not (equal? (term ($ENV |[| "setmetatable" |]|))
                                 (term prefixexp))))
   ; TODO: recursive function?
   (type_e Γ_1 Π cfgKG C prefixexp Γ_2 (($tup t_1 ...) -> t_2))
   (type_stat_fun_call_param Γ_2 Π cfgKG C
                             ($statFunCall prefixexp (hole e_2 ...)) e_1 Γ_3
                             (t_3 ...))

   ; actual parameters are a subtype of formal parameters
   ;(side-condition (subtyping_rel ($tup t_3 ...) ($tup t_1 ...)))
   -----------------------------------------------------------------------
   (type_s Γ_1 Π cfgKG C ($statFunCall prefixexp (e_1 e_2 ...)) Γ_3)]
                                                     
  [(type_e Γ_1 Π cfgKG (in-hole C ($statFunCall ($ENV |[| "setmetatable" |]|)
                                                (hole e)))
           Name Γ_2 ((\{ (\[ t_1 \] : t_2) ... \}) weakness))
   
   (type_e Γ_2 Π cfgKG (in-hole C ($statFunCall ($ENV |[| "setmetatable" |]|)
                                                (Name hole)))
            
           e Γ_3 ((\{ _ ... (\[ ("__mode" : str) \] : (String : str)) _ ...
                      \}) _))
   (side-condition ,(string-contains? (term String) "k")) 
   
   (where Γ_4 (set Γ_3 Name ((\{ (\[ t_1 \] : t_2) ... \}) wk))) 
   ---------------------------------------------------------------------------
   (type_s Γ_1 Π cfgKG C
           ($statFunCall ($ENV |[| "setmetatable" |]|) (Name e))
           Γ_4)]

  [(type_e Γ_1 Π cfgKG (in-hole C ($statFunCall ($ENV |[| "setmetatable" |]|)
                                                (hole e)))
           Name Γ_2 ((\{ (\[ t_1 \] : t_2) ... \}) weakness))
   
   (type_e Γ_2 Π cfgKG (in-hole C ($statFunCall ($ENV |[| "setmetatable" |]|)
                                                (Name hole)))
            
           e Γ_3 ((\{ _ ... (\[ ("__mode" : str) \] : (String : str)) _ ...
                      \}) _))
   (side-condition ,(string-contains? (term String) "v"))
   (where Γ_4 (set Γ_3 Name ((\{ (\[ t_1 \] : t_2) ... \}) wv))) 
   ---------------------------------------------------------------------------
   (type_s Γ_1 Π cfgKG C
           ($statFunCall ($ENV |[| "setmetatable" |]|) (Name e))
           Γ_4)]

  [(type_e Γ_1 Π cfgKG (in-hole C ($statFunCall ($ENV |[| "setmetatable" |]|)
                                                (hole e)))
           Name Γ_2 ((\{ (\[ t_1 \] : t_2) ... \}) weakness))
   (type_e Γ_2 Π cfgKG (in-hole C ($statFunCall ($ENV |[| "setmetatable" |]|)
                                                (Name hole)))
           e Γ_3 ((\{ _ ... (\[ ("__mode" : str) \] : (String : str)) _ ...
                      \}) _))
   (side-condition ,(and (string-contains? (term String) "k")
                         (string-contains? (term String) "v")))
   (where Γ_4 (set Γ_3 Name ((\{ (\[ t_1 \] : t_2) ... \}) wkv))) 
   ---------------------------------------------------------------------------
   (type_s Γ_1 Π cfgKG C
           ($statFunCall ($ENV |[| "setmetatable" |]|) (Name e))
           Γ_4)]

  ; meta-table with not weakness information
  [(type_e Γ_1 Π cfgKG (in-hole C ($statFunCall ($ENV |[| "setmetatable" |]|)
                                                (hole e)))
           Name Γ_2 ((\{ (\[ t_1 \] : t_2) ... \}) weakness_1))
   (type_e Γ_2 Π cfgKG (in-hole C ($statFunCall ($ENV |[| "setmetatable" |]|)
                                                (Name hole)))
           e Γ_3 ((\{ (\[ t_3 \] : t_4) ... \}) weakness_2))
   
   (side-condition
    ,(not (redex-match? lang-reach-defs
                        (side-condition
                         ((\{ _ ... (\[ ("__mode" : str) \] : String)
                              _ ... \}) _)
                         (or (string-contains? (term String) "k")
                             (string-contains? (term String) "v")))
                        (term ((\{ (\[ t_3 \] : t_4) ... \})
                               weakness_2)))))
   
   ; dynamic semantics dictates that the table weakness changes to strong
   (where Γ_4 (set Γ_3 Name ((\{ (\[ t_1 \] : t_2) ... \}) strong))) 
   ---------------------------------------------------------------------------
   (type_s Γ_1 Π cfgKG C
           ($statFunCall ($ENV |[| "setmetatable" |]|) (Name e))
           Γ_4)]
  
  ; TODO: not checking for membership of Name to prefixexp
  [; TODO: better way to assert that we are not calling setmetatable?
   (side-condition ,(or (not (equal? (term $ENV)
                                     (term prefixexp)))
                        (not (equal? (term Name)
                                     (term setmetatable)))))
   ; TODO: recursive function?
   ; prefixexp is an object with the corresponding method
   (type_e Γ_1 Π cfgKG C prefixexp Γ_2 ((\{ any_1 ...
                                            (\[ t_1 \] : (($tup) -> t_2))
                                            any_2 ... \}) weakness))
   (side-condition (subtyping_rel t_1 str))
   -------------------------------------------------
   (type_s Γ_1 Π cfgKG C ($statFunCall prefixexp : Name ()) Γ_2)]

  [; TODO: better way to assert that we are not calling setmetatable?
   (side-condition ,(not (equal? (term ($ENV |[| "setmetatable" |]|))
                                 (term prefixexp))))
   ; TODO: recursive function?
   ; prefixexp is an object with the corresponding method
   (type_e Γ_1 Π cfgKG C prefixexp Γ_2
           ((\{ any_1 ...
                (\[ t_1 \] : (($tup t_2 ...) -> t_3))
                any_2 ... \}) weakness))
   
   (side-condition (subtyping_rel t_1 str))

   ; actual parameters are a subtype of formal parameters
   (type_stat_method_call_param
    Γ_2 Π cfgKG C ($statFunCall prefixexp : Name (hole e_2 ...)) e_1
    Γ_3 (t_4 ...))

   (side-condition (subtyping_rel ($tup t_4 ...) ($tup t_2 ...)))
   -----------------------------------------------------------------------
   (type_s Γ_1 Π cfgKG C ($statFunCall prefixexp : Name (e_1 e_2 ...)) Γ_3)]
  
  [(type_s Γ_1 Π cfgKG (in-hole C (do hole end)) s Γ_2)
   -------------------------------------
   (type_s Γ_1 Π cfgKG C (do s end) Γ_2)]

  [(type_e Γ_1 Π cfgKG (in-hole C (if hole then s_1 else s_2 end)) e Γ_2 t)
   (type_s Γ_2 Π cfgKG (in-hole C (if e then hole else s_2 end)) s_1 Γ_3)
   (type_s Γ_2 Π cfgKG (in-hole C (if e then s_1 else hole end)) s_2 Γ_4)
   -------------------------------------
   (type_s Γ_1 Π cfgKG C (if e then s_1 else s_2 end) (supremum_env Γ_3 Γ_4))]
  
  [(type_e Γ_1 Π cfgKG (in-hole C (while hole do s end)) e Γ_2 t)
   (type_s Γ_2 Π cfgKG (in-hole C (while e do hole end)) s Γ_3)
   -------------------------------------
   (type_s Γ_1 Π cfgKG C (while e do s end) (supremum_env Γ_2 Γ_3))]

  [(type_conc_stats Γ_1 Π cfgKG C_1 (hole s_2 s_3 ...) s_1 Γ_2)
   --------------------------------------------------------------
   (type_s Γ_1 Π cfgKG C_1 (s_1 s_2 s_3 ...) Γ_2)]

  ; var. assign.
  ; assumption: types of keys, in field assing., are exclusive
  [(type_lrvalues Γ_1 Π cfgKG C (hole var_2 ...  = e ...) var_1 Γ_2
                  (any_1 any_2 ...) ; lvalues 
                  (t_1 t_2 ...) ; rvalues
                  )
   (type_assign Γ_2 (hole any_2 ...) any_1 (hole t_2 ...) t_1 Γ_3)
   --------------------------------------------------------------
   (type_s Γ_1 Π cfgKG C (var_1 var_2 ... = e ...) Γ_3)]
  
  ; loc. var. def.
  [; type check rvalues
   (type_loc_rval Γ_1 Π cfgKG C (local (Name_1 : t_1) (Name_2 : t_2) ... =
                                  hole e_2 ... in s end) e_1 Γ_2 (t_3 t_4 ...))

   ; type check type annotations
   (bind_loc_vars Γ_2 (hole (Name_2 : t_2) ...) (Name_1 : t_1) (hole t_4 ...)
                  t_3 Γ_3)

   ; type check body
   (type_s Γ_3 Π cfgKG
           (in-hole C (local (Name_1 : t_1) (Name_2 : t_2) ... = e_1 e_2 ... in
                        hole end))
           s Γ_4)

   ; remove bindings from local vars in Γ_4
   (where Γ_5 (del* Γ_4 (Name_1 Name_2 ...)))
   ------------------------------------------------------------------
   (type_s Γ_1 Π cfgKG C
           (local (Name_1 : t_1) (Name_2 : t_2) ... = e_1 e_2 ... in s end)
           Γ_5)]
  )

(provide type_s)


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
; impose a tree structure over set of reach. defs., taking into account
; reachabilitys
(define-metafunction lang-reach-defs
  build_tree : ((Name = e) ...) -> ioTree

  [(build_tree ())
   empty]

  [(build_tree ((Name_1 = e_1) (Name_2 = e_2) ...))
   (; fst level
    ((Name_3 = e_3) ...) ioTree)

   ; recognize first level
   (where (((Name_3 = e_3) ...) ((Name_4 = e_4) ...))
          (fst_lev ((Name_1 = e_1) (Name_2 = e_2) ...)
                   ((Name_1 = e_1) (Name_2 = e_2) ...)))

   ; continue with remaining levels
   (where ioTree
          (build_tree ((Name_4 = e_4) ...)))
   ]
  )

(provide build_tree)

; recognizes the var. defs that belongs to the first level of an ioTree. That
; is, var. which do no appear as rvalues.
; PARAMS
; ((Name_1 = e_1) ...) : original list of reach. defs
; ((Name_2 = e_2) ...) : list of reach. defs over which recursion is done
; RETURNS
; (var. defs from the first level, vars from the remaining levels)
(define-metafunction lang-reach-defs
  fst_lev : ((Name = e) ...) ((Name = e) ...) -> (((Name = e) ...)
                                                  ((Name = e) ...))

  [(fst_lev _ ())
   (() ())]

  ; not from the fst. level
  [(fst_lev ((Name_1 = e_1) ...)
            ((Name_2 = e_2) (Name_3 = e_3) ...))
   (((Name_7 = e_6) ...) ((Name_2 = e_2) (Name_8 = e_7) ...))

   ; Name_2 appears as rvalue
   (where
    (side-condition
     ((Name_4 = e_4) ...
      (Name_5 = (in-hole C Name_2))
      (Name_6 = e_5) ...)
     ; check that Name_5 != Name_2, to avoid loops
     (not (equal? (term Name_2) (term Name_5))))
    
    ((Name_1 = e_1) ...))

   (where (((Name_7 = e_6) ...) ((Name_8 = e_7) ...))
          (fst_lev ((Name_1 = e_1) ...)
                   ((Name_3 = e_3) ...)))]

  ; fst. level
  [(fst_lev ((Name_1 = e_1) ...)
            ((Name_2 = e_2) (Name_3 = e_3) ...))
   (((Name_2 = e_2) (Name_4 = e_4) ...) ((Name_5 = e_5) ...))

   (where (((Name_4 = e_4) ...) ((Name_5 = e_5) ...))
          (fst_lev ((Name_1 = e_1) ...)
                   ((Name_3 = e_3) ...)))]
  
  )

(define-metafunction lang-reach-defs
  reachCte_stat : (e ioTree Γ) -> any

  ; begin traversal of reach tree from each identifier in scope 
  [(reachCte_stat (side-condition
                   (e_1 (((Name_1 = e_2) ...
                          (Name_2 = e_3)
                          (Name_3 = e_4) ...) ioTree) Γ)
                   (term (reachCte_stat_aux (e_1 Name_2
                                                 (((Name_1 = e_2) ...
                                                   (Name_2 = e_3)
                                                   (Name_3 = e_4) ...) ioTree)
                                                 
                                                 (((Name_1 = e_2) ...
                                                   (Name_2 = e_3)
                                                   (Name_3 = e_4) ...) ioTree)
                                                 Γ)))))
   #t]
  
  [(reachCte_stat _)
   #f]
  )

(provide reachCte_stat)

; reachability from a given exp.
; PARAMS
; e_1 : exp. for which we want to determine reachability
; e_2 : exp. from which we begin to traverse the reach. tree
; ioTree_1 : reach. tree over which the traversal is done
; ioTree_2 : original reach. tree, to treat the case of ephemerons
; Γ : Γ environment from the point where e_1 occurs
(define-metafunction lang-reach-defs
  reachCte_stat_aux : (e e ioTree ioTree Γ) -> any

  ; TODO: ioTree should be the next level
  ; base case: occurs literally in the actual term
  [(reachCte_stat_aux (e (in-hole C e) _ _ _))
   #t]

  ; dereference step: variable assigned to a table
  [(reachCte_stat_aux (side-condition
                       (e_1 (in-hole C Name_1)
                            (((Name_3 = e_3) ... 
                              (Name_1 = (\{ field ... \}))
                              (Name_4 = e_5) ...) ioTree_1) ioTree_2 Γ)
                       
                       ; assumption: binding in Γ refers to (Name_1 = e_4)
                       (term (reachTable_stat
                              (e_1
                               (SO_stat (\{ field ... \})
                                        (index Γ Name_1))
                               ; dereference: next level in the reach
                               ; tree: ioTree_1 
                               ioTree_1
                               ioTree_2
                               Γ
                               Name_1))))
                      )
   #t]

  ; ; dereference step: variable is not assigned to a table 
  [(reachCte_stat_aux (side-condition
                       (e_1 (in-hole C Name_1)
                            (((Name_3 = e_3) ... 
                              (Name_1 = e_4)
                              (Name_4 = e_5) ...)
                             ioTree_1)
                            ioTree_2 Γ)
                       
                       (and (not (redex-match? lang-reach-defs
                                               (\{ field ... \})
                                               (term e_4)))
                            (term (reachCte_stat_aux
                                   (e_1
                                    e_4
                                    ; dereference: next level in the reach
                                    ; tree: ioTree_1 
                                    ioTree_1
                                    ioTree_2
                                    Γ))))
                       ))
   #t]

  ; default case
  [(reachCte_stat_aux _)
   #f]
  )

(provide reachCte_stat_aux)

; function SO adapted to the context of static analysis
(define-metafunction lang-reach-defs
  SO_stat : tableconstructor ((\{ (\[ t \] : t) ... \}) weakness) -> (any ...)

  ; wv
  [(SO_stat (\{ field ... \}) ((\{ (\[ t_1 \] : t_2) ... \}) wv))
   any

   ; only the keys are referred by strong references
   (where any (cteKeys_stat (field ...) ((\[ t_1 \] : t_2) ...)))]

  ; strong
  [(SO_stat (\{ field ... \}) ((\{ (\[ t_1 \] : t_2) ... \}) strong))
   any

   ; keys and values are referred by strong references
   (where any (cteFields_stat (field ...) ((\[ t_1 \] : t_2) ...)))]

  ; wk
  [(SO_stat (\{ field ... \}) ((\{ (\[ t_1 \] : t_2) ... \}) wk))
   any

   ; ephemeron table
   (where any (cteEphemeron_stat (field ...) ((\[ t_1 \] : t_2) ...)))]

  ; wv and wk
  [(SO_stat _ _)
   ()]
  )

(provide SO_stat)

(define-metafunction lang-reach-defs
  cteKeys_stat : (field ...) ((\[ t \] : t) ...) -> (e ...)

  [(cteKeys_stat () _)
   ()]
  
  [(cteKeys_stat ((\[ e_1 \] = _) field ...) ((\[ ctet \] : _)
                                              (\[ t_1 \] : t_2) ...))
   (e_1 e_2 ...)

   (where (e_2 ...) (cteKeys_stat (field ...) ((\[ t_1 \] : t_2) ...)))]

  [(cteKeys_stat (field_1 field_2 ...) ((\[ t_1 \] : t_2) (\[ t_3 \] : t_4) ...))
   (cteKeys_stat (field_2 ...) ((\[ t_3 \] : t_4) ...))]
  )

(provide cteKeys_stat)

(define-metafunction lang-reach-defs
  cteFields_stat : (field ...) ((\[ t \] : t) ...) -> (e ...)

  [(cteFields_stat () _)
   ()]

  ; key and values in ctet
  [(cteFields_stat ((\[ e_1 \] = e_2) field ...)
                   ((\[ ctet_1 \] : ctet_2) (\[ t_1 \] : t_2) ...))
   (e_1 e_2 e_3 ...)

   (where (e_3 ...) (cteFields_stat (field ...) ((\[ t_1 \] : t_2) ...)))]

  ; key in ctet
  [(cteFields_stat ((\[ e_1 \] = e_2) field ...) ((\[ ctet \] : t_1)
                                                  (\[ t_2 \] : t_3) ...))
   (e_1 e_3 ...)

   (where (e_3 ...) (cteFields_stat (field ...) ((\[ t_1 \] : t_2) ...)))]

  ; value in ctet
  [(cteFields_stat ((\[ e_1 \] = e_2) field ...) ((\[ t \] : ctet)
                                                  (\[ t_1 \] : t_2) ...))
   (e_2 e_3 ...)

   (where (e_3 ...) (cteFields_stat (field ...) ((\[ t_1 \] : t_2) ...)))]

  ; Default case: no CTEs in field
  [(cteFields_stat (field_1 field_2 ...) ((\[ t_1 \] : t_2)
                                          (\[ t_3 \] : t_4) ...))
   (cteFields_stat (field_2 ...) ((\[ t_3 \] : t_4) ...))]
  )

(provide cteFields_stat)

(define-metafunction lang-reach-defs
  cteEphemeron_stat : (field ...) ((\[ t \] : t) ...) -> ((e e) ...)

  [(cteEphemeron_stat () _)
   ()]
  
  [(cteEphemeron_stat ((\[ e_1 \] = e_2) field ...) ((\[ t_1 \] : ctet)
                                                     (\[ t_2 \] : t_3) ...))
   ((e_1 e_2) (e_3 e_4) ...)

   (where ((e_3 e_4) ...)
          (cteEphemeron_stat (field ...) ((\[ t_2 \] : t_3) ...)))]

  ; Default case: no CTEs in field
  [(cteEphemeron_stat (field_1 field_2 ...) ((\[ t_1 \] : t_2)
                                             (\[ t_3 \] : t_4) ...))
   (cteEphemeron_stat (field_2 ...)
                      ((\[ t_3 \] : t_4) ...))]
  )

; predicate reachTable adapted to the context of static analysis
; PARAMS:
; e : exp. for which we want to determine reachability
; (any ...) : list of ctes that occurs strongly in the table
; ioTree_1 : level of the reach tree from which the content of the table belongs
; ioTree_2 : full reach. tree
; Γ : the actual environment 
; Name : variable identifier bound to the table
(define-metafunction lang-reach-defs
  reachTable_stat : (e_1 (any ...) ioTree ioTree Γ Name) -> any
  ; simplification: not taking into account meta-tables
  
  ; ephemerons
  [(reachTable_stat (side-condition (e_1 (any_1 ... (e_2 e_3) any_2 ...)
                                         ioTree_1 ioTree_2
                                         Γ Name)
                                    (term (eph_stat e_1 (e_1 e_2) ioTree_1
                                                    ioTree_2 Γ Name))))
   #t]

  [(reachTable_stat (side-condition (e_1 (e_2 ... e_3 e_4 ...) ioTree_1 ioTree_2
                                         Γ Name)
                                    (term (reachCte_stat_aux (e_1 e_3 ioTree_1
                                                                  ioTree_2
                                                                  Γ))))
                    )
   #t]

  ; default case
  [(reachTable_stat _)
   #f]
  )

(define-metafunction lang-reach-defs
  eph_stat : e (e e) ioTree ioTree Γ Name -> any

  ; key is a cte
  [(eph_stat e_1 (cte e_2) ioTree_1 ioTree_2 Γ Name)
   ,(and (term (reachCte_stat_aux (e_1 e_2 ioTree_1 ioTree_2 Γ)))
         (term (reachCte_stat (cte
                               ; remove eph. pair (cte e_2) from the table, to
                               ; avoid taking it into account again
                               (remove_eph ioTree_2 Name (cte e_2))
                               Γ))))]

  ; key is not a cte
  [(eph_stat e_1 (e_2 e_3) ioTree_1 ioTree_2 Γ Name)
   (reachCte_stat_aux (e_1 e_3 ioTree_1 ioTree_2 Γ))]
  )

; when deciding for reachibility through an ephemeron, we need to remove the
; ephemeron considered, to allow the collection of a field where
; the only reference to the key comes from the value.
; This meta-function removes a given ephemeron pair from a given tree
; PARAMS
; ioTree : the tree from which we need to remove the ephemeron
; Name : variable identifier bound to the table to which the eph. pair belongs
; (e e) : ephemeron pair
(define-metafunction lang-reach-defs
  remove_eph : ioTree Name (e e) -> ioTree

  [(remove_eph empty _ _)
   empty]
  
  [(remove_eph (((Name_1 = e_1) ...
                 (Name_2 = (\{ field_1 ... (\[ e_2 \] = e_3) field_2 ... \}))
                 (Name_3 = e_4) ...) ioTree) Name_2 (e_2 e_3))
   
   (((Name_1 = e_1) ...
     (Name_2 = (\{ field_1 ... field_2 ... \}))
     (Name_3 = e_2) ...) ioTree)]

  ; def. of Name_2 is not in the actual level of the tree
  [(remove_eph (((Name_1 = e_1) ...) ioTree_1) Name_2 (e_2 e_3))
   (((Name_1 = e_1) ...) ioTree_2)

   (where ioTree_2 (remove_eph ioTree_1 Name_2 (e_2 e_3)))]

  )