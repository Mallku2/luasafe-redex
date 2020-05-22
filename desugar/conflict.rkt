#lang racket


(require parser-tools/yacc
         parser-tools/lex
         (prefix-in re- parser-tools/lex-sre))


;                                          
;                                          
;   ;;;                                    
;     ;                                    
;     ;                                    
;     ;                                    
;     ;      ;;;;   ;;  ;;   ;;;;    ;;;;  
;     ;     ;;  ;;   ;  ;   ;;  ;;   ;;  ; 
;     ;     ;    ;    ;;    ;    ;   ;     
;     ;     ;;;;;;    ;;    ;;;;;;   ;     
;     ;     ;         ;;    ;        ;     
;     ;     ;;   ;   ;  ;   ;;   ;   ;     
;      ;;;   ;;;;   ;;  ;;   ;;;;    ;     
;                                          
;                                          
;                                          
;                                          

(define-tokens non-empty-tokens (NUMBER))

(define-lex-abbrevs
  (digits (char-range "0" "9"))

  ; Decimal number, with optional fractional part
  (simp-number-lit (re-or (concatenation (re-+ digits)
                                         (re-? ".")
                                         (re-* digits))

                          (concatenation (re-* digits)
                                         (re-? ".")
                                         (re-+ digits))))
  
  ; Decimal number, with optional fractional part and decimal exponent
  (scient-number-lit (concatenation simp-number-lit
                                    (re-or "e" "E")
                                    (re-or (concatenation (re-or "-" "+")
                                                          (re-+ digits))
                                           (re-+ digits))))
  
  (number-lit (re-or simp-number-lit
                     scient-number-lit))

  )

(define-empty-tokens empty-tokens (EOF))


; Lexer for Lua
(define lua-lexer
  (lexer ; literals
   
   (number-lit (token-NUMBER (exact->inexact (string->number lexeme))))

   ; skip whitespaces
   (whitespace (lua-lexer input-port))
   
   ((eof) (token-EOF))))

; Racket's strings "prints using doublequotes, where doublequote and backslash
; characters within the string are escaped with backslashes"
; (from https://docs.racket-lang.org/guide/strings.html). Which is not the case
; with Lua's string. We implement this procedure to eliminate redundant escape
; characters.
(define (clean string)
  (define aux string)
  
  ; Unescape backslashs from \n
  (set! aux (string-replace aux "\\n" "\n"))
  (set! aux (string-replace aux "\\\n" "\n"))
  ; Unescape backslashs from \0
  (set! aux (string-replace aux "\\0" "\0"))
  (set! aux (string-replace aux "\\\0" "\0"))
    
  (if (equal? aux string)
      string ; Nothing left to be cleaned
      (clean aux))
  )

; Translate to Racket's hexadecimal numbers' notation
(define (2-racket-hex number)
  (string-replace number (regexp "0x|0X") "#x"))


(provide lua-lexer)


;                                                  
;                                                  
;                                                  
;                                                  
;                                                  
;                                                  
;   ;;;;;     ;;;    ;;;;    ;;;;    ;;;;    ;;;;  
;   ;;  ;;   ;   ;   ;;  ;  ;    ;  ;;  ;;   ;;  ; 
;   ;    ;       ;   ;      ;       ;    ;   ;     
;   ;    ;   ;;;;;   ;       ;;;;   ;;;;;;   ;     
;   ;    ;  ;    ;   ;           ;  ;        ;     
;   ;;  ;;  ;   ;;   ;      ;    ;  ;;   ;   ;     
;   ;;;;;    ;;; ;   ;       ;;;;    ;;;;    ;     
;   ;                                              
;   ;                                              
;   ;                                              
;

(struct chain (l m r))

; Parser for Lua
(define lua-parser
  (parser
   
   ; Start symbol
   (start S)
   
   (end EOF)
   
   (error (lambda (tok-ok? tok-name tok-value)
            (error "Parser error. Token name:" tok-name)))

   (debug "salida")
   
   ; Tokens declaration
   (tokens empty-tokens
           non-empty-tokens)
   
   ; Prec. declarations, to avoid shit-reduce conflicts
   ;(precs (nonassoc BEGINNING_WHILE))
   
   ; Lua's grammar
   (grammar
    (S
     ((NUMBER NUMBER S) (chain 0 $2 0))
     (() (list))
     )
    )
   
   ))

(provide lua-parser)


;                                                                          
;                             ;     ;;;       ;                            
;                                     ;                                    
;      ;                              ;                                    
;     ; ;                             ;                                    
;     ; ;   ;    ;  ;;  ;;  ;;;       ;     ;;;       ;;;    ;;;;   ;    ; 
;     ; ;   ;    ;   ;  ;     ;       ;       ;      ;   ;   ;;  ;   ;   ; 
;    ;   ;  ;    ;    ;;      ;       ;       ;          ;   ;       ;  ;  
;    ;   ;  ;    ;    ;;      ;       ;       ;      ;;;;;   ;       ;  ;  
;    ;;;;;  ;    ;    ;;      ;       ;       ;     ;    ;   ;        ; ;  
;   ;;   ;; ;   ;;   ;  ;     ;       ;       ;     ;   ;;   ;        ;;   
;   ;     ;  ;;; ;  ;;  ;;  ;;;;;      ;;;  ;;;;;    ;;; ;   ;         ;   
;                                                                      ;   
;                                                                     ;    
;                                                                    ;;    

; From doc of 2 LALR(1) Parsers: The result of a parser expression with one
; start non-terminal is a function, parse, that takes one argument. This
; argument must be a zero argument function, gen, that produces successive
; tokens of the input each time it is called. 
(define (lex-this lexer input) (lambda () (lexer input)))

(define (rep str)
  (match str
    ((chain l m r)  (append (list l)
                            (rep m)
                            (list r)))

    ; maybe it is a functioncall
    (_ (list))))

(define (parse-this input runtime? ref)
  (rep
   (lua-parser (lex-this lua-lexer (open-input-string input))))
  )

(provide parse-this)
