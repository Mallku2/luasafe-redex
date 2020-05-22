#lang racket
(require redex
         "phrases_constructors.rkt"
         rackunit
         rackunit/text-ui)

(define-test-suite phrases-constructors-test-suite
  ; statements
  (check-equal? (concrete-grammar-s (skip))
             (term \;))
  
  (check-equal? (concrete-grammar-s (break))
             (term break))

  (check-equal? (concrete-grammar-s (return (exps (list (tuple (exps (list (nmbr 1))))))))
                (term (return (< 1 >))))

  (check-equal? (concrete-grammar-s (fun-call (id-name 'x) (exps (list (nmbr 1)))))
             (term (x (1))))

  (check-equal? (concrete-grammar-s (method-call (id-name 'x) (id-name 'y) (exps (list (nmbr 1)))))
             (term (x : y (1))))

  (check-equal? (concrete-grammar-s (built-in-call (id-name 'service) (exps (list (nmbr 1)))))
             (term (\$builtIn service (1))))

  (check-equal? (concrete-grammar-s (conditional (nil) (skip) (break)))
          (term (if nil then \; else break end)))

  (check-equal? (concrete-grammar-s (while (true) (skip)))
          (term (while true do \; end)))

  (check-equal? (concrete-grammar-s (local-vars (exps (list (id-name 'x) (id-name 'y)))
                                                (exps (list (nmbr 1)))
                                                (skip)))
          (term (local x y = 1 in \; end)))

  (check-equal? (concrete-grammar-s (var-assign (exps (list (id-name 'x) (id-name 'y)))
                                                (exps (list (nmbr 1) (nmbr 2)))))
          (term (x y = 1 2)))

  (check-equal? (concrete-grammar-s (conc-stats (list (skip)
                                                      (var-assign (exps (list (id-name 'x) (id-name 'y)))
                                                                  (exps (list (nmbr 1) (nmbr 2)))))))
                (term (\;
                       (x y = 1 2))))

  ; expressions
  (check-equal? (concrete-grammar-e (nil))
          (term nil))

  (check-equal? (concrete-grammar-e (true))
          (term true))

  (check-equal? (concrete-grammar-e (false))
          (term false))

  (check-equal? (concrete-grammar-e (nmbr 1))
          (term 1))

  (check-equal? (concrete-grammar-e (nmbr 1.1))
          (term 1.1))

  (check-equal? (concrete-grammar-e (str "asd"))
          (term "asd"))

  (check-equal? (concrete-grammar-e (id-name 'asd))
          (term asd))

  (check-equal? (concrete-grammar-e (id-vararg))
          (term <<<))

  (check-equal? (concrete-grammar-e (var-table-field (nmbr 1) (nmbr 2)))
          (term (1 \[ 2 \])))

  (check-equal? (concrete-grammar-e (parent-e (nmbr 1)))
          (term (\( 1 \))))

  (check-equal? (concrete-grammar-e (binop (add)
                                                (nmbr 1)
                                                (nmbr 2)))
                (term (1 + 2)))

  (check-equal? (concrete-grammar-e (binop (sub)
                                                (nmbr 1)
                                                (nmbr 2)))
                (term (1 - 2)))

  (check-equal? (concrete-grammar-e (binop (mul)
                                                (nmbr 1)
                                                (nmbr 2)))
                (term (1 * 2)))

  (check-equal? (concrete-grammar-e (binop (div)
                                                (nmbr 1)
                                                (nmbr 2)))
                (term (1 / 2)))

  (check-equal? (concrete-grammar-e (binop (pow)
                                                (nmbr 1)
                                                (nmbr 2)))
                (term (1 ^ 2)))

  (check-equal? (concrete-grammar-e (binop (mod)
                                                (nmbr 1)
                                                (nmbr 2)))
                (term (1 % 2)))

  (check-equal? (concrete-grammar-e (binop (lt)
                                                (nmbr 1)
                                                (nmbr 2)))
                (term (1 < 2)))

  (check-equal? (concrete-grammar-e (binop (le)
                                                (nmbr 1)
                                                (nmbr 2)))
                (term (1 <= 2)))

  (check-equal? (concrete-grammar-e (binop (gt)
                                                (nmbr 1)
                                                (nmbr 2)))
                (term (1 > 2)))

  (check-equal? (concrete-grammar-e (binop (ge)
                                                (nmbr 1)
                                                (nmbr 2)))
                (term (1 >= 2)))

  (check-equal? (concrete-grammar-e (binop (eq)
                                                (nmbr 1)
                                                (nmbr 2)))
                (term (1 == 2)))

  (check-equal? (concrete-grammar-e (binop (\\and)
                                                (nmbr 1)
                                                (nmbr 2)))
                (term (1 and 2)))

  (check-equal? (concrete-grammar-e (binop (\\or)
                                                (nmbr 1)
                                                (nmbr 2)))
                (term (1 or 2)))

  (check-equal? (concrete-grammar-e (binop (str-concat)
                                                (nmbr "1")
                                                (nmbr "2")))
                (term ("1" .. "2")))

  (check-equal? (concrete-grammar-e (unop (unm)
                                               (nmbr 1)))
                (term (- 1)))

  (check-equal? (concrete-grammar-e (unop (\\not)
                                               (true)))
                (term (not true)))

  (check-equal? (concrete-grammar-e (unop (len)
                                               (nmbr "asd")))
                (term (\# "asd")))

  (check-equal? (concrete-grammar-e (tuple (exps (list (nmbr 1) (nmbr 2)))))
          (term (< 1 2 >)))

  (check-equal? (concrete-grammar-e (tableconstructor
                               (fields (list
                                             (kv-table-field
                                              (nmbr 1)
                                              (nmbr 2))
                                             (v-table-field (nmbr 3))))))
          (term (\{ (\[ 1 \] = 2) 3 \})))

  (check-equal? (concrete-grammar-e (func-def (id-name 'x)
                                             (params (exps (list (id-name 'y))))
                                             (skip)))
          (term (function x (y) \; end)))

  )

(provide phrases-constructors-test-suite)