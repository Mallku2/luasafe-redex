#lang racket
(require redex
         "../luasafe.rkt"
         "../desugar/parser.rkt"
         "../typing.rkt"
         "../typing_lang_theory.rkt"
         "../reaching_defs.rkt")

(define (luasafe-test-suite)
  ;;;;;;;;;;;;;;;;;;;;;;;;
  ; syntactic forms recognized by LuaSafe                                                    
  ;;;;;;;;;;;;;;;;;;;;;;;;
  
  ; exps
  ; values
  (luasafe "local var = 1")
  (luasafe "local var = true")
  (luasafe "local var = \"asd\"")
  (luasafe "local var = nil")
  ; non-vararg functions, with multiple parameters; only one returned value
  (luasafe "local var = function () end")
  (luasafe "local var = function (x,y,z) return x end")
  ; tables with primitive types as keys, any type as value
  (luasafe "local var = {[1] = function () end, [true] = {}, [\"asd\"] = {}}")
  
  ; prefixexp
  ; table indexing
  (luasafe "local var = {[1] = function () end}
            return var[1]")
  ; function call: not fully supported yet (must improve CFG first); though
  ; type inf. and checking support them
    (luasafe "local f = function (x) return x + 1 end
              local var = f(1)")
  ; parenthesized exp., though limited by the absence of mult. returned
  ; values from fun. calls
  (luasafe "local f = function (x) return function () end end
            (f()) ()")

  ; operators
  ; arithmetic
  (luasafe "local var = 1 + 1")
  (luasafe "local var = 1 - 1")
  (luasafe "local var = 1 * 1")
  (luasafe "local var = 1 / 1")
  (luasafe "local var = 1 ^ 1")
  (luasafe "local var = 1 % 1")
  (luasafe "local var = - 1")
  
  ; relational
  (luasafe "local var = 1 < 1")
  (luasafe "local var = 1 <= 1")
  (luasafe "local var = 1 > 1")
  (luasafe "local var = 1 >= 1")
  (luasafe "local var = 1 == 1")

  ; logical
  (luasafe "local var = false or 1")
  (luasafe "local var = true and 1")
  (luasafe "local var = not false")

  ; strings concat., length operator
  (luasafe "local var = \"a \" .. \"string\"")
  (luasafe "local var = #{}")
  (luasafe "local var = #\"asd\"")

  ; stats
  
  ; skip
  (luasafe ";")

  ; return: only one value
  (luasafe "return 1")

  ; do-end
  (luasafe "do ; end")

  ; conditional
  (luasafe "if true then ; else ; end")

  ; while loop
  (luasafe "while true do break end")
  
  ; local var. defs.: only one variable
  (luasafe "local var = 1")

  ; assignment
  (luasafe "local var = 1
            var = var + 1")

  (luasafe "local t = {}
            t.method = function () end")
  
  ; fun. calls: not fully supported yet (must improve CFG first); though
  ; type inf. and checking support them
  (luasafe "local f = function () return 1 end
            f()")

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Basic capabilities of gc-safeness recognition:
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ; table with weak values but a strong ref
  (luasafe "local t1 = {[1] = {}}
               local x = t1[1]
               setmetatable(t1, {__mode = \"v\"})
               local i = 0
               while true do
  	       i = i + 1
  	       local garbage = {}
  	       if not t1 [1] then break end
               end
               return i")
  
  ; table with weak values, but non cte values
  (luasafe "local t1 = {[1] = 1}
               setmetatable(t1, {__mode = \"v\"})
               local i = 0
               while true do
  	       i = i + 1
  	       local garbage = {}
  	       if not t1 [1] then break end
               end
               return i")
  
  ; non-weak table
  (luasafe "local t1 = {[1] = {}}
               local i = 0
               while true do
  	       i = i + 1
  	       local garbage = {}
  	       if not t1 [1] then break end
               end
               return i")
  
  ; table with weak keys, but non cte
  (luasafe "local t1 = {[1] = {}}
               setmetatable(t1, {__mode = \"k\"})
               local i = 0
               while true do
  	       i = i + 1
  	       local garbage = {}
  	       if not t1 [1] then break end
               end
               return i")
  
  ; From paper:
  ; cache
  (luasafe "local cache1 = {[1] = function() return 1 end,
                            [2] = function() return 2 end,
                            [3] = function() return 3 end}
            local obj = {method = cache1[1], attr = {}}
            local cache2 = {[1] = cache1[2]}
            setmetatable(cache1, { __mode = \"v\"})
            setmetatable(cache2, { __mode = \"v\"})
            cache1[1]()
            cache1[2]()
            cache1[3]()")

  ; evolution of table
  (luasafe "local t1 = {}
            t1[\"attr1\"] = 1
            t1[\"method\"] = function(x) return x + t1[\"attr1\"] end
            t1[\"attr2\"] = (t1[\"method\"] (t1[\"attr1\"]))

            setmetatable(t1, {__mode = \"v\"})

            t1[\"method\"](t1[\"attr2\"])")

  ; table with weak values, no strong ref
  (luasafe "local t1 = {[1] = {}}
               setmetatable(t1, {__mode = \"v\"})
               local i = 0
               while true do
 	       i = i + 1
  	       local garbage = {}
  	       if not t1 [1] then break end
               end
               return i")

  )
