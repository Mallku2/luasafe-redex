# luasafe-redex

This is the Redex mechanization of LuaSafe, a prototype tool that helps to uncover potential misuses of weak tables, in Lua 5.2 programs. For a formal introduction, see the paper *"Understanding Lua’s Garbage Collection - Towards a Formalized Static Analyzer"*. For a quick introduction about the capabilities of the tool, see module **tests/luasafe_test_suite.rkt**.

1. **Introduction**

For a given program **P**, LuaSafe performs type inference, data-flow analysis and type checking, to determine if **P** belongs to the set of gc-safe programs, i.e., it determines if **P** does not exhibit non-deterministic behavior as a result of interactions with the garbage collector through the interfaces to it provided by Lua 5.2. The present version of LuaSafe is aimed at reasoning about weak tables. 

2. **Installation of DrRacket**

To install Racket's IDE DrRacket, simply download the installer that corresponds to your system from https://racket-lang.org/download/. The present version of the mechanization has been tested on several versions of DrRacket, up to 7.2.

3. **Structure of the mechanization**

* **grammar.rkt**: grammar of the language.
* **type_inference.rkt**: mechanization of our type inference algorithm
* **typing.rkt**: mechanization of our type checking algorithm
* **data_flow_analysis.rkt**: construction of the required CFG
* **reaching_defs.rkt**: implementation of the reaching definitions problem
* **typing_lang_theory.rkt**: language of types and type variables
* **luasafe.rkt**: simple procedures to ease the use of LuaSafe.
* **tests/** : collection of test suites for each module 

4. **Basic usage**

Module **luasafe.rkt** provides a procedure, luasafe, that eases the use of the tool. It receives a string, containing a Lua program (defined with the set of constructions described in **tests/luasafe_test_suite.rkt**). The procedure will inform if it encounters any operation that may present a non-deterministic behavior, by printing the involved expression. Note that the present mechanization is intended for analysis of small snippets of code. 
Example:
```racket
(luasafe "local t1 = {[1] = {}}
          setmetatable(t1, {__mode = \"v\"})
          local x = t1 [1]")
"Access to: "
'(t1 |[| 1.0 |]|)
"may exhibit non-deterministic behavior"
#t
```

5. **Guarantess provided by LuaSafe**

The actual mechanization is provided as a prototype tool, intended to test ideas about static analysis over Lua programs for reasoning about gc-safeness. The present version lacks formal guarantees of correctness. Also, it is provided as a mechanization in PLT Redex, which, while serving the purpose of a specification document (as there are few implementation-specific details to read about), lacks the performance of a tool implemented with a regular PL.
