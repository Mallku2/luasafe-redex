# luasafe-redex

This is the Redex mechanization of LuaSafe, a prototype tool that helps to uncover potential misuses of
weak tables, in Lua 5.2 programs. For a formal introduction, see the paper "Understanding Lua’s Garbage 
Collection - Towards a Formalized Static Analyzer". For a quick introduction about the capabilities of
the tool, see module tests/luasafe_test_suite.rkt.

1. Installation of DrRacket

To install Racket's IDE DrRacket, simply download the installer that corresponds to your system from https://racket-lang.org/download/. The present version of the mechanization has been tested on several versions of DrRacket, up to 7.2.

2. Structure of the mechanization

* grammar.rkt: grammar of the language.
* type_inference.rkt: mechanization of our type inference algorithm
* typing.rkt: mechanization of our type checking algorithm
* data_flow_analysis.rkt: construction of the required CFG
* reaching_defs.rkt: implementation of the reaching definitions problem
* typing_lang_theory.rkt: language of types and type variables
* luasafe.rkt: simple procedures to ease the use of LuaSafe.
* tests/ : collection of test suites for each module 
