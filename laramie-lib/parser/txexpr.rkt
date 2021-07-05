#lang typed/racket/base

(unsafe-require/typed
 txexpr
 [txexpr (-> Symbol
             (Listof (List Symbol String))
             (Listof XExpr)
             XExpr)]
 [txexpr* (-> Symbol
              (Listof (List Symbol String))
              XExpr *
              XExpr)])

(provide txexpr
         txexpr*)
