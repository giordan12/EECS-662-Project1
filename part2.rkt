#lang plai

(define-type CFWAE
  (num (n number?))
  (id (name symbol?))
  (add (lhs CFWAE?) (rhs CFWAE?))
  (mult (lhs CFWAE?) (rhs CFWAE?))
  (div (lhs CFWAE?) (rhs CFWAE?))
  (fun (arg symbol?) (body CFWAE?))
  (app (fun CFWAE?) (arg CFWAE?))
  (if0 (det CFWAE?) (true CFWAE?) (false CFWAE?))
  (with (name symbol?) (named_expr CFWAE?) (body CFWAE?))
  ;;(cond0 ())
  )
