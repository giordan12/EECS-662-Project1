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

(define-type CFAE
	(cnum (n number?))
	(cid (name symbol?))
	(cadd (lhs CFAE?) (rhs CFAE?))
	(csub (lhs CFAE?) (rhs CFAE?))
	(cmult (lhs CFAE?) (rhs CFAE?))
	(cdiv (lhs CFAE?) (rhs CFAE?))
	(capp (fun CFAE?) (arg CFAE?))
	(cif0 (det CFAE?) (true CFAE?) (false CFAE?))
)