#lang plai

(define-type CFWAE
  (num (n number?))
  (cid (name symbol?))
  (cadd (lhs CFWAE?) (rhs CFWAE?))
  (csub (lhs CFWAE?) (rhs CFWAE?))
  (cmult (lhs CFWAE?) (rhs CFWAE?))
  (cdiv (lhs CFWAE?) (rhs CFWAE?))
  (cfun (arg symbol?) (body CFWAE?))
  (capp (fun CFWAE?) (arg CFWAE?))
  (cif0 (det CFWAE?) (true CFWAE?) (false CFWAE?))
  (with (name symbol?) (named_expr CFWAE?) (body CFWAE?))
  ;;(cond0 ())
  )

(define-type CFAE
	(num (n number?))
	(id (name symbol?))
	(add (lhs CFAE?) (rhs CFAE?))
	(sub (lhs CFAE?) (rhs CFAE?))
	(mult (lhs CFAE?) (rhs CFAE?))
	(div (lhs CFAE?) (rhs CFAE?))
	(app (fun CFAE?) (arg CFAE?))
	(if0 (det CFAE?) (true CFAE?) (false CFAE?))
	(fun (arg symbol?) (body CFAE?))
)

(define elab-cfwae 
	(lambda (expr)
		(type-case CFWAE expr
			(cnum (n) (num n))
			(cid (name) (id name))
			(cadd (l r) (add l r))
			(csub (l r) (sub l r))
			(cmult (l r) (mult l r))
			(cdiv (l r) (div l r))
			(cfun (a b) (fun a b))
			(capp (f a) (app f a))
			(cif0 (d t f) (if0 d t f))
			(with (n ne b) (app (fun n b) ne))
		)
	)
)