#lang plai

(define-type CFWAE
  (cnum (n number?))
  (cid (name symbol?))
  (cadd (lhs CFWAE?) (rhs CFWAE?))
  (csub (lhs CFWAE?) (rhs CFWAE?))
  (cmult (lhs CFWAE?) (rhs CFWAE?))
  (cdiv (lhs CFWAE?) (rhs CFWAE?))
  (cfun (arg symbol?) (body CFWAE?))
  (capp (fun CFWAE?) (arg CFWAE?))
  (cif0 (det CFWAE?) (true CFWAE?) (false CFWAE?))
  (with (name symbol?) (named_expr CFWAE?) (body CFWAE?))
  (cond0 (conditions (listof Condition?)) (def CFWAE?))
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
			(cadd (l r) (add (elab-cfwae l) (elab-cfwae r)))
			(csub (l r) (sub (elab-cfwae l) (elab-cfwae r)))
			(cmult (l r) (mult (elab-cfwae l) (elab-cfwae r)))
			(cdiv (l r) (div (elab-cfwae l) (elab-cfwae r)))
			(cfun (a b) (fun a (elab-cfwae b)))
			(capp (f a) (app (elab-cfwae f) (elab-cfwae a)))
			(cif0 (d t f) (if0 (elab-cfwae d) (elab-cfwae t) (elab-cfwae f)))
			(with (n ne b) (app (fun n (elab-cfwae b)) (elab-cfwae ne)))
                  (cond0 (c def) (cond
                                   ((empty? (rest c)) (if0 (elab-cfwae (try-condi (first c))) (elab-cfwae (try-do (first c))) (elab-cfwae def)))
                                   (else (if0 (elab-cfwae (try-condi(first c))) (elab-cfwae (try-do(first c))) (elab-cfwae (cond0(rest c) def)))))
                                   )
		)
	)
)

(define-type Binding
  (bind (name symbol?) (val Value?)))

(define-type Value
  (numV (n number?))
  (closV (arg symbol?) (body CFAE?) (env (listof Binding?)))
  )

(define-type Condition
  (try (condi CFWAE?) (do CFWAE?)))

(define-type ListCondition
  (lis (l (listof Condition?))))

(define lookup
  (lambda (for env)
    (cond
      ((empty? env) (error 'lookup "name not found"))
      (else (cond
              ((symbol=? for (bind-name (first env))) (bind-val (first env)))
              (else (lookup for (rest env)))
              ))
      )
    )
  )

(define extend-env cons)
(define mt-env empty)

(define interp-cfae
	(lambda (expr ds)
		(type-case CFAE expr
			(num (n) (numV n))
			(id (n) (lookup n ds))
			(add (l r) (num+ (interp-cfae l ds) (interp-cfae r ds)))
			(sub (l r) (sub+ (interp-cfae l ds) (interp-cfae r ds)))
			(mult (l r) (mult+ (interp-cfae l ds) (interp-cfae r ds)))
			(div (l r) (div+ (interp-cfae l ds) (interp-cfae r ds)))
			(fun (a b) (closV a b ds))
			 (app (f a) (local ([define f-value (interp-cfae f ds)])
							(interp-cfae (closV-body f-value)
								(extend-env (bind (closV-arg f-value)
                                                                                  (interp-cfae a ds))
											(closV-env f-value)))
			))
			(if0 (d t f) (cond
							((and (numV? (interp-cfae d ds)) (equal? (interp-cfae d ds) (numV 0))) (interp-cfae t ds))
							(else (interp-cfae f ds))	
			))
		)
	)
)

(define num+
  (lambda (l r)
    (cond
      ((and (numV? l) (numV? r)) (numV (+ (numV-n l) (numV-n r))))
      (else (error 'num+ "one argument is not a number"))
      )
    ))

(define sub+
  (lambda (l r)
    (cond
      ((and (numV? l) (numV? r)) (numV (- (numV-n l) (numV-n r))))
      (else
       (error 'sub+ "one argument was not a number"))
      )))

(define mult+
  (lambda (l r)
    (cond
      ((and (numV? l) (numV? r)) (numV (* (numV-n l) (numV-n r))))
      (else 'mult+ "one argument was not a number")
      )
    ))

(define div+
  (lambda (l r)
    (cond
      ((and (numV? l) (numV? r)) (numV (/ (numV-n l) (numV-n r))))
      (else 'div+ "one argument was not a number")
      )
    ))

(define eval-cfwae
  (lambda (expr)
    (interp-cfae (elab-cfwae expr) mt-env)
    )
  )






