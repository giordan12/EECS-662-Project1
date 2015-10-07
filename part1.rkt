#lang plai

;;Name: Giordanno Castro Garcia
;;KUID: 2738180
;;Project2 Part1




;;datatype according to what was specified on the webpage
(define-type CFAE
  (num (n number?))
  (id (name symbol?))
  (add (lhs CFAE?)(rhs CFAE?))
  (sub (lhs CFAE?)(rhs CFAE?))
  (mult (lhs CFAE?)(rhs CFAE?))
  (div (lhs CFAE?)(rhs CFAE?))
  (fun (arg symbol?)(body CFAE?))
  (app (fun CFAE?) (arg CFAE?))
  (if0 (det CFAE?) (true CFAE?) (false CFAE?))
  )

;;datatype that will associate the symbol to a value
(define-type Binding
  (bind (name symbol?) (val Value?)))

;;(define-type-alias Env (listof Binding))

;;datatype used to specify the possible return types after interpretation
(define-type Value
  (numV (n number?))
  (closV (arg symbol?) (body CFAE?) (env (listof Binding?))));;plai does not have define-type-alias, therefore use an alternative to this method


;;lookup method that will find the associated value for a symbol
(define lookup
  (lambda (for env)
    (cond
      ((empty? env) (error 'lookup "name not found"))
      (else (cond
              ((symbol=? for (bind-name (first env))) (bind-val (first env)))
              (else (lookup for (rest env)))
              ))      
      )
    ))

(define extend-env cons)
(define mt-env empty)

;;actual interpreter
;;every arithmetic operation uses a helper method in order to do type verification
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


      )
      ;;add the condition for if0 here.
      ;;put a cond and interp the condition
      ;;if once the condition is interp, it returns a numV with n = 0
      ;;interp the first one 
      ;;otherwise interp the second one
    )
  )
  
;;Helper methods for arithmetic operations
;;They do type checking on the operands

(define num+
  (lambda (l r)
    (cond
      ((and (numV? l) (numV? r)) (numV (+ (numV-n l) (numV-n r))))
      (else
       (error 'num+ "one argument was not a number"))
      )))

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