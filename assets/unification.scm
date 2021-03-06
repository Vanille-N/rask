(library (tspl unification)
  (export unify)
  (import (rnrs))

 ; occurs? returns true if and only if u occurs in v
  (define occurs?
    (lambda (u v)
      (and (pair? v)
           (let f ([l (cdr v)])
             (and (pair? l)
                  (or (eq? u (car l))
                      (occurs? u (car l))
                      (f (cdr l))))))))

 ; sigma returns a new substitution procedure extending s by
 ; the substitution of u with v
  (define sigma
    (lambda (u v s)
      (lambda (x)
        (let f ([x (s x)])
          (if (symbol? x)
              (if (eq? x u) v x)
              (cons (car x) (map f (cdr x))))))))

 ; try-subst tries to substitute u for v but may require a
 ; full unification if (s u) is not a variable, and it may
 ; fail if it sees that u occurs in v.
  (define try-subst
    (lambda (u v s ks kf)
      (let ([u (s u)])
        (if (not (symbol? u))
            (uni u v s ks kf)
            (let ([v (s v)])
              (cond
                [(eq? u v) (ks s)]
                [(occurs? u v) (kf "cycle")]
                [else (ks (sigma u v s))]))))))

 ; uni attempts to unify u and v with a continuation-passing
 ; style that returns a substitution to the success argument
 ; ks or an error message to the failure argument kf.  The
 ; substitution itself is represented by a procedure from
 ; variables to terms.
  (define uni
    (lambda (u v s ks kf)

      (cond
        [(symbol? u) (try-subst u v s ks kf)]
        [(symbol? v) (try-subst v u s ks kf)]
        [(and (eq? (car u) (car v))
              (= (length u) (length v)))
         (let f ([u (cdr u)] [v (cdr v)] [s s])
           (if (null? u)
               (ks s)
               (uni (car u)
                    (car v)
                    s
                    (lambda (s) (f (cdr u) (cdr v) s))
                    kf)))]
        [else (kf "clash")])))

 ; unify shows one possible interface to uni, where the initial
 ; substitution is the identity procedure, the initial success
 ; continuation returns the unified term, and the initial failure
 ; continuation returns the error message.
  (define unify
    (lambda (u v)
      (uni u
           v
           (lambda (x) x)
           (lambda (s) (s u))
           (lambda (msg) msg)))))
