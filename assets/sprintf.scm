(library (tspl formatted-output)
  (export printf fprintf)
  (import (rnrs))

 ; dofmt does all of the work.  It loops through the control string
 ; recognizing format directives and printing all other characters
 ; without interpretation.  A tilde at the end of a control string is
 ; treated as an ordinary character.  No checks are made for proper
 ; inputs.  Directives may be given in either lower or upper case.
  (define dofmt
    (lambda (p cntl args)
      (let ([nmax (- (string-length cntl) 1)])
        (let loop ([n 0] [a args])
          (if (<= n nmax)
              (let ([c (string-ref cntl n)])
                (if (and (char=? c #\~) (< n nmax))
                    (case (string-ref cntl (+ n 1))
                      [(#\a #\A)
                       (display (car a) p)
                       (loop (+ n 2) (cdr a))]
                      [(#\s #\S)
                       (write (car a) p)
                       (loop (+ n 2) (cdr a))]
                      [(#\%)
                       (newline p)
                       (loop (+ n 2) a)]
                      [(#\~)
                       (put-char p #\~) (loop (+ n 2) a)]
                      [else
                       (put-char p c) (loop (+ n 1) a)])
                    (begin
                      (put-char p c)
                      (loop (+ n 1) a)))))))))

 ; printf and fprintf differ only in that fprintf passes its
 ; port argument to dofmt while printf passes the current output
 ; port.
  (define printf
    (lambda (control . args)
      (dofmt (current-output-port) control args)))

  (define fprintf
    (lambda (p control . args)
      (dofmt p control args))))
