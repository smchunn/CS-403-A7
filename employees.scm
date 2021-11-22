(define (perform . args)
    (cond
        ((c_2_args args)
        (p_2_args args))

        ((c_4_args args)
        (p_4_args args))

        ((c_invalid_args args)
        (p_invalid_args))
    )
    (display "\n")
)

(define (c_2_args args)
    (cond
        ((= (length args) (length '(0 1))) #t)
        (else #f)
    )
)

(define (c_4_args args)
    (cond
        ((= (length args) (length '(0 1 2 3))) #t)
        (else #f)
    )
)

(define (c_invalid_args args)
    (cond
        ((c_2_args args) #f)
        ((c_4_args args) #f)
        (else #t)
    )
)

(define (p_invalid_args)
    (display "invalid params need to implement")
)

(define (p_2_args args)
    (p_4_args (append args (list '"ge" 0)))
)

(define (p_4_args args)
    (cond
        ((equal? (car (cdr args)) "count")
        (p_count (read_file (car args)) (cdr (cdr args))))

        ((equal? (car (cdr args)) "print")
        (p_print (read_file (car args)) (cdr (cdr args))))
        
        ((equal? (car (cdr args)) "min")
        (p_min (read_file (car args)) (cdr (cdr args))))
        
        ((equal? (car (cdr args)) "max")
        (p_max (read_file (car args)) (cdr (cdr args))))
        
        ((equal? (car (cdr args)) "total")
        (p_total (read_file (car args)) (cdr (cdr args))))
        
        ((equal? (car (cdr args)) "avg")
        (p_avg (read_file (car args)) (cdr (cdr args))))
    )
)

(define (read_file file_name)
    (with-input-from-file file_name
        (lambda ()
            (let loop ((lines '())
                (next-line (read-line)))
                (if (eof-object? next-line)
                    (reverse lines)
                    (loop (cons (string-split next-line) lines)
                        (read-line)
                    )
                )
            )
        )
    )
)

(define (tokenize l)
  (let loop ((t '())
             (l l))
    (if (pair? l)
        (let ((c (car l)))
          (if (char=? c #\space)
              (cons (reverse t) (loop '() (cdr l)))
              (loop (cons (car l) t) (cdr l))))
        (if (null? t)
            '()
            (list (reverse t))))))

(define (string-split s)
  (map list->string (tokenize (string->list s)))
)

(define (p_count emps args)
    (display "There are " (count_helper emps args) " employees\n")
)

(define (count_helper emps args)
    (if (and (not (null? emps)) (not (null? (car emps))))
        (begin
            (if (op (cons (earnings (car emps)) args))
                (+ (count_helper (cdr emps) args) 1)
                (+ (count_helper (cdr emps) args) 0)
            )
        )
        0
    )
)

(define (p_print emps args)
    (print_helper emps args)
)

(define (print_emp emp)
    (display (car (cdr emp)))
    (display " ")
    (display (car (cdr (cdr emp))))
    (display "\n")
    (display (earnings emp))
    (display "\n")
)

(define (print_helper emps args)
    (if (and (not (null? emps)) (not (null? (car emps))))
        (begin
            (if (op (cons (earnings (car emps)) args))
                (begin
                    (print_emp (car emps))
                )
            )
            (print_helper (cdr emps) args)
        )
    )
)

(define (p_min emps args)
    (print_emp (min_helper emps args 1000000))
)

(define (min_helper emps args min)
    (if (and (not (null? emps)) (not (null? (car emps))))
        (begin
            (if (and (< (earnings (car emps)) min) (op (cons (earnings (car emps)) args)))
                (min_helper (cdr emps) args (earnings (car emps)))
                (min_helper (cdr emps) args min)
            )
        )
        min
    )
)

(define (p_max emps args)
    (print_emp (max_helper emps args -1 (car emps)))
)

(define (max_helper emps args max emp)
    (if (and (not (null? emps)) (not (null? (car emps))))
        (begin
            (if (and (> (earnings (car emps)) max) (op (cons (earnings (car emps)) args)))
                (max_helper (cdr emps) args (earnings (car emps)) (car emps))
                (max_helper (cdr emps) args max emp)
            )
        )
        emp
    )
)

(define (p_total emps args)
    (display "Total: ")
    (display (tot_helper emps args 0))
    (display "\n")
)

(define (tot_helper emps args totval)
    (if (and (not (null? emps)) (not (null? (car emps))))
        (begin
            (if (op (cons (earnings (car emps)) args))
                (tot_helper (cdr emps) args (+ totval (earnings (car emps))))
            )
        )
        totval
    )
)

(define (p_avg emps args)
    (display "AVG: ")
    (display (/ (tot_helper emps args 0) (count_helper emps args)))
    (display "\n")
)

(define (earnings empl)
    (cond
        ((equal? (car empl) "salaried")
        (salaried-earnings (cdr empl)))
        ((equal? (car empl) "commission")
        (commission-earnings (cdr empl)))
        ((equal? (car empl) "hourly")
        (hourly-earnings (cdr empl)))
    )
)

(define (salaried-earnings empl)
    (string->number (car (cdr (cdr empl))))
)

(define (commission-earnings empl)
    (if (> (* (string->number (car (cdr (cdr (cdr empl))))) (string->number (car (cdr (cdr (cdr (cdr empl))))))) (string->number (car (cdr (cdr empl)))))
        (* (string->number (car (cdr (cdr (cdr empl))))) (string->number (car (cdr (cdr (cdr (cdr empl)))))))
        (string->number (car (cdr (cdr empl))))
    )
)

(define (hourly-earnings empl)
    (if (<= (string->number (car (cdr (cdr empl)))) 40)
        (* (string->number (car (cdr (cdr empl)))) (string->number (car (cdr (cdr (cdr empl))))))
        (if(<= (string->number (car (cdr (cdr empl)))) 50)
            (+ (* (string->number (car (cdr (cdr (cdr empl))))) 40) (* (- (string->number (car (cdr (cdr empl)))) 40) (string->number (car (cdr (cdr (cdr empl))))) 1.5))
            (+ (* (string->number (car (cdr (cdr (cdr empl))))) 40) (* 10 (string->number (car (cdr (cdr (cdr empl))))) 1.5) (* (- (string->number (car (cdr (cdr empl)))) 50) (string->number (car (cdr (cdr (cdr empl))))) 2))
        )
    )
)

(define (op args)
    (let((anOp (car (cdr args))) (aThreshold (car (cdr (cdr args)))) (anAmt (car args)))
        (cond
            ((equal? anOp "eq")
                (if (= anAmt aThreshold)
                    (= 1 1)
                    (= 1 0)
                )
            )
            
            ((equal? anOp "ne")
                (if (!= anAmt aThreshold)
                    (= 1 1)
                    (= 1 0)
                )
            )
            
            ((equal? anOp "ge")
                (if (>= anAmt aThreshold)
                    (= 1 1)
                    (= 1 0)
                )
            )
            
            ((equal? anOp "le")
                (if (<= anAmt aThreshold)
                    (= 1 1)
                    (= 1 0)
                )
            )
            
            ((equal?anOp  "gt")
                (if (> anAmt aThreshold)
                    (= 1 1)
                    (= 1 0)
                )
            )
            
            ((equal?anOp  "lt")
                (if (< anAmt aThreshold)
                    (= 1 1)
                    (= 1 0)
                )
            )
        )
    )
)
