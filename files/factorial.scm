(define fact (lambda (x)
    (if (eq? x 1)
        1
        (* x (fact (sub x 1))))))
(fact 10)