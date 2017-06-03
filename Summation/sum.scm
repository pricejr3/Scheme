(define (sum TARGET TEST)
    (mapping TARGET 
    (map addition (listCombinations TEST)
    )
    )
)


(define (addition lst)
  (if (null? lst) 0
    (+ (CAR lst) (addition (CDR lst)))
  )
)

(define (listCombinations set)
 (if (null? set) '(())
 (let 
 ((value 
 (listCombinations (cdr set))) )
 (append value
 (map (lambda (list) 
 (cons (car set) list))
     value)
       )
     )
   )
 )

(define (mapping TARGET TEST)
    (if (NULL? TEST)
        #f
        (if (= (car TEST) TARGET)
            #t
            (mapping TARGET (cdr TEST))
	)
	)
)

