; Jarred Price
; 27 September 2016

; for my use
; to load:
; scheme48 -h 8000000 then (load "basics.scm")
; or: ,load "basics.scm"

; zipcodes.scm contains all the US zipcodes.
; You should not modify this file. Your code
; should work for other instances of this file.
(load "zipcodes.scm")

; Helper function
(define (mydisplay value)
	(display value)
	(newline)
	#t
)

(define (ismember atm lst)
	(cond
	((null? lst) #f)
	((equal? atm (car lst)) #t)
	(else (ismember atm (cdr lst)))
      )
)

(define (square number)

	(* number number)

) ;end square definition


(define (discriminant a b c)

	(sqrt (- (square b)(* 4.0 a c) ))


);end discrinimant defintion


; Returns the roots of the quadratic formula, given
; ax^2+bx+c=0. Return only real roots. The list will
; have 0, 1, or two roots
(define (quadratic a b c)
	 (list(/ (+ (- b) (discriminant a b c)) (* 2 a))
      (/ (- (- b) (discriminant a b c)) (* 2 a))
 )

)

(mydisplay (quadratic 1 0 0))
(mydisplay (quadratic 0 1 0))
(mydisplay (quadratic 3 4 2))

; Return a list with only the negatives items
(define (negatives lst)
	
	(cond
	; Return empty list if null
	((null? lst) '())
                
	; If the first element of the list is negative
        ; add it to the list and then feed the rest of the list back in.
	((negative? (car lst)) (cons (car lst) (negatives (cdr lst))))
	
        ; If the first element of the list is not negative
	; remove the element and feed the rest of the list back 
	; in.
	(else (negatives (cdr lst)))

 
	)
) ;end negatives definition


(mydisplay (negatives '(-1 1 2 3 4 -4 5)))



; Definition for removing all of the atoms of the list:
(define (deleteAtoms lst)
   (cond
    ; Return empty list if null
   ((null? lst) '())

   ;( (atom? (car lst))) (deleteAtoms (cdr lst))
   ((not (pair? (car lst))) (deleteAtoms (cdr lst)))
   (else(cons (deleteAtoms (car lst)) (deleteAtoms (cdr lst))))
     
   )

)

; Returns true if the two lists have identical structure.
; (struct '(a b c (c a b)) '(1 2 3 (a b c))) -> #t
; (struct '(a b c d (c a b)) '(1 2 3 (a b c))) -> #f
; (struct '(a b c (c a b)) '(1 2 3 (a b c) 0)) -> #f
(define (struct lst1 lst2)
	
	; check to see if lists are equal
	(equal? (deleteAtoms lst1) (deleteAtoms lst2))

); end struct definition

(mydisplay (struct '(a b c (c a b)) '(1 2 3 (a b c))))
(mydisplay (struct '(a b c d (c a b)) '(1 2 3 (a b c))))
(mydisplay (struct '(a b c (c a b)) '(1 2 3 (a b c) 0)))

; Returns a list of two numeric values. The first is the smallest
; in the list and the second is the largest in the list. 
; lst -- flat, contains numeric values, and length is >= 1.
(define (minAndMax lst)
	(list(minimum lst) (maximum lst))
)


; helper #1 for minAndMax
(define (minimum lst)
	(if (= (length lst) 1) (car lst) (min(car lst) (minimum (cdr lst))))
	

)

; helper #2 for minAndMax
(define (maximum lst) 
	(if (= (length lst) 1) (car lst) (max(car lst) (maximum (cdr lst))))
)


(mydisplay (minAndMax '(1 2 -3 4 2)))
(mydisplay (minAndMax '(1)))

; Returns a list identical to the first, except all nested lists
; are removed:
; (flatten '(a b c)) -> (a b c)
; (flatten '(a (a a) a) -> (a a a a)
; (flatten '((a b) (c (d) e) f) -> (a b c d e f)
;
(define (flatten lst)
     (cond 
     ((null? lst) '())
     ;((list? lst) (cons (flatten (car lst))(flatten (cdr lst))))

     ; can't use cons here since that will create more
     ; nested lists. 
     ((list? lst) (append (flatten (car lst))(flatten (cdr lst))))

     ; if it is not a pair, then just return the list
      (else (list lst))
    )
); end flatten definition

(mydisplay (flatten '(a b c)))
(mydisplay (flatten '(a (a a) a)))
(mydisplay (flatten '((a b) (c (d) e) f)))

; The paramters are two lists. The result should contain the cross product
; between the two lists: 
; The inputs '(1 2) and '(a b c) should return a single list:
; ((1 a) (1 b) (1 c) (2 a) (2 b) (2 c))
; lst1 & lst2 -- two flat lists.
(define (crossproduct lst1 lst2)

	(cond
	((null? lst1) '())
	((null? lst2) '())
	( (cons (list (car lst1) (car lst2))
		  ;NO;;(list (crossproduct (list (car lst1)) (cdr lst2))
		  (append (crossproduct (list (car lst1)) (cdr lst2))
			  (crossproduct (cdr lst1) lst2)))))
);end crossproduct definintion.

(mydisplay (crossproduct '(1 2) '(a b c)))

; Returns all the latitude and longitude of particular zip code.
; Returns the first lat/lon, if multiple entries have same zip code.
; zipcode -- 5 digit integer
; zips -- the zipcode DB
(define (getLatLon zipcode zips)
       (cond 
       ((= zipcode (caar zips)) (cddr (cddar zips)))
       (else (getLatLon zipcode (cdr zips)))
       )     
)

(mydisplay (getLatLon 45056 zipcodes))

; Returns a list of all the place names common to two states.
; placeName -- is the text corresponding to the name of the place
; zips -- the zipcode DB
(define (getCommonPlaces state1 state2 zips)
    ;(intersect '(a b d) '(b g c))
	;(intersect (( extractDuplicates(getPlaceList state1 zipcodes))) (( extractDuplicates(getPlaceList state2 zipcodes))))

	(intersect (extractDuplicates(getPlaceList state1 zipcodes)) (extractDuplicates(getPlaceList state2 zipcodes)))
)

;ZIP PLACE STATE COUNTY LAT LON
;zip = caar zips
;place = caaar zips

; gets the list of all the places per state
(define (getPlaceList stateName zips)

	(cond
	((null? zips) '())
	((equal? stateName (caddar zips) ) ( cons(cadar zips)  (getPlaceList stateName (cdr zips))))
	(else ( getPlaceList stateName (cdr zips) ))
	)
);end getPlaceList


(define (intersect lst1 lst2)

	(cond
	((null? lst1) '())
	((null? lst2) '())
	((ismember (car lst1) lst2)
	(cons (car lst1)
	(intersect (cdr lst1) lst2)))
	(else (intersect (cdr lst1) lst2))
     )
       

) ;end intersection


(define (extractDuplicates duplst)
      (if (null? duplst)'()
      (if (ismember (car duplst) (cdr duplst))  
          (extractDuplicates (cdr duplst)) 
          (cons (car duplst)
          (extractDuplicates (cdr duplst))))
	)

) ; end extractDuplicates

(mydisplay (getCommonPlaces "OH" "MI" zipcodes))

; Returns a list of all the place names common to a set of states.
; states -- is list of state names
; zips -- the zipcode DB
(define (getCommonPlaces2 states zips)
	'("Oxford" "Franklin")
)

(mydisplay (getCommonPlaces2 '("OH" "MI" "PA") zipcodes))

; Returns the number of zipcode entries for a particular state.
; If a zipcode appears multiple times in zipcodes.scm, count one
; for each occurance.
; state -- state
; zips -- zipcode DB
(define (zipCount state zips)
	; Need to leave duplicates in this time, since we are counting it for each occurence.
	(length(getPlaceList state zipcodes))
	


);end definition of zipCount.


; Call this from zipCount to acquire the number of zipcode entries. 
(define (length2 lst)
	(cond
        ((null? lst) 0)
        ((list? lst) (+ 1 (length2 (cdr lst))))
        (else 0)
	)
)

(mydisplay (zipCount "OH" zipcodes))

; Returns the distance between two zip codes.
; Use lat/lon. Do some research to compute this.
; zip1 & zip2 -- the two zip codes in question.
; zips -- zipcode DB
(define (getDistanceBetweenZipCodes zip1 zip2 zips)
	0
)

(mydisplay (getDistanceBetweenZipCodes 45056 48122 zipcodes))

; Some sample predicates
(define (POS? x) (> x 0))
(define (NEG? x) (> x 0))
(define (LARGE? x) (>= (abs x) 10))
(define (SMALL? x) (NOT (LARGE? x)))

; Returns a list of items that satisfy a set of predicates.
; For example (filterList '(1 2 3 4 100) '(EVEN?)) should return the even numbers (2 4 100)
; (filterList '(1 2 3 4 100) '(EVEN? SMALL?)) should return (2 4)
; lst -- flat list of items
; filters -- list of predicates to apply to the individual elements
(define (filterList lst filters)
	lst
)

(mydisplay (filterList '(1 2 3 11 22 33 -1 -2 -3 -11 -22 -33) '(POS?)))
(mydisplay (filterList '(1 2 3 11 22 33 -1 -2 -3 -11 -22 -33) '(POS? EVEN?)))
(mydisplay (filterList '(1 2 3 11 22 33 -1 -2 -3 -11 -22 -33) '(POS? EVEN? LARGE?)))

; include the following line when on lnx01
;,exit