; 5) classify
(define (classify type my-list)
  (cond [(null? my-list) '(() ())]
        [(equal? type even?) (list (classify-is-even my-list) (classify-is-odd my-list))]
        [(equal? type real?) (list (classify-is-real my-list) (classify-is-not-real my-list))]
        [(equal? type integer?) (list (classify-is-integer my-list) (classify-is-not-integer my-list))]
        )
  )

; helper functions for classify
; classify-is-even: builds the list if the element is even
(define (classify-is-even my-list)
  (cond [(null? my-list) '()]
        [(even? (head my-list)) (cons (head my-list) (classify-is-even (tail my-list)))]
        [else (classify-is-even (tail my-list))]
        )
  )

; classify-is-odd: builds the list if the element is odd
(define (classify-is-odd my-list)
  (cond [(null? my-list) '()]
        [(odd? (head my-list)) (cons (head my-list) (classify-is-odd (tail my-list)))]
        [else (classify-is-odd (tail my-list))]
        )
  )

; classify-is-real: builds the list if the element is real
(define (classify-is-real my-list)
  (cond [(null? my-list) '()]
        [(real? (head my-list)) (cons (head my-list) (classify-is-real (tail my-list)))]
        [else (classify-is-real (tail my-list))]
        )
  )

; classify-is-not-real: builds the list if the element is not real
(define (classify-is-not-real my-list)
  (cond [(null? my-list) '()]
        [(real? (head my-list)) (classify-is-not-real (tail my-list))]
        [else (cons (head my-list) (classify-is-not-real (tail my-list)))]
        )
  )

; classify-is-integer: builds the list if the element is an integer
(define (classify-is-integer my-list)
  (cond [(null? my-list) '()]
        [(integer? (head my-list)) (cons (head my-list) (classify-is-integer (tail my-list)))]
        [else (classify-is-integer (tail my-list))]
        )
  )

; classify-is-not-integer: builds the list if the element is not an integer
(define (classify-is-not-integer my-list)
  (cond [(null? my-list) '()]
        [(integer? (head my-list)) (classify-is-not-integer (tail my-list))]
        [else (cons (head my-list) (classify-is-not-integer (tail my-list)))]
        )
  )