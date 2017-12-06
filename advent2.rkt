#lang racket

; A Spreadsheet is a [List-of [List-of Number] ]

(define EX1 (list (list 5 1 9 5) (list 7 5 3) (list 2 4 6 8)))
(define EX2 (list (list 5 9 2 8) (list 9 4 7 3) (list 3 8 6 5)))

(define INPUT (map
               (lambda (l) (map string->number l))
               (map string-split (file->lines "advent2-in.txt"))))

(define (ss-checksum ss)
  (local ((define (row-dif row)
            (- (apply max row) (apply min row))))
    (foldr + 0 (map row-dif ss))))

;;; <rant> I am so mad. I took hours coding this function and I tried five different approaches
;;; because I DIDN'T NOTICE ITS TEST DATA WERE DIFFERENT!!!
;;; Anyways this might not be the cleanest approach I tried but I'm leaving it this way! </rant>
(define (ss-divsum ss)
  (local ((define (row-div init-row)
            (let row-div-helper ([row init-row])
              (local ((define (div-by-smth num l)
                        (cond
                          [(empty? l) #f]
                          [(exact-integer? (/ num (first l))) (/ num (first l))]
                          [else (div-by-smth num (rest l))])))
                (cond
                  [(empty? row) 0]
                  [(boolean? (div-by-smth (first row) (remove (first row) init-row)))
                   (row-div-helper (rest row))]
                  [else (div-by-smth (first row) (remove (first row) init-row))])))))
              
    (foldr + 0 (map row-div ss))))