#lang racket

; A Spreadsheet is a [List-of [List-of Number] ]

(define EX (list (list 5 1 9 5) (list 7 5 3) (list 2 4 6 8)))

(define INPUT (map
               (lambda (l) (map string->number l))
               (map string-split (file->lines "advent2-in.txt"))))

(define (ss-checksum ss)
  (local ((define (row-dif row)
            (- (apply max row) (apply min row))))
    (foldr + 0 (map row-dif ss))))