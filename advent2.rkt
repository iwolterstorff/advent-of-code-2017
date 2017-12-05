#lang racket

; A Spreadsheet is a [List-of [List-of Number] ]

(define EX (list (list 5 1 9 5) (list 7 5 3) (list 2 4 6 8)))

(define PORT-IN (open-input-file "advent2-in.txt"))
(define INPUT (for/list ([num (stop-before PORT-IN (lambda (in) (eq? in "\t")))])
                num))

#;(define INPUT (for/list ([num (stop-before "5048	177	5280	5058"
                                           char-whitespace?)])
                num))


(define (ss-checksum ss)
  (local ((define (row-dif row)
            (- (apply max row) (apply min row))))
    (foldr + 0 (map row-dif ss))))