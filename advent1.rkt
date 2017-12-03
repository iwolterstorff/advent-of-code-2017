#lang htdp/isl+

(require 2htdp/batch-io)

(define INPUT (read-file "advent1-in.txt"))






(define (inverse-captcha input)
  (local ((define list-input (explode input))
          (define absolute-first (string->number (first list-input)))
          ;ACCUMULATOR: Sum so far
          (define (sum in acc)
            (local ((define first-num (string->number (first in))))
              (cond
                [(= (length in) 1)
                 (cond
                   [(= first-num absolute-first) (+ acc first-num)]
                   [else acc])]
                [(= first-num (string->number (second in))) (sum (rest in) (+ acc first-num))]
                [else (sum (rest in) acc)]))))

    (sum list-input 0)))


(define (inverse-captcha2 input)
  (local ((define list-input (explode input))
          (define absolute-first (string->number (first list-input)))
          ;ACCUMULATOR: Sum so far
          (define (sum in acc)
            (local ((define first-num (string->number (first in))))
              (cond
                [(= (length in) 1)
                 (cond
                   [(= first-num absolute-first) (+ acc first-num)]
                   [else acc])]
                [(= first-num (string->number (second in))) (sum (rest in) (+ acc first-num))]
                [else (sum (rest in) acc)]))))

    (sum list-input 0)))  