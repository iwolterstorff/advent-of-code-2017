#lang htdp/isl+

(require 2htdp/batch-io)

(define INPUT (read-file "advent1-in.txt"))

; A Digit is a numeric digit in String form

;;;;; TESTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(check-expect (inverse-captcha "1122") 3)
(check-expect (inverse-captcha "1111") 4)
(check-expect (inverse-captcha "1234") 0)
(check-expect (inverse-captcha "91212129") 9)

(check-expect (inverse-captcha2 "1212") 6)
(check-expect (inverse-captcha2 "1221") 0)
(check-expect (inverse-captcha2 "123425") 4)
(check-expect (inverse-captcha2 "123123") 12)
(check-expect (inverse-captcha2 "12131415") 4)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
          (define dist (/ (length list-input) 2))
          
          ; [List-of Digit] N N -> Digit
          (define (circ-list-ref l dist ind)
            (cond
              [(< (+ dist ind) (length l)) (list-ref l (+ dist ind))]
              [else (list-ref l (- (+ dist ind) (length l)))]))
          
          ;ACCUMULATOR: Sum so far
          (define (sum in acc index)
            (local ((define first-num (string->number (first in))))
              (cond
                [(= (length in) 1)
                 (cond
                   [(= first-num (string->number (circ-list-ref list-input dist index)))
                    (+ acc first-num)]
                   [else acc])]
                [(= first-num (string->number (circ-list-ref list-input dist index)))
                 (sum (rest in) (+ acc first-num) (add1 index))]
                [else (sum (rest in) acc (add1 index))]))))

    (sum list-input 0 0)))