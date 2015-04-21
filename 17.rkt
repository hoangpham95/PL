#lang pl 1eros=ones : String -> Boolean)
;; Identifies strings of n 0s followed by n 1s
(define zeros=ones
  (pushdown 0s end
    [0s  : ((0) ()  -> 0s  (A))
           (()  ()  -> 1s  ())]
    [1s  : ((1) (A) -> 1s  ())
           ((*) (*) -> end (*))]
    [end : (()  (*) -> end ())]))

;; tests:
(test (zeros=ones ""))
(test (zeros=ones "01"))
(test (zeros=ones "000111"))
(test (not (zeros=ones "0")))
(test (not (zeros=ones "11")))
(test (not (zeros=ones "10")))
(test (not (zeros=ones "00011")))
(test (not (zeros=ones "00101111")))

(define minutes-spent 90)
