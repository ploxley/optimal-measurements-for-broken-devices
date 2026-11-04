#lang racket
(require "static_sensor_model.scm")
;(require "time_delay_sensor_model.scm")
(require "binary_decision_tree.scm")
(require racket/format)

(define N 2) ; number of measurements to take
(define n #e1e3) ; number of possible measurements to choose from: |U|
(define U (lambda (k x) ((make-controls (make-interval n)) k x)))
(define initial-state '(0.0 . 1.0))
;(define initial-state '((0.0 . 1.0) (0 . 0)))
(define cdf (lambda (x) x)) ; cdf and inv-cdf for uniform
(define inv-cdf (lambda (p) p))
;(define cdf (lambda (x) (F-interval x initial-state))) ; cdf and inv-cdf for truncated Gaussian
;(define inv-cdf (lambda (p) (inv-F-interval p initial-state)))
 
(define greedy-policy-node-list
  (policy-node-list (greedy-policy U W) N initial-state))

(define nol (+ (length greedy-policy-node-list) 1))

(define bt (build-binary-tree greedy-policy-node-list))

(define sub-interval-probabilities
  (lambda (sub-interval-list cdf)
    (letrec ((loop
	      (lambda (l prev)
		(cond
		 ((null? l) '())
		 (else
		  (let ((curr (cdf (car l))))
		    (cons
		     (- curr prev)
		     (loop (cdr l) curr))))))))
      (loop sub-interval-list (cdf 0.0)))))

(define greedy-probabilities
  (sub-interval-probabilities
   (append (sort (map (lambda (l) (car l))
		      greedy-policy-node-list) <) '(1.0)) cdf))

(define sub-interval-entropy
  (lambda (probability-list)
    (foldl + 0.0
	   (map (lambda (p)
		  (* p (log2 (/ 1 p))))
		probability-list))))

(define total-path-length
  (lambda (binary-tree)
    (letrec ((epl
              (lambda (t depth)
		(cond
		 ((null? t) depth)
		 (else
                  (+ (epl (left-branch t) (add1 depth))
                     (epl (right-branch t) (add1 depth))))))))
      (epl binary-tree 0))))

(newline)
(display "Greedy binary decision tree:")
(newline)
bt
(display "Number of leaves: ")
(display nol)
(newline)
(display "Average path length: ")
(display (~r (/ (exact->inexact (total-path-length bt)) nol)
	     #:precision 4))
(newline)
(display "Number of measurements: ")
(display N)
(newline)
(display "Number of bits: ")
(display (~r (sub-interval-entropy greedy-probabilities)
	     #:precision 4))

(bottom-up-tree-relabel! bt nol inv-cdf)

(newline)
(newline)
(display "Optimal binary decision tree:")
(newline)
bt
(display "Number of leaves: ")
(display nol)
(newline)
(display "Average path length: ")
(display (~r (/ (exact->inexact (total-path-length bt)) nol)
	     #:precision 4))
(newline)
(display "Number of measurements: ")
(display N)
(newline)
(display "Number of bits: ")
(display (~r (log2 nol) #:precision 4))
