#lang racket
(require "static_sensor_model.scm")
;(require "time_delay_sensor_model.scm")
(require "dynamic_programming.scm")

(define N 2) ; number of measurements to take
(define n #e1e2) ; number of possible measurements to choose from: |U|
(define S (make-states (make-interval n)))
(define U (lambda (k x) ((make-controls (make-interval n)) k x)))
(define initial-state '(0.0 . 1.0))
;(define initial-state '((0.0 . 1.0) (0 . 0)))
(define branch (build-list N (lambda (x) 0))); outcome LLL...

(define optimal-policy-object
  (make-policy-object N S U W
		      (min-Q-factor U W)
		      (dp-step lookup)))

(define greedy-policy-object
  (make-policy-object N S U W 
		      (lambda (k x J)
			((Q-factor (dp-step lookup) W)
			 k x
			 ((policy U W J (dp-step (lambda (a b c) 0.0))) k x)
			 J))
		      (dp-step (lambda (a b c) 0.0))))

(define output-display
  (lambda (initial-state object branch)
    (let* ((k 0)
	  (x initial-state)
	  (u ((object 'get-policy) k x)))
      (display "policy for outcome ")
      (display branch)
      (newline)
      (display k)
      (display " ")
      (display x)
      (define loop
	(lambda (l)
	  (cond
	   ((null? l) "done")
	   (else
	    (display " ")
	    (display u)
	    (newline)
	    (set! x (f k x u (car l))) ; must update x before k due to k dependence!
	    (set! k (add1 k))
	    (display k)
	    (display " ") 
	    (display x)
	    (set! u ((object 'get-policy) k x))
	    (loop (cdr l))))))
      (loop branch)
      (newline)
      (display "final J value =")
      (display " ")
      (display (- ((object 'get-final-J) initial-state)))
      (newline))))

(define inorder-tree
  (lambda (k x policy N)
    (let ((u (policy k x))
	  (k1 (add1 k)))
      (cond
       ((or (> k1 N) (null? u)) '())
       (else
	(append
	 (inorder-tree k1 (f k x u 0) policy N)
	 (list (cons k u))
	 (inorder-tree k1 (f k x u 1) policy N)))))))

(define node-list
  (lambda (N initial-state object)
    (sort
     (inorder-tree 0 initial-state (object 'get-policy) N)
     (lambda (x y) (< (car x) (car y))))))

(define vert-display
  (lambda (l)
    (for-each
     (lambda (x)
       (display x)
       (newline))
     l)))

;;;; Output ;;;;
(newline)
(display "Optimal policy:")
(newline)
(output-display initial-state optimal-policy-object branch)
(newline)
(display "Greedy policy:")
(newline)
(output-display initial-state greedy-policy-object branch)
(newline)
(display "Bisections for optimal: ")
(newline)
(vert-display (node-list N initial-state optimal-policy-object))
(newline)
(display "Bisections for greedy: ")
(newline)
(vert-display (node-list N initial-state greedy-policy-object))
(newline)
