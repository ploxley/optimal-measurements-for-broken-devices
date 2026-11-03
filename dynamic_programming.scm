#lang racket
(require "static_sensor_model.scm")
;(require "time_delay_sensor_model.scm")
(provide (all-defined-out))

;;;; General purpose dynamic programming code and helpers ;;;;

;;;; DP algorithm ;;;; 
;; time period: k in set N
;; state: x in set S
;; control: u in set U(k,x)
;; disturbance outcome: w in set W
;; disturbance pmf:  p(w|x,u)
;; cost per-stage: g(k,x,u,w),
;; dynamics: f(k,x,u,w)

(define make-table make-hash)

(define lookup               
  (lambda (J k x)
    (hash-ref J (cons k x))))

(define lookup?
  (lambda (J k x)
    (hash-has-key? J (cons k x))))

(define update!
  (lambda (J k x cost)
    (hash-set! J (cons k x) cost)))

(define min-list
  (lambda (l)
    (foldl min (car l) l)))

(define sum-list
  (lambda (l)
    (foldl + 0.0 l)))

(define argmin
  (lambda (lp)
    (cond
     ((null? lp) '())
     (else
      (car
       (foldl
	(lambda (p1 p2)
	  (if (< (cdr p1) (cdr p2))
	      p1
	      p2))
	(car lp)
	lp))))))

(define seq-apply*
  (lambda (f list test)
    (letrec ((SA*
	      (lambda (l)
		(cond
		 ((null? l) #t)
		 ((test (car l))
		  (f (car l))
		  (SA* (cdr l)))
		 (else
		  (SA* (car l))
		  (SA* (cdr l)))))))
      (SA* list))))

(define dp-step
  (lambda (term-to-add)
    (lambda (k x u w J)
      (let ((k1 (add1 k))
	    (x1 (f k x u w)))
	(if (lookup? J k1 x1)
	    (+ (g k x u w) (term-to-add J k1 x1))
	    (expt 2 63)))))) ; penalty if state not in table!

(define Q-factor
  (lambda (dp-step W)
    (lambda (k x u J)
      (sum-list
       (map (lambda (w)
	      (* (p w x u)
		 (dp-step k x u w J)))
	    W)))))

(define min-Q-factor
  (lambda (U W)
    (lambda (k x J)
      (let ((u-list (U k x)))
	(min-list
	 (map (lambda (u)
		((Q-factor (dp-step lookup) W) k x u J))
	      u-list))))))

(define policy
  (lambda (U W J dp-step)
    (lambda (k x)
      (let ((u-list (U k x)))
	(argmin
	 (map (lambda (u)
		(cons u
		      ((Q-factor dp-step W) k x u J)))
	      u-list))))))

(define make-policy-object
  (lambda (N S U W update-step dp-step)
    (let ((J (make-table))) ; local state J
      (seq-apply* (lambda (x) (update! J N x 0)) S state?) ; initialize row N of J to 0s
      (letrec ((dp-iteration
		(lambda (k)
		  (seq-apply* (lambda (x)
				(update! J k x (update-step k x J)))
			      S
			      state?)
		  (cond
		   ((zero? k) #t)
		   (else
		    (dp-iteration (sub1 k)))))))
	(dp-iteration (sub1 N)))
      (define final-J-value
	(lambda (initial-state)
	  (lookup J 0 initial-state)))
      (define policy-value
	(lambda (k x)
	  ((policy U W J dp-step) k x)))
      (define dispatch
	(lambda (m)
	  (cond
	   ((eq? m 'get-final-J) final-J-value)
	   ((eq? m 'get-policy) policy-value)
	   (else
	    (error "Unknown operation" m)))))
      dispatch)))
