#lang racket
(require math/distributions)
(provide (all-defined-out))

;;;; Model provides W, f, p, g, make-states, and make-controls ;;;;

(define delay-time 10) 
(define W '(0 1)) ; set of measurement outcomes 

;; List of floating-point numbers uniform from 0.0 and 1.0
(define make-interval
  (lambda (n)
    (append
     (build-list n (lambda (x) (+ (/ x n) 0.0)))
     '(1.0))))

(define make-state
  (lambda (a b c d)
    (list (cons a b)
	  (cons c d))))

(define make-states
  (lambda (interval)
    (map (lambda (a)
	   (map (lambda (b)
		  (if (< a b)
		      (map (lambda (c)
			     (map (lambda (d)
				    (make-state a b c d))
				  (cons 0 (build-list delay-time add1))))
			   W)
		      '()))
		interval))
	 interval)))

(define left-state
  (lambda (state)
    (car (car state))))

(define right-state
  (lambda (state)
    (cdr (car state))))

(define prev-state
  (lambda (state)
    (car (cadr state))))

(define delay-state
  (lambda (state)
    (cdr (cadr state))))

(define dotted-pair?
  (lambda (x)
    (and (pair? x) (not (list? (cdr x))))))

(define atom?
  (lambda (x)
    (not (pair? x))))

(define state?
  (lambda (s)
    (if (null? s)
	#f
	(and (dotted-pair? (car s))
	     (dotted-pair? (cadr s))))))

(define make-controls
  (lambda (interval)
    (lambda (k x)
      (cond
       ((not (zero? (delay-state x))) (list (right-state x)))
       (else
	(filter
	 (lambda (y)
	   (and (>= y (left-state x))
		(< y (right-state x))))
	 interval))))))

(define f
  (lambda (k x u m)
    (let* ((current-delay (delay-state x))
	   (new-delay
	    (cond
	     ((zero? current-delay)
	      (cond
	       ;((= m 0) delay-time)
	       ((and (= (prev-state x) m) (> k 0)) delay-time)
	       (else 0)))
	     (else
	      (sub1 current-delay)))))
      (cond
       ((= m 0) (make-state (left-state x) u m new-delay))
       ((= m 1) (make-state u (right-state x) m new-delay))
       (else
	(error "m not a valid outcome"))))))

;;; cdf and inverse cdf for truncated Gaussian
(define phi
  (lambda (x)
    (flnormal-cdf 0.5 0.2 x #f #f)))

(define inv-phi
  (lambda (p)
    (flnormal-inv-cdf 0.5 0.2 p #f #f)))

(define Z
  (lambda (a b)
    (- (phi b) (phi a))))

(define F
  (lambda (x a b)
    (* (/ 1 (Z a b)) (- (phi x) (phi a)))))

(define inv-F
  (lambda (p a b)
    (inv-phi
     (+ (* (Z a b) p) (phi a)))))

(define p
  (lambda (m x u)
    (let ((p0 (/ (- u (left-state x)) (- (right-state x) (left-state x)))))
    ;(let ((p0 (F u (left-state x) (right-state x))))
      (cond ((= m 0) p0)
	    ((= m 1) (- 1.0 p0))
	    (else
	     (error "m not a valid outcome"))))))
	   
(define (log2 x)
  (/ (log x) (log 2)))

(define g
  (lambda (k x u m)
    (let ((pmf (p m x u)))
      (if (zero? pmf)
	  -0.0
	  (- (log2 (/ 1 pmf)))))))
