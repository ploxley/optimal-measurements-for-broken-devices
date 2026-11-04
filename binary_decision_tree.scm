#lang racket
(require compatibility/mlist)
(require "static_sensor_model.scm")     
;(require "time_delay_sensor_model.scm")
(require "dynamic_programming.scm") ; helpers only
(provide (all-defined-out))

;;; Mutable ordered binary tree for multi-element nodes ;;;

(define make-node
  (lambda (a b c)
    (list a b c)))

(define key
  (lambda (x)
    (car x)))

(define store-left
  (lambda (x)
    (cadr x)))

(define store-right
  (lambda (x)
    (caddr x)))

(define make-tree
  (lambda (entry left right)
    (mlist entry left right)))

(define entry
  (lambda (tree)
    (mcar tree)))

(define left-branch
  (lambda (tree)
    (mcar (mcdr tree)))) 

(define right-branch
  (lambda (tree)
    (mcar (mcdr (mcdr tree)))))

(define set-entry!
  (lambda (tree value)
    (set-mcar! tree value)))

(define adjoin-set
  (lambda (x set)
    (cond
     ((null? set) (make-tree x '() '())) 
     ((= (key x) (key (entry set))) set)            
     ((< (key x) (key (entry set)))
      (make-tree (entry set)
		 (adjoin-set x (left-branch set))
		 (right-branch set)))
     ((> (key x) (key (entry set)))
      (make-tree (entry set)
		 (left-branch set)
		 (adjoin-set x (right-branch set)))))))

;;; Construct greedy binary decison tree from current measurement model ;;;

(define greedy-policy
  (lambda (U W)
    (lambda (k x)
      (let ((u-list (U k x)))
	(argmin
	 (map (lambda (u)
		(cons u
		      (sum-list
		       (map (lambda (w)
			      (* (p w x u)
				 (g k x u w)))
			    W))))
	      u-list))))))

(define policy-node-list
  (lambda (policy N x0)
    (letrec ((preorder
	      (lambda (k x)
		(let ((u (policy k x))
		      (k1 (add1 k)))
		  (cond
		   ((or (> k1 N) (null? u)) '())
		   (else
		    (append
		     (list (make-node u '() '()))
		     (preorder k1 (f k x u 0))
		     (preorder k1 (f k x u 1)))))))))
      (filter (lambda (x)
		(not (equal? (key x) 1.0)))
	      (remove-duplicates
	       (preorder 0 x0))))))

(define build-binary-tree
  (lambda (node-list)
    (foldl adjoin-set '() node-list)))

(define F-interval
  (lambda (x x0)
    (let ((a (left-state x0))
	  (b (right-state x0)))
      (F x a b))))

(define inv-F-interval
  (lambda (p x0)
    (let ((a (left-state x0))
	  (b (right-state x0)))
      (inv-F p a b))))

(define optimal-leaf-intervals
  (lambda (nol inv-cdf) 
    (build-list nol
		(lambda (x)
		  (cons (inv-cdf (/ x nol))
			(inv-cdf (/ (add1 x)  nol)))))))

;;; Construct optimal binary decision tree

(define bottom-up-tree-relabel!
  (lambda (bt nol inv-cdf)
    (let ((labels (optimal-leaf-intervals nol inv-cdf)))
      (letrec ((postorder
		(lambda (t)
		  (cond
		   ((null? t)
		    (let ((label (car labels)))
		      (set! labels (cdr labels))
		      label))
		   (else
		    (let ((left-child
			   (postorder (left-branch t)))
			  (right-child
			   (postorder (right-branch t))))
		      (cond
		       ((and (dotted-pair? left-child) (dotted-pair? right-child))
			(let ((node (make-node
				     (cdr left-child)
				     (car left-child)
				     (cdr right-child))))
			  (set-entry! t node)   
			  node))
		       ((dotted-pair? left-child)
			(let ((node (make-node
				     (cdr left-child)
				     (car left-child)
				     (store-right right-child))))
			  (set-entry! t node)   
			  node))
		       ((dotted-pair? right-child)
			(let ((node (make-node
				     (car right-child)
				     (store-left left-child)
				     (cdr right-child))))
			  (set-entry! t node)
			  node))
		       (else
			(let ((node (make-node
				     (store-right left-child)
				     (store-left left-child)
				     (store-right right-child))))
			  (set-entry! t node)
			  node)))))))))
	(postorder bt)
	(void)))))
