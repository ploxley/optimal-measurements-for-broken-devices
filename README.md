# Code for "Finding optimal measurement sequences for broken or constrained measuring devices" <br> [doi 10.48550/arXiv.2611.02470](https://arxiv.org/abs/2611.02470)

This code implements the methods presented in the journal article "Finding optimal measurement sequences for broken or constrained measuring devices". To run this code requires `Racket`, although it should also run under most LISP Scheme implementations with minimal modification. Download a snapshot of the repository's files as a zip file to your local computer, and then unpack the zip file. 

## Dynamic programming method

To reproduce the results for Figure 1, type `(enter! "run_dp_model.scm")` on the command line of the interpreter (REPL). The first few lines of output give:

```
1: Optimal policy:
2: policy for outcome (0 0)
3: 0 (0.0 . 1.0) 0.33
4: 1 (0.0 . 0.33) 0.33
5:   (0.0 . 0.33)
6: final J value = 1.5848
```
Lines 1 and 2 state this is the optimal policy for measurement outcome *LL*, corresponding to `outcome (0 0)`. Measurement outcome *RR* would correspond to `outcome (1 1)`. On Line 3, the first measurement takes interval [0, 1) and bisects it at 0.33, leading to the sub-interval [0, 0.33) on Line 4 after the first measurement outcome is received. On Line 4, the second measurement takes interval [0, 0.33) and again bisects it at 0.33, leading to no change in the sub-interval one Line 5. One Line 5, the final sub-interval is [0, 0.33). On Line 6, the information from the measurement is given as 1.5848 bits. There is a similar output for the greedy policy. 

The second part of the output gives:
```
1: Bisections for optimal: 
2: (0 . 0.33)
3: (1 . 0.33)
4: (1 . 0.66)
```
This output summarizes the bisections used by the optimal policy. On Line 2, the first measurement bisected the interval at 0.33. On Line 3, the second measurement bisected the left sub-interval [0, 0.33) at 0.33, and the right sub-interval [0.33, 1) at 0.66. The only new bisection on the second measurement was 0.66. This is displayed in the optimal policy tree given in the journal article. There is a similar output for the greedy policy. 

## Binary decision tree method

The previous method uses dynamic programming to solve the static sensor model. We can instead construct a binary decision tree to solve this model by typing `(enter! "run_bdt_model.scm")` on the command line. The second part of the output gives:

```
1: Optimal binary decision tree:
2: (mcons
3:  '(1/3 0 1)
4:  (mcons '() (mcons (mcons '(2/3 1/3 1) (mcons '() (mcons '() '()))) '())))
5: Number of leaves: 3
6: Average path length: 1.6667
7: Number of measurements: 2
8: Number of bits: 1.585
```
The binary decision tree is made from constructors defined in the file `binary_decision_tree.scm"`, and makes use of mutable lists. The easiest way to decompose this tree into its left and right branches is to make use of the selectors defined in the file.

Each internal node of the tree is a list that gives the proposed bisection, followed by the left and right end-points of the current sub-interval. Leaf nodes are not displayed. Left branches of the tree correspond to *L* measurement outcomes, and right branches of the tree, to *R* measurement outcomes; similar to the policy trees shown in the journal article. Lines 2--4 make it clear the bisections are 1/3 and 2/3 for this example. Lines 5--8 give further details about the measurement sequence. A similar output is given for the greedy binary decision tree.

Now that we can interpret the output from both methods, I will briefly discuss how to change some of the key model parameters, and how to switch between different constraints and different state distributions.  

## Changing the number of measurements
In either of the files `run_dp_model.scm` or `run_bdt_model.scm`, the number of measurements is changed on the line:

`(define N 2) ; number of measurements to take`

which is currently set for two measurements. For example, to check the data point at `N = 10` in Figure 2, we set `(define N 10)` and find the following output for `run_bdt_model.scm`:
```
Greedy binary decision tree:
...
Number of bits: 2

Optimal binary decision tree:
...
Number of bits: 3.4594
```
We can check this last result using the analytical formula `(log2 (+ N 1))`, which returns the same value. The key point to remember here is that as `N` is increased, `n` on the next line:

`(define n #e1e3) ; number of possible measurements to choose from: |U|`

may also need to be increased, sometimes by orders of magnitude (for exponential growth). This will, of course, slow down the computation (more so for the dynamic programming method than for the binary decision tree: see journal article). 

## Changing from static to dynamic constraints
To do this we need to edit the Racket module system. In each of the files `dynamic_programming.scm`, `binary_decision_tree.scm`, `run_dp_model.scm`, and `run_bdt_model.scm` there are two lines of the form:

```
(require "static_sensor_model.scm")
;(require "time_delay_sensor_model.scm")
```
To change to the dynammic constrain, we simply comment-out the first line, and un-comment the second line:

```
;(require "static_sensor_model.scm")
(require "time_delay_sensor_model.scm")
```
In addition, in the `run_dp_model.scm` and `run_bdt_model.scm` files, we need to change `initial-state` from:

```
(define initial-state '(0.0 . 1.0))
;(define initial-state '((0.0 . 1.0) (0 . 0)))
```
To:

```
;(define initial-state '(0.0 . 1.0))
(define initial-state '((0.0 . 1.0) (0 . 0)))
```

Reproducing the `N = 3` results for Dynamic Constraint 2 can be done this way. Further, we can switch to Dynamic Constraint 1 by editing the file `time_delay_sensor_model.scm`. In particular, in the definition of function `f`, the following two lines are relevant:

```
;((= m 0) delay-time) ; Dynamic Constraint 1
((and (= (prev-state x) m) (> k 0)) delay-time) ; Dynamic Constraint 2
```

These lines must be modified to:

```
((= m 0) delay-time) ; Dynamic Constraint 1
;((and (= (prev-state x) m) (> k 0)) delay-time) ; Dynamic Constraint 2
```

in order to activate Dynamic Constraint 1. Reproducing the `N = 3` results for Dynamic Constraint 1 should now be possible.

A final parameter to consider editing in `time_delay_sensor_model.scm` is `(define delay-time 1)`, which is currently set to 1.

## Changing from a uniform state to a non-uniform state
Let's go back to the `N = 2` case of Static Constraint (when reversing the previous edits don't forget to change the initial state back). The first change occurs in the function definition for `p` in `static_sensor_model.scm` (similarly in `time_delay_sensor_model.scm`). In that function are the two lines given by:

```
(let ((p0 (/ (- u (left-state x)) (- (right-state x) (left-state x)))))
;(let ((p0 (F u (left-state x) (right-state x))))
```
which must now be changed to:

```
;(let ((p0 (/ (- u (left-state x)) (- (right-state x) (left-state x)))))
(let ((p0 (F u (left-state x) (right-state x))))
```
In addition, in the file `run_bdt_model.scm`, we must change the lines:

```
(define cdf (lambda (x) x)) ; cdf and inv-cdf for uniform
(define inv-cdf (lambda (p) p))
;(define cdf (lambda (x) (F-interval x initial-state))) ; cdf and inv-cdf for truncated Gaussian
;(define inv-cdf (lambda (p) (inv-F-interval p initial-state)))
```

to:

```
;(define cdf (lambda (x) x)) ; cdf and inv-cdf for uniform
;(define inv-cdf (lambda (p) p))
(define cdf (lambda (x) (F-interval x initial-state))) ; cdf and inv-cdf for truncated Gaussian
(define inv-cdf (lambda (p) (inv-F-interval p initial-state)))
```

Once these changes have been made, it should be possible to reproduce the results corresponding to Figure 8 with the static constraint.


