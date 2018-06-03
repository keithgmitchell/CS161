
(defun sat? (n delta)
    (cond(t (create-values n delta NIL))
    )
)

;*
(defun create-values (n delta values)
(print "create-values")
(print n)
(print delta)
(print values)
;;;;THIS IS WRONG WE ARE CALLING N=0 for tree which doesnt work there
    (cond((= n 0) (tree-search n delta values 0))
         (t (create-values (- n 1) delta (cons T values)))
    )
)

(defun tree-search (n delta values pos)
(print "tree-search")
(print n)
(print delta)
(print values)

    ;if the current values set in the search tree are valid then return them (should eventually be function)
    (cond((and (and (= (- n 1) pos) (not (check-satisfaction n delta values)))
                                    (not (check-satisfaction n delta (change-value values pos))) NIL)

         ;if the settings satisfy the delta then return T and call another function that returns the values
         ((check-satisfaction n delta values) (values-return n delta values))
         ((check-satisfaction n detla (change-value values pos)) (values-return n delta values))

         ;move through each position and try both the changed and not changed value in a search tree
         (t (or (tree-search n delta values (+ pos 1))
                (tree-search n delta (change-value values (+ pos 1)))))

    )
)

;*
(defun get-value (values pos)
    (cond ((= pos 0)(car values))
          (t (get-value (cdr values) (- pos 1)))
    )
)

;*
(defun change-value (values pos)
    (cond ((= pos 0)(cond((= (get-value values pos) 1) (cons 0 (cdr values)))
                         ((= (get-value values pos) 0) (cons 1 (cdr values)))
                    )
          )
          (t (or (cons (car values) (change-value (cdr values) (- pos 1)))))
    )
)

;*
(defun check-satisfaction (n delta values)
;recursive function to each of the lines in the file
     (cond ((= (length values) 1) (check-line delta value))
           (t (or (check-satisfactions n (car delta) car values)
                  (check-satisfactions n (cdr delta) cdr values)))

)

(defun check-line (delta values)
    (cond ((= (length delta) 1) (check-value (abs delta) (abs delta) values))
          ((and (check-line (car delta)) (check-line (cdr delta))) T)
          (t NIL)
)

(defun values-return (n delta values)
)

(defun check-value pos variable values)
; recursive call to go through list of values n (for the variable) number of times
; pos and variable are initially the same because we want to move to the position invalues where these equal

)

(defun convert-to-binary (value)
;ultimately we will represent our values in the binary list as 0 for "not" or the -n and 1 for +n

    (cond ((< value 0) 0)
          (t 1)
)

(defun convert-to-value (number binary)

)

;(format t "1. Sat (-1 -2 3) = ~a ~%" (sat? 3 '((1 -2 3) (−1) (-2 3))))
;(format t "2. Sat = ~a ~%" (sat? 1 '((1) (−1))))
;(print "test functions")
;(sat? 1 '((1) (−1)))
(format t "1. Change Value = ~a ~%" (change-value '(1 1 1) 0))
(format t "2. Change Value = ~a ~%" (change-value '(1 0 1) 1))
(format t "3. Get Value = ~a ~%" (get-value '(1 9 9) 0))
;(format t "2. Change Value = ~a ~%" (sat? 1 '((1) (−1))))
;(format t "3. Change Value = ~a ~%" (sat? 1 '((1) (−1))))
