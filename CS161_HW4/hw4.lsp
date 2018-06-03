;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Keith Mitchell
;;; HW 4
;;; 704281781
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;SAT?
;    core top level function
;    calls the create-values where a binary list is created
;    then create values will proceed the flow to tree-search

(defun sat? (n delta)
    (create-values n delta NIL)
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;CREATE-VALUES
;    this will construct a list of 0's
;    when n = 0 we have created a long enough list from the recursive call
;    tree-search is then called

(defun create-values (n delta values)
;(print "create-values")
;(print n)
;(print delta)
;(print values)

    (cond((= n 0) (tree-search (length values) delta values 0))
         (t (create-values (- n 1) delta (cons 0 values)))
    )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;TREE-SEARCH
;    core function for changing the values and evaluating all circumstances
;    helper functions are:
;         check-satisfaction-> iterates through the clauses and evaluates whether they are all true
;         values-return -> returns the current values that satisfied the CNF, no longer in the binary
;         change-value -> changes the value in the list at the position passed to it
;    performs a recursive call on itself for each of the two binary values at each position

(defun tree-search (n delta values pos)
;(print "tree-search")
;(print n)
;(print delta)
;(print values)
;(print pos)

    ;if the current values set in the search tree are valid then return them
    (cond((and (and (= (- n 1) pos) (not (check-satisfaction n delta values)))
                                    (not (check-satisfaction n delta (change-value values pos)))) NIL)

         ;if the values satisfy the delta then return T and call another function that returns the values
         ((check-satisfaction n delta values) (values-return n 1 values))
         ((check-satisfaction n delta (change-value values pos)) (values-return n 1 (change-value values pos)))

         ;move through each position and try both the changed and not changed value in a search tree
         (t (or (tree-search n delta values (+ pos 1))
                (tree-search n delta (change-value values pos) (+ pos 1))))

    )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;GET-VALUE
;    returns the value (binary) at the given pos
;    recursive call to itself to iterate until pos = 0 and the desired one is found
(defun get-value (values pos)
    (cond ((= pos 0)(car values))
          (t (get-value (cdr values) (- pos 1)))
    )
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;CHANGE-VALUE
;    this function simply goes through the values to a certain pos
;    it takes whatever position and swaps the binary value
;    recursive call to the change-value function till pos=0 (coordinate is reached)
(defun change-value (values pos)
    (cond ((= pos 0)(cond((= (get-value values pos) 1) (cons 0 (cdr values)))
                         ((= (get-value values pos) 0) (cons 1 (cdr values)))
                    )
          )
          (t (or (cons (car values) (change-value (cdr values) (- pos 1)))))
    )
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;CHECK-SATISFACTION
;    this function will be called by tree search and is the core evaluator function for the tree-search
;    helper function for tree search
;    uses the helper functions: check-line
;    recursive call to itself to iterate through each of the clauses

(defun check-satisfaction (n delta values)
;(print "check-satisfaction")
;(print n)
;(print delta)
;(print values)

;recursive function to each of the lines in the delta
;will call the helper function check-line to check each of the individual clauses
    (cond ((NULL delta) T)
          ((or (atom (car delta))(atom delta))(check-line delta values))
          ;and is used because in the CNF all of the clauses should be satisfied
          ((and (check-satisfaction n (car delta) values)
                (check-satisfaction n (cdr delta) values)) T)
    )

)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;CHECK-LINE
;    helper function for the check-satisfaction function
;    evaluates to whether or not the clause is satisfied that is passed from recursive call to check-sat

(defun check-line (delta values)
;(print "Check Line")
;(print delta)
;(print values)

    (cond ((numberp delta) (check-value (abs delta) delta values))
          ((= (length delta) 1) (check-line (car delta) values))
          ;use the or here since one of the individual binary variables must be true for the current values
          ;recursive call to itself in order to iterate through each of the values in the line
          ((or (check-line (car delta) values) (check-line (cdr delta) values)) T)
          (t NIL)
    )
)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;VALUES-RETURN
;    helper function for the tree search
;    returns a list of 1 through n that are neg/pos based on the current values
;    recursive call to itself in order to construct the list for of the the values 1 through n

(defun values-return (n pos values)
;(print "values-return")
;(print n)
;(print pos)
;(print values)

;n will be the number to go to and pos will be the current position in the values return
;call this function with pos = 1 for the first in 1 through n

    (cond ((numberp values) (cons (binary-to-num values pos) NIL))
          ((= pos n) (cons (binary-to-num (car values) pos) NIL))
          ;((= (length values) 1) (binary-to-num (car values) pos))
          (t (cons (binary-to-num (car values) pos)(values-return n (+ pos 1) (cdr values))))
    )

)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;CHECK-VALUE
;    takes a pos or value of n, the variable the binary values

(defun check-value (pos variable values)
;(print "check-value")
;(print pos)
;(print variable)
;(print values)

; recursive call to go through list of values n (for the variable) number of times
; pos and variable are initially the same because we want to move to the position invalues where these equal
    (cond ((= pos 1) (= (convert-to-binary variable) (car values)))
          (t (check-value (- pos 1) variable (cdr values)))
    )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;CONVERT-TO-BINARY
;    helper function fro the check-value function which is called by the check-line function

(defun convert-to-binary (variable)
;(print "convert to bin")
;(print value)

;ultimately we will represent our values in the binary list as 0 for "not" or the -n and 1 for +n
    (cond ((< variable 0) 0)
          (t 1)
    )
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;BINARY-TO-NUM
;    this function returns the binary value interpretation of the pos or value of n in a clause
;    if binary = 1 then pos will be returned as positive
;    if binary = 0 then pos will be returned as negative
;    helper function for the values return where its recursive call to itself calls this function

(defun binary-to-num (value pos)
    (cond((= value 1) pos)
         ((= value 0) (- 0 pos))
    )
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(format t "1. Sat (-1 -2 3) = ~a ~%" (sat? 3 '((1 -2 3) (−1) (-2 3))))
;(format t "2. Sat = ~a ~%" (sat? 1 '((1) (−1))))

;(print "TEST FUNCTIONS")

;(format t "1. Change Value: (0 1 1) = ~a ~%" (change-value '(1 1 1) 0))
;(format t "1.1. Change Value: (1 1 0) = ~a ~%" (change-value '(1 1 1) 2))
;(format t "2. Change Value: (1 1 1) = ~a ~%" (change-value '(1 0 1) 1))
;(format t "3. Get Value: 1 = ~a ~%" (get-value '(1 9 9) 0))



;(format t "4.0. Check value: T = ~a ~%" (check-value 1 1 '(1 0 1)))
;(format t "4.0. Check value: T = ~a ~%" (check-value 1 3 '(1)))
;(format t "4. Check line: T  =  ~a ~%" (check-line '(-1) '(0 0 1)))
;(format t "4.1. Check line: T  =  ~a ~%" (check-line '(-1 2 3) '(1 0 1)))
;(format t "4.2. Check line: T  =  ~a ~%" (check-line '(1 -2 -3) '(1 0 0)))
;(format t "4.3. Check line: T  =  ~a ~%" (check-line '(-1 -2 3) '(0 0 1)))
;(format t "4.4. Check line: T  =  ~a ~%" (check-line '(1 -2 3) '(1 0 1)))
;(format t "4.5. Check line: T  =  ~a ~%" (check-line '(-1 -3 -2) '(0 0 0)))
;(format t "4.6. Check line: NIL  =  ~a ~%" (check-line '(-3 -2 -1) '(1 1 1)))
;(format t "4.7. Check line: NIL  =  ~a ~%" (check-line '(1 2 3) '(0 0 0)))
;(format t "4.8. Check line: T  =  ~a ~%" (check-line '(1 -2 3) '(0 0 1)))
;(format t "4.9. Check line: NIL  =  ~a ~%" (check-line '(-3) '(0 0 1)))
;(format t "4.10. Check line: T  =  ~a ~%" (check-line '(-2 3) '(0 0 1)))


;(format t "5.0. Check sat: T  =  ~a ~%" (check-satisfaction 3 '((1 -2 3) (-1) (-2 3)) '(0 0 1)))
;(format t "5.3. Check sat: NIL  =  ~a ~%" (check-satisfaction 3 '((1 -2 3) (1) (-2 3)) '(0 1 1)))
;(format t "5.4. Check sat: T  =  ~a ~%" (check-satisfaction 3 '((1 -2 3) (-1) (-2 3)) '(0 1 1)))
;(format t "5.5. Check sat: NIL  =  ~a ~%" (check-satisfaction 3 '((1 -2 3) (-1) (-2 3)) '(1 0 1)))
;(format t "5.6. Check sat: T  =  ~a ~%" (check-satisfaction 3 '((1 -2 3) (-1) (-2 3)) '(0 0 0)))
;(format t "5.7. Check sat: NIL  =  ~a ~%" (check-satisfaction 3 '((1 -2 3) (-1) (-2 3)) '(1 1 1)))
;(format t "5.8. Check sat: NIL  =  ~a ~%" (check-satisfaction 3 '((1 -2 3) (-1) (-2 3)) '(1 0 1)))
;(format t "5.9. Check sat: NIL  =  ~a ~%" (check-satisfaction 3 '((1 -2 3) (-1) (-2 3)) '(1 0 1)))
;(format t "5.10. Check sat: NIL  =  ~a ~%" (check-satisfaction 3 '((1 -2 3) (-1) (-2 3)) '(1 0 1)))


;(format t "6.1. Values Return: (-1 2 -3)  =  ~a ~%" (values-return 3 1 '(0 1 0)))
;(format t "6.1. Values Return: (-1 2 -3)  =  ~a ~%" (values-return 4 1 '(0 1 0 0)))
;(format t "6.1. Values Return: (-1 2 -3)  =  ~a ~%" (values-return 5 1 '(0 1 0 0 0)))
;(format t "6.1. Values Return: (-1 2 -3)  =  ~a ~%" (values-return 6 1 '(0 1 0 0 0 0)))

;(format t "7.0. Check tree: T  =  ~a ~%" (tree-search 3 '((1 -2 3) (-1) (-2 3)) '(0 0 1) 0))


;(format t "8.0. Sat: T  =  ~a ~%" (sat? 3 '((1 -2 3) (-1) (2 3))))
;(format t "8.1. Sat: NIL  =  ~a ~%" (sat? 3 '((1 -2 3) (-1) (1))))
;(format t "8.2. Sat: (1 2 3)  =  ~a ~%" (sat? 3 '((1 -2 -3) (-2 3) (2 -3) (2 3))))
;(format t "8.3. Sat: NIL  =  ~a ~%" (sat? 4 '((-1 -2 -3) (-2 3) (2 -3) (2 3) (-4))))
;(format t "8.4. Sat: NIL  =  ~a ~%" (sat? 4 '((-1 -2 -3) (-2 3) (2 -3) (2 3) (-4))))
;(format t "8.5. Sat: (1 2 3)  =  ~a ~%" (sat? 3 '((1) (3) (2))))
;(format t "8.6. Sat: (1 -2 3)  =  ~a ~%" (sat? 3 '((1) (3) (-2))))
;(format t "8.7. Sat: (1 2 -3)  =  ~a ~%" (sat? 3 '((1) (-3) (2))))
;(format t "8.8. Sat: (1 -2 -3)  =  ~a ~%" (sat? 3 '((1) (-3) (-2))))
;(format t "8.9. Sat: (-1 -2 -3)  =  ~a ~%" (sat? 3 '((-1) (-3) (-2))))
;(format t "8.10. Sat: (-1 -2 3)  =  ~a ~%" (sat? 3 '((-1) (3) (-2))))
;(format t "8.10. Sat: (-1 2 -3)  =  ~a ~%" (sat? 3 '((-1) (-3) (2))))



