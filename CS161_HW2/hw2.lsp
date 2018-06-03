;;;;Keith Mitchell
;;;;704281781
;;;;HW #2 DUE 10/19




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;QUESTION 1
;;;
;;;DESCRIPTION:
;;;    Write a single pure LISP function, called DFS, that performs a depth-first search of a tree. The function
;;;    should take a single argument that is the list representation of the tree, and returns a single, top-level list of
;;;    the terminal nodes in the order they would be visited by a left-to-right depth-first search. For example, (dfs
;;;    '((A (B)) C (D))) would return (A B C D). Do not use any auxiliary functions.

(defun DFS(TREE)
    (cond ((NULL TREE) NIL) ;check to make sure that the tree is not empty

          ;;base check, to see if an individual object is all that is left in the tree, in which case construct to append
          ((not(listp TREE)) (cons TREE NIL)) 
          
          ;;if a list is left with only length 1 then break down parentheses (span levels) by using grabbing 
          ;;    the first and only value with car
          ((= (length TREE) 1) (DFS(car TREE)))

          ;;if >1 (not number) append TREE-ORDER of each of the items in the tree to a list
          ;;    this recursive call will ensure that any list with in the tree are continuosly broken down
          (t (append (DFS (car TREE)) (DFS (cdr TREE))))
))

;;;TEST CASES FOR QUESTION 1
;(print "TEST CASES FOR QUESTION 1")

;(print (DFS '((A (B)) C (D))))
;(print (DFS '((A ((B))) C (D))))
;(print (DFS '((A (B)) C (D))))
;(print (DFS '((A (B)) C (D))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;QUESTION 2

;;;DESCRIPTION
;;;    Write a set of pure LISP functions that implement depth-first iterative-deepening. Your top-level function,
;;;    called DFID, should take two arguments, the list representation of the tree, and an integer representing
;;;    the maximum depth of the tree, and returns a single top-level list of the terminal nodes in the order that they
;;;    would be visited by a left-to-right depth-first iterative-deepening search. Note that those nodes that are visited
;;;    in multiple iterations will appear multiple times in the output list. For example, (dfid '((A (B)) C (D))
;;;    3) would return (C A C D A B C D).

;;;    Each of these functions must work for trees of arbitrary depth and branching factor, and hence you may not
;;;    assume any a priori upper bound on these parameters. Be sure to exhibit sufficient test cases to convince
;;;    yourself and us that your programs work in general. Try at least the examples above, as well as the list (A
;;;    (B C) (D) (E (F G))).

(defun DFID-DFS (TREE DEPTH)
    (cond ((NULL TREE) NIL) ;check to make sure that the tree is not empty
          
          ((< DEPTH 0) NIL)

          ;;base check, to see if an individual object is all that is left in the tree, in which case construct to append
          ((not(listp TREE)) (cons TREE NIL)) 
          
          ;;if a list is left with only length 1 then break down parentheses (span levels) by using grabbing 
          ;;    the first and only value with car
          ((= (length TREE) 1) (DFID-DFS (car TREE) (- DEPTH 1)))

          ;;if >1 (not number) append TREE-ORDER of each of the items in the tree to a list
          ;;    this recursive call will ensure that any list with in the tree are continuosly broken down
          (t (append (DFID-DFS (car TREE) (- DEPTH 1)) (DFID-DFS (cdr TREE) DEPTH)))

))

(defun DFID (TREE DEPTH)
    (cond ((NULL TREE) NIL)
          ((< DEPTH 0) NIL)
          (t (append (DFID TREE (- DEPTH 1))(DFID-DFS TREE DEPTH)))

))

;(print (DFID '((A (B)) C (D)) 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;QUESTION 3

;;; These functions implement a depth-first iterative-deepening solver for the
;;; missionary-cannibal problem. In this problem, three missionaries and three
;;; cannibals are trying to go from the east side of a river to the west side.
;;; They have a single boat that can carry two people at a time from one side of
;;; the river to the other. There must be at least one person in the boat to cross
;;; the river. There can never be more cannibals on one side of the river than
;;; missionaries. If there are, the cannibals eat the missionaries.

;;; In this implementation, a state is represented by a single list (MISSIONARIES
;;; CANNIBALS SIDE). SIDE represents which side the boat is currently on, and is T
;;; if it is on the east side and NIL if on the west side. MISSIONARIES and
;;; CANNIBALS represent the number of missionaries and cannibals on the same side
;;; as the boat. Thus, the initial state for this problem is (3 3 T) (three
;;; missionaries, three cannibals, and the boat are all on the east side of the
;;; river) and the goal state is (3 3 NIL).

;;; The main entry point for this solver is the function ID-DFS, which is called
;;; with the initial state to search from and the depth up to which depth-first
;;; search will be performed. It returns the complete path from the initial state
;;; to the goal state: this path is a list of intermediate problem states. The
;;; first element of the path is the initial state and the last element is the
;;; goal state. Each intermediate state is the state that results from applying
;;; the appropriate operator to the preceding state.

;;; To solve the original problem, one would call (ID-DFS '(3 3 T) 0). 

;;; Examples of calls to some of the helper functions can be found after the code.

;;---------------------------------------------------------------------------------------------

;;;DESCRIPTION
;;;    FINAL-STATE takes a single argument (S), the current state, and returns T if
;;;    it is the goal state (3 3 NIL) and NIL otherwise.

;;;PERSONAL NOTES
;;;    This function will be called by SINGLE-DFS before generation

(defun FINAL-STATE (S)
 (cond ((equal '(3 3 NIL) S) T); return T if goal state
       (t NIL); otherwise return NIL
))

;;--------------------------------------------------------------------------------------------

;;;DESCRIPTION: NEXT-STATE
;;;    NEXT-STATE returns the state that results from applying an operator to the
;;;    current state. It takes three arguments: the current state (S), a number of
;;;    missionaries to move (M), and a number of cannibals to move (C). It returns a
;;;    list containing the state that results from moving that number of missionaries
;;;    and cannibals from the current side of the river to the other side of the
;;;    river. If applying this operator results in an invalid state (because there
;;;    are more cannibals than missionaries on either side of the river, or because
;;;    it would move more missionaries or cannibals than are on this side of the
;;;    river) it returns NIL.

;;;PERSONAL NOTES
;;;    NOTE that next-state returns a list containing the successor state (which is
;;;    itself a list); the return should look something like ((1 1 T)).
;;;    s = (MISSIONARIES CANNIBALS SIDE)

(defun NEXT-STATE (S M C)
(cond((< (- (first S) M) 0) NIL) ;make sure enough of the missionairies exist on the states side
     ((< (- (second S) C) 0) NIL) ;make sure enough of the cannibals exist on the states side
    
     ;;check to make sure not more C then M on either side, not valid if number of M is 0
     ((and (< (- (first S) M) (- (second S) C)) (not(= (- (first S) M) 0))) NIL) ;current states side
     ((and (< (+ M (- 3 (first S))) (+ C (- 3 (second S)))) (not(= (+ M (- 3 (first S))) 0))) NIL) ;check other side

     ;;otherwise we have a valid move so return it
     (t (list(list (+ (- 3 (first S)) M) (+ (- 3 (second S)) C ) (not (third S)))))

))

;;------------------------------------------------------------------------------------------

;;;DESCRIPTION: SUCC-FN
;;;    SUCC-FN returns all of the possible legal successor states to the current
;;;    state. It takes a single argument (S), which encodes the current state, and
;;;    returns a list of states that can be reached by applying legal operators to
;;;    the current state.

;;;PERSONAL NOTES
;;;  Given S then: (5 options to check to see if valid by calling NEXT-STATE)
;;;    M can be 2 and C can be 0
;;;    M can be 1 and C can be 1
;;;    M can be 0 and C can be 2
;;;    M can be 1 adn C can be 0
;;;    M can be 0 and C can be 1

(defun SUCC-FN (S)
(cond(t (append (NEXT-STATE S 2 0) (NEXT-STATE S 0 2)
                (NEXT-STATE S 1 1) (NEXT-STATE S 0 1)
                (NEXT-STATE S 1 0)))
))

;;-------------------------------------------------------------------------------------------

;;;DESCRIPTION: MULT-DFS
;;;    MULT-DFS is a helper function for SINGLE-DFS. It takes three arguments: the
;;;    path from the initial state to the current state (PATH), the legal successor
;;;    states to the last state on PATH (STATES), and the depth (DEPTH). PATH is a
;;;    first-in first-out list of states; that is, the first element is the initial
;;;    state for the current search and the last element is the most recent state
;;;    explored. MULT-DFS does a single depth-first iteration to the given depth on
;;;    each element of STATES in turn. If any of those searches reaches the final
;;;    state, MULT-DFS returns the complete path from the initial state to the goal
;;;    state. Otherwise, it returns NIL.

;;;PERSONAL NOTES
;;;    I like to think of this as the function that is going to process multiple STATES generated by SINGLE-DFS
;;;    Expansion of the single state will be carried out by the SINGLE-DFS function
;;;    This function will itterate through the fringe and call SINGLE-DFS for first object
;;;    Since it calls for the first object this makes this a depth first search to current iteration of depth

(defun MULT-DFS (STATES PATH DEPTH)

(cond;;if SINGLE-DFS is not NIL (has depth or is GS) then initiate expansion of that 
     ((SINGLE-DFS (car STATES) PATH DEPTH) (SINGLE-DFS (car STATES) PATH DEPTH))
     ;;otherwise move across fringe with recursive call to MULT-DFS
     ((cdr STATES) (MULT-DFS (cdr STATES) PATH DEPTH))
     (t NIL)			
))

;;-------------------------------------------------------------------------------------------

;;;DESCRIPTION: SINGLE-DFS
;;;    SINGLE-DFS does a single depth-first iteration to the given depth. It takes
;;;    three arguments: a state (S), the path from the initial state to S (PATH), and
;;;    the depth (DEPTH). If S is the initial state in our search, PATH should be
;;;    NIL. It performs a depth-first search starting at the given state. It returns
;;;    the path from the initial state to the goal state, if any, or NIL otherwise.

;;;PERSONAL NOTES
;;;    first we will want to check if the state that is to be checked is the goal
;;;    if so we can return the path to the state
;;;    I like to think of this function as the generation step
;;;    Check to see if there is still depth, otherwise will explore rest of existing fringe.
;;;    If still depth to go calls SUCC-FN to find out the valid steps to generate
;;;    Calls MULT-DFS to explore the newly generate objects

(defun SINGLE-DFS (S PATH DEPTH); S should be 1 item here and will be 
(cond((FINAL-STATE S) (append PATH (list S))); check if we found the goal state
     ;;if DEPTH < 0 then this is the limit of the current itterative search so return NIL at the end
     ;;Generate and append S to path which will now be the parent node, decrease depth
     ((<= 0 DEPTH) (MULT-DFS (SUCC-FN S) (append PATH (list S)) (- DEPTH 1)) )
     (t NIL)
))

;;-------------------------------------------------------------------------------------------

;;;DESCRIPTION: ID-DFS
;;;    ID-DFS is the top-level function. It takes two arguments: an initial state (S)
;;;    and a search depth (DEPTH). ID-DFS performs a series of depth-first
;;;    iterations, starting from the given depth until a solution is found. It
;;;    returns the path from the initial state to the goal state. The very first call
;;;    to ID-DFS should use depth = 0.

;;;PERSONAL NOTES
;;;   Recursive funciton ID-DFS that will generate an increasing itterative call to SINGLE-DFS

(defun ID-DFS (S DEPTH)
(cond((SINGLE-DFS S NIL DEPTH) (SINGLE-DFS S NIL DEPTH));if SINGLE-DFS is not NIL call it
     (t (ID-DFS S (+ DEPTH 1)));otherwise continue recurive increasing itterative call to ID-DFS
))

;;-------------------------------------------------------------------------------------------

; Function execution examples

;(print (NEXT-STATE '(1 1 T) 1 1))
;(print (ID-DFS '(1 1 T) 0))
;(print (ID-DFS '(2 2 T) 0))
;(print (NEXT-STATE '(0 0 T) 1 1))
;(print (NEXT-STATE '(3 3 T) 1 1))
;(print (NEXT-STATE '(1 1 T) 0 2))
;(print (NEXT-STATE '(3 3 T) 2 0))
;(print (NEXT-STATE '(3 3 T) 1 0)) ;"= NIL")
;(print (NEXT-STATE '(3 3 T) 0 1)) ;"= ((0 1 NIL))")

; Applying this operator would result in an invalid state, with more cannibals
; than missionaries on the east side of the river.

;(print (next-state '(3 3 t) 1 0)); -> NIL

; Applying this operator would result in one cannibal and zero missionaries on
; the west side of the river, which is a legal operator. (note that NEXT-STATE
; returns a LIST of successor states, even when there is only one successor)

;(print (next-state '(3 3 t) 0 1)); -> ((0 1 NIL))

; SUCC-FN returns all of the legal states that can result from applying
; operators to the current state.

;(print (succ-fn '(3 3 t))); -> ((0 1 NIL) (1 1 NIL) (0 2 NIL))
;(print (succ-fn '(1 1 t))); -> ((3 2 NIL) (3 3 NIL))
