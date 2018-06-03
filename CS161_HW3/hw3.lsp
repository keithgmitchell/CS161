; Keith Mitchell
; 704281781
; CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;  
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time. 
;
(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4) ;goal
(setq boxstar 5) ;box and goal
(setq keeperstar 6) ;keeper on top of goal

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of a Sokoban game.
; (no box is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.

;   this function performs a simple recursion to check through all values and return false if any boxes (2) are found
;   null s means that this satifies the goal states (no boxes)
;   once an individual number is found and not a list then reurn opposite of if a box is found since that is NIL 
;               for our purposes in this situation and not T
(defun goal-test (s)
    (cond((NULL s) t)
         ((numberp s) (not (isBox s))) 
         (t (and (goal-test (car s)) (goal-test (cdr s))))
    )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; EXERCISE: Modify this function to return the list of 
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
; 
; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; 
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
;

;;GET-COORD
;    type refers to the coord we are intersted in.
;    type 0 = immediate-row (row of square closest to keeper in some given direction) 
;    type 1 = immediate-col (col of square closest to keeper in some given direction)
;    type 2 = subsequent-row (row of the space 2 spaces away in some given direction)
;    type 3 = subsequent-col (col of the space 2 spaces away in some given direction)

(defun get-coord (type kRow kCol dir)
    (cond((= type 0)(cond((= dir 0)(- kRow 1))
                         ((= dir 1)(+ kRow 1))
                         (t kRow)
                    )
         )
         ((= type 1)(cond((= dir 2)(- kCol 1))
                         ((= dir 3)(+ kCol 1))
                         (t kCol)
                    )
         )
         ((= type 2)(cond((= dir 0)(- kRow 2))
                         ((= dir 1)(+ kRow 2))
                         (t kRow)
                    )
         )
         ((= type 3)(cond((= dir 2)(- kCol 2))
                         ((= dir 3)(+ kCol 2))
                         (t kCol)
                    )
         )
    )
)

;;MOVE-TYPE
;    based on the configuration that of the next two square this code will deduce that information to a number
;    this function helps in the transition from check-move to make-move
(defun move-type (immediate subsequent)
    (cond((isStar immediate) 1)
	 ((isBlank immediate) 2)
	 ((isBox immediate)(cond ((isBlank subsequent) 3)
                                 ((isStar subsequent) 4)
                                 (t 0)
                           )
         )
	 ((isBoxStar immediate)(cond((isBlank subsequent) 5)
                                    ((isStar subsequent) 6)
                                    (t 0)
                               )
         )
	 (t 0)
    )
)


;;CHECK-VALUE
;    this function will return the number of a position using the row and column starting at 0
;    s = state
;    bin here is whether or not we should start counting down the column instead now
;    completed using a recursive call to itself and differentiating between moving through row/col with binary value bin
(defun check-value (s row col bin)
    (cond
         ((and (and (<= row 0) (= bin 1)) (<= col 0)) (car s))
         ((and (<= row 0) (= bin 1)) (check-value (cdr s) row (- col 1) bin))
         ((and (<= row 0) (= bin 0)) (check-value (car s) row col (+ bin 1)))
         (t (check-value (cdr s) (- row 1) col bin))
     )
) 

;;CHANGE-VALUE
;    recursive call in order to reconstruct the state passed to it and return it with a substituion
;    s = state, row = row of position, col = column of position, new = new object @ position
;    the final product from this function is actually what the function make-move will be returning to check-move and then back to next-states
(defun change-value (s row col new)
    (cond
	((NULL s) NIL)
	((> row 0) (cons (first s) (change-value (cdr s) (- row 1) col new)))
	((> col 0) (let*((x (change-value (cons (cdar s) (cdr s)) row (- col 1) new)))
		         (cons (cons (caar s) (car x)) (cdr x))
                   )
	)
	(t (cons (cons new (cdar s)) (cdr s))) 
    )
) 

;;MAKE-MOVE
;   this function will take the information given to it via move-type and check-move and return changed arenas
;   certain move types associate with the objects that were just in those positions and a required in order to know what to set those coordinates to after the move
;   MOVE-TYPE function will provide more information regarding the actual parameters that a given number associates with
(defun make-move (s kRow kCol dir move)
	(let* ((immediate-row (get-coord 0 kRow kCol dir))
               (immediate-col (get-coord 1 kRow kCol dir))
               (subsequent-row (get-coord 2 kRow kCol dir))
               (subsequent-col (get-coord 3 kRow kCol dir))
               (keeper-spot (check-value s kRow kCol 0))
               
               ;first make changes to where the keeper just moved from
               (changed-state (cond((isKeeper keeper-spot)(change-value s kRow kCol blank))
                                   ((isKeeperStar keeper-spot) (change-value s kRow kCol star))
                                   ((= move 0) NIL)
                                   (t s)
                      )
               )
               
               ;secondly make changes to where the keeper just moved to
               (changed-state (cond((or (or (= move 1)(= move 5))(= move 6))(change-value changed-state immediate-row immediate-col keeperstar))
                                   ((or (or (= move 2)(= move 3))(= move 4))(change-value changed-state immediate-row immediate-col keeper))
                                   ((= move 0) NIL)
                                   (t changed-state)
                      )
               ))
          
               ;finally make changes to the subsequent square, or the next one in the same direction to where the keeper moved 
	       (cond((or (= move 3) (= move 5)) (change-value changed-state subsequent-row subsequent-col box))
	            ((or (= move 6) (= move 4)) (change-value changed-state subsequent-row subsequent-col boxstar))
                    ((= move 0) NIL)
		    (t changed-state)
	       )
         )
)

;;CHECK-MOVE
;
(defun check-move (s kRow kCol dir)
	(let* ((immediate-row (get-coord 0 kRow kCol dir))
               (immediate-col (get-coord 1 kRow kCol dir))
               (subsequent-row (get-coord 2 kRow kCol dir))
               (subsequent-col (get-coord 3 kRow kCol dir))
	       (immediate (check-value s immediate-row immediate-col 0))
               (subsequent (check-value s subsequent-row subsequent-col 0))
               (s s)
              )
          
	       (cond((NULL s) nil)
	            ((or (< immediate-row 0) (<= (length s) immediate-row)) NIL)
	            ((or (< immediate-col 0) (<= (length (first s)) immediate-col)) NIL)
		    (t (make-move s kRow kCol dir (move-type immediate subsequent)))
		)
	)
)


;;NEXT-STATES
;   returns a list of the possible states that can be moved to
;   general flow from here is next-states->check-move->make-move
;   flow for this function will also include the helper functions:
        ;get-coord, change-value, check-value
(defun next-states (s)
    ; up = 0
    ; down = 1
    ; left = 2
    ; right = 3
    (let* ((kCol (first (getKeeperPosition s 0)))
	   (kRow (second (getKeeperPosition s 0)))
          )
        (cleanUpList(list (check-move s kRow kCol 0)
                      (check-move s kRow kCol 1)
                      (check-move s kRow kCol 2)
                      (check-move s kRow kCol 3)
                    )
        ) 
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
;
(defun h0 (s)
    0
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
;
(defun h1 (s)
    (cond((NULL s) 0)
         ((and (numberp s)(isBox s)) 1)
         ((and (numberp s)(not(isBox s))) 0)
         ((= 1 (length s)) (h1 (car s)))
         (t (+ (h1 (car s)) (h1 (cdr s))))
    )   
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
;

;H-Box-Keeper-2
;    more details in h704281781
(defun H-Box-Keeper-2 (s row kRow kCol r c)
    (cond((= (length row) 0) 0)
         ((or (>= c (length s))(< c 0)) 0)
	 ((isbox (car row)) (+ (+ (abs (- r kRow)) (abs (- c kCol))) (H-Box-Keeper-2 s (cdr row) kRow kCol r (+ 1 c))))
	 (t (H-Box-Keeper-2 s (cdr row) kRow kCol r (+ 1 c)))
    )
)
(print "here")
;H-Box-Keeper
;    more details in h704281781
(defun H-Box-Keeper (s r kRow kCol)
	(cond((= (length s) 0) 0)
             ((or (>= r (length s)) (> 0 r)) 0)
	     (t (+ (H-Box-Keeper-2 s (car s) kRow kCol r 0) (H-Box-Keeper (cdr s) (+ 1 r) kRow kCol)))
	)
)

;H70428178
;The overall scheme here is to call H-Box-Helper which breaks down all each row and adds them together using the sub function 
;H-Box-Helper 2 which goes through each individual column and add the number of moves until a given box is reached. Similarly
;the H-Box-Helper function does something similar while keeping track of the rows that it has moved when evaluating an individual row.1
(defun h704281781 (s)
	
   (let* (
          (kCol (first (getKeeperPosition s 0)))
          (kRow (second (getKeeperPosition s 0)))
         )
          ;Additionally the total distance between the keeper and the boxes is calculated and multiplied times the number of boxes
          ;therefore a box being on a start will have a very big pull on the heuristic score.
	  (* (H-Box-Keeper s 0 kRow kCol)(h1 s))
    
          ;In addition I was hoping to add this to the number of moves for each box to a star by searching the layers around the square.
          ;unfortunately I did not have time to implement my full heuristic due to spending so much time on next-states
     )
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are ordered roughly by their difficulties.
 | For most problems, we also privide 2 additional number per problem:
 |    1) # of nodes expanded by A* using our next-states and h0 heuristic.
 |    2) the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below 
 | was solved by 80 nodes expansion of A* and its optimal solution depth is 7.
 | 
 | Your implementation may not result in the same number of nodes expanded, but it should probably
 | give something in the same ballpark. As for the solution depth, any admissible heuristic must 
 | make A* return an optimal solution. So, the depths of the optimal solutions provided could be used
 | for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#

;(80,7)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)))

;(110,10)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 0 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(211,12)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(300,13)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 0 0)
	   (0 3 1 0 0 0 0)))

;(551,10)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 0 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(722,12)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 0 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(1738,50)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 0 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(1763,22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 0 0 4 1)
	   (1 1 1 1 1 1)))

;(1806,41)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 0 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(10082,51)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 0 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(16517,48)
(setq p11 '((1 1 1 1 1 1 1)
	    (1 4 0 0 0 4 1)
	    (1 0 2 2 1 0 1)
	    (1 0 2 0 1 3 1)
	    (1 1 2 0 1 0 1)
	    (1 4 0 0 4 0 1)
	    (1 1 1 1 1 1 1)))

;(22035,38)
(setq p12 '((0 0 0 0 1 1 1 1 1 0 0 0)
	    (1 1 1 1 1 0 0 0 1 1 1 1)
	    (1 0 0 0 2 0 0 0 0 0 0 1)
	    (1 3 0 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 2 1 1 1 0 0 0 1)
	    (1 0 0 0 0 1 0 1 4 0 4 1)
	    (1 1 1 1 1 1 0 1 1 1 1 1)))

;(26905,28)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 4 0 0 0 0 0 2 0 1)
	    (1 0 2 0 0 0 0 0 4 1)
	    (1 0 3 0 0 0 0 0 2 1)
	    (1 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 0 0 0 0 4 1)
	    (1 1 1 1 1 1 1 1 1 1)))

;(41715,53)
(setq p14 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 0 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 0 0)
	    (0 0 1 4 0 0 0)))

;(48695,44)
(setq p15 '((1 1 1 1 1 1 1)
	    (1 0 0 0 0 0 1)
	    (1 0 0 2 2 0 1)
	    (1 0 2 0 2 3 1)
	    (1 4 4 1 1 1 1)
	    (1 4 4 1 0 0 0)
	    (1 1 1 1 0 0 0)
	    ))

;(91344,111)
(setq p16 '((1 1 1 1 1 0 0 0)
	    (1 0 0 0 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(3301278,76)
(setq p17 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 0 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(??,25)
(setq p18 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 0 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))
;(??,21)
(setq p19 '((0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 0 1 0 0 1 0 0 0 0)
	    (0 0 0 0 0 0 3 0 0 0 2 0)
	    (0 0 0 0 1 0 0 1 0 0 0 4)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 2 0 4 1 0 0 0)))

;(??,??)
(setq p20 '((0 0 0 1 1 1 1 0 0)
	    (1 1 1 1 0 0 1 1 0)
	    (1 0 0 0 2 0 0 1 0)
	    (1 0 0 5 5 5 0 1 0)
	    (1 0 0 4 0 4 0 1 1)
	    (1 1 0 5 0 5 0 0 1)
	    (0 1 1 5 5 5 0 0 1)
	    (0 0 1 0 2 0 1 1 1)
	    (0 0 1 0 3 0 1 0 0)
	    (0 0 1 1 1 1 1 0 0)))

;(??,??)
(setq p21 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 0 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;(??,??)
(setq p22 '((0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 2 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 1 1 1 0 0 2 1 1 0 0 0 0 0 0 0 0 0)
	    (0 0 1 0 0 2 0 2 0 1 0 0 0 0 0 0 0 0 0)
	    (1 1 1 0 1 0 1 1 0 1 0 0 0 1 1 1 1 1 1)
	    (1 0 0 0 1 0 1 1 0 1 1 1 1 1 0 0 4 4 1)
	    (1 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 4 4 1)
	    (1 1 1 1 1 0 1 1 1 0 1 3 1 1 0 0 4 4 1)
	    (0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
	    (0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;TEST FUNCTIONS


;(format t "1. Goal Test NIL  = ~a ~%" (goal-test '((0 0 0 1)(0 2 0 1))))
;(format t "2. Goal Test T = ~a ~%" (goal-test 0))
;(format t "3. Goal Test NIL = ~a ~%" (goal-test p1))
;(format t "4. Goal Test NIL = ~a ~%" (goal-test p2))
;(format t "5. Goal Test NIL = ~a ~%" (goal-test p3))
;(format t "6. Goal Test NIL = ~a ~%" (goal-test p4))
;(format t "7. Goal Test NIL = ~a ~%" (goal-test p5))
;(format t "8. Goal Test NIL = ~a ~%" (goal-test p6))
;(format t "9. Goal Test NIL = ~a ~%" (goal-test p7))
;(format t "10. Goal Test NIL = ~a ~%" (goal-test p8))
;(format t "11. Goal Test NIL = ~a ~%" (goal-test p9))
;(format t "12. Goal Test NIL = ~a ~%" (goal-test p10))
;(format t "13. Goal Test NIL = ~a ~%" (goal-test p11))
;(format t "14. Goal Test NIL = ~a ~%" (goal-test p12))
;(format t "15. Goal Test NIL = ~a ~%" (goal-test p13))
;(format t "16. Goal Test NIL = ~a ~%" (goal-test p14))
;(format t "17. Goal Test NIL = ~a ~%" (goal-test p15))
;(format t "18. Goal Test NIL = ~a ~%" (goal-test p16))
;(format t "19. Goal Test NIL = ~a ~%" (goal-test p17))
;(format t "20. Goal Test NIL = ~a ~%" (goal-test p18))
;(format t "21. Goal Test T  = ~a ~%" (goal-test '((0 0 0 1)(0 1 0 1))))
;(format t "22. Check State 0  = ~a ~%" (check-value '((0 0 0 1)(0 1 0 1)) 0 0 0))
;(format t "23. Check State 1  = ~a ~%" (check-value '((0 0 0 1)(0 1 0 3)) 1 1 0))
;(format t "24. Check State 3  = ~a ~%" (check-value '((0 0 0 1)(0 1 0 3)) 1 3 0))
;(format t "25. Check State 0  = ~a ~%" (check-value '((0 0 0 1)(0 1 0 1)) 0 0 0))
;(format t "26. Check State 0  = ~a ~%" (check-value '((0 0 0 1)(0 1 0 1)) 0 0 0))
;(format t "27. Check State 0  = ~a ~%" (check-value '((0 0 0 1)(0 1 0 1)) 0 0 0))
;(format t "28. H1 1  = ~a ~%" (h1 '(0 0 2)))
;(format t "29. H1 2  = ~a ~%" (h1 '((0 0 2)(0 0 2))))
;(format t "30. H1 1  = ~a ~%" (h1 p1))
;(format t "31. H1 1  = ~a ~%" (h1 p2))
;(format t "32. H1 1  = ~a ~%" (h1 p3))
;(format t "33. H1 1  = ~a ~%" (h1 p4))
;(format t "34. H1 2  = ~a ~%" (h1 p5))
;(format t "35. H1 2  = ~a ~%" (h1 p6))
;(format t "36. H1 1  = ~a ~%" (h1 p7))
;(format t "37. H1 3  = ~a ~%" (h1 p8))
;(format t "38. H1 2  = ~a ~%" (h1 p9))
;(format t "39. H1 3  = ~a ~%" (h1 p10))

;(format t "x. move-type = ~a ~%" (move-type 2 2))

;(format t "40. Next States = ~a ~%" (next-states p1))
;(format t "41. Next States = ~a ~%" (next-states p2))
;(format t "42. Next States = ~a ~%" (next-states p3))
;(format t "43. Next States = ~a ~%" (next-states p4))
;(format t "44. Next States = ~a ~%" (next-states p5))
;(format t "45. Next States = ~a ~%" (next-states p6))
;(format t "46. Next States = ~a ~%" (next-states p7))
;(format t "47. Next States = ~a ~%" (next-states p8))
;(format t "48. Next States = ~a ~%" (next-states p9))
;(format t "49. Next States = ~a ~%" (next-states p10))
;(format t "50. Next States = ~a ~%" (next-states p11))
;(format t "51. Next States = ~a ~%" (next-states p12))
;(format t "52. Next States = ~a ~%" (next-states p13))
;(format t "53. Next States = ~a ~%" (next-states p14))
;(format t "54. Next States = ~a ~%" (next-states p15))
;(format t "55. Next States = ~a ~%" (next-states p16))
;(format t "56. Next States = ~a ~%" (next-states p17))
;(format t "57. Next States = ~a ~%" (next-states p18))
;(format t "58. Next States = ~a ~%" (next-states p19))
;(format t "59. Next States = ~a ~%" (next-states p20))
;(format t "60. Next States = ~a ~%" (next-states p21))
;(format t "61. Next States = ~a ~%" (next-states p22))


(format t "60. Next States = ~a ~%" (h704281781 p21))
(format t "61. Next States = ~a ~%" (h704281781 p22))