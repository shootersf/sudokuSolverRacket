#lang racket

;; Takes a sudoku puzzle as one list with empty squares represented by 0.
;; Solves the puzzle via recursion and backtracking when it reaches a dead end.
;; While each helper function has a small description of it's own, in an effort
;; to keep things uniform, the following variable names (letters) will always
;; reference the same thing in the solver:
;; P - The entire sudoku puzzle in its current level of completion.
;; R - The remainder of the squares still to work on when stepping square by square
;;     through the puzzle.
;; S - The index of a square starting from 0.
;; N - A literal integer. Usually passed to fill in a square.
;; O - A list of potential options that could go into a specific square
;; B - The backtrack list. A type of stack made of lists of lists. Each sublist is one
;;     step back in the puzzle. 
;;
;; The solver finishes around line 185 and the rest of the code is a way to output the 
;; solution more elegantly.

;; To use: (solve puzzle)
;; Or better: (pretify (solve puzzle))
;; Or the same but shorter: (ps puzzle)

;; Initial test puzzles

(define puzzle 
	'(0 0 0 2 6 0 7 0 1
	6 8 0 0 7 0 0 9 0
	1 9 0 0 0 4 5 0 0
	8 2 0 1 0 0 0 4 0
	0 0 4 6 0 2 9 0 0
	0 5 0 0 0 3 0 2 8
	0 0 9 3 0 0 0 7 4
	0 4 0 0 5 0 0 3 6
	7 0 3 0 1 8 0 0 0)
)

(define hardPuzzle
    `(7 3 0 1 0 0 9 0 0
	0 0 4 0 0 0 2 3 0
	0 0 0 3 0 0 0 5 0
	9 0 0 0 0 0 3 0 0
	0 0 5 0 0 0 6 2 0
	0 0 0 0 0 0 0 8 4
	2 0 0 0 4 1 0 0 0
	8 0 0 0 9 0 0 0 0
	1 0 0 0 8 5 0 0 2)
)
	

;; The initial function to call. Requires the puzzle to solve and should output the solved puzzle

(define solve
	(lambda (P)
		(solveNextSquare P P 0 `() )
))

;; solveNextSquare helper function. Takes the current puzzle, the remainder of the puzzle to solve
;; and the current square number to work on, it also requires a backtrack list in case of dead ends.
;; If the remainder of the puzzle to be solved is empty, we have solved it and return the puzzle in
;; its final state.
;; Otherwise if the number in the current square is not 0 it is already solved and we move to the next
;; square.
;; Finally if it is 0. We will call a couple of helper functions to solve the square with options,
;; which in turn will recursively call this function for the next square.

(define solveNextSquare
	(lambda (P R S B)
		(cond
			[	(null? R)					P		]
			[	(not (eq? 0 (car R)))		(solveNextSquare P (cdr R) (+ S 1) B)		    ]
			[	#t							(solveSquareWithOptions P R S (getOptions P S) B) 	]
)))

;; solveSquareWithOptions. 
;; Similar to above but takes a list of options for an empty square.
;; If the options are an empty list the program has hit a dead end and backtracks one step.
;; Otherwise it fills the square with the first option and creates a new backtrack step with the current P R S and
;; the rest of the options in case of a dead end in the future and the current backtrack as its backtrack.
;; There is also a fail case where there are no options and the backtrack is empty. This indicates a failed
;; puzzle and will return an empty list. This shouldn't happen if the puzzle has a solution.

(define solveSquareWithOptions
	(lambda (P R S O B)
		(cond
			[ 	(and (null? O) (null? B))				`() ]
			[	(null? O)					(backtrack B)	]
			[ 	#t							(solveNextSquare (updatePuzzle P S (car O)) (cdr R) (+ S 1) (list P R S (cdr O) B))	]
)))

;; backtrack
;; Takes a stack of backtracks and applies solveSquareWithOptions to the first one passing in the
;; rest as the new backtrack. Essentially stepping back to that square but dropping the most recent option
;; as it led to a dead end. 

(define backtrack
	(lambda (B)
		(apply solveSquareWithOptions B)
))

;; updatePuzzle
;; Takes the current puzzle, the square to update and the number to put in that square
;; Recursively calls itself stepping through the puzzle until in reaches the square. Replacing it with N and adding on the rest

(define updatePuzzle
	(lambda (P S N)
		(cond
			[	(eq? 0 S)		(cons N (cdr P))									]
			[	#t				(cons (car P) (updatePuzzle (cdr P) (- S 1) N))		]
)))

;; getOptions
;; Takes the puzzle and a square index. Will return a list of all possible numbers that can currently be put in that square
;; Uses a recursive helper function to do this.

(define getOptions
	(lambda (P S)
		(getOptionsNextSquare P 0 S)
))

;; getOptionsNextSquare
;; Takes the remaining puzzle, the index of the current square to work on which to avoid confusion I will call C for cell,
;; and the index of the current square we are getting options for S.
;; The stopping case will be when there is no remaining puzzle and will return all possible numbers.
;; If the current cell contains a 0 then we don't need to compare as it doesn't reduce the options for S.
;; Otherwise if the current cell has the same row, column or group number we remove it as an options from all previous options.

(define getOptionsNextSquare
	(lambda (R C S)
		(cond
			[	(null? R)						`(1 2 3 4 5 6 7 8 9)	]
			[	(eq? 0 (car R))					(getOptionsNextSquare (cdr R) (+ C 1) S)	]
			[	(eq? (rowNum C) (rowNum S))		(remove (car R) (getOptionsNextSquare (cdr R) (+ C 1) S)) ]
			[	(eq? (colNum C) (colNum S))		(remove (car R) (getOptionsNextSquare (cdr R) (+ C 1) S)) ]
			[	(eq? (groupNum C) (groupNum S))	(remove (car R) (getOptionsNextSquare (cdr R) (+ C 1) S)) ] 
			[	#t								(getOptionsNextSquare (cdr R) (+ C 1) S)	]
)))


;; rowNum
;; Takes a square and calculates the row number by using integer division by 9.
;; This uses a helper function created called "//" which just floors the result of "/"

(define rowNum 
	(lambda (S)
		(// S 9)
))

;; colNum
;; Takes a square and calculates the col number by using mod 9

(define colNum
	(lambda (S)
		(modulo S 9)
))

;; groupNum
;; Takes a square and calculates its group number. This was a little more complex than the previous
;; Basically it gets a column group of 0, 1, or 2 and adds it to a row group of 0, 3, 6

(define groupNum
	(lambda (S)
	(+ (// (colNum S) 3) (* 3 (// (rowNum S) 3)))
))

;; remove
;; Takes a number N and removes it from a list L

(define remove
	(lambda (N L)
		(cond
			[	(null? L)										`()	]
			[	(eq? N (car L))								(cdr L)	]
			[	#t				(cons (car L) (remove N (cdr L))) 	]
)))

;; // Is a helper function that floors the result of a division /

(define //
	(lambda (A B)
		(floor (/ A B))
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- END OF SOLVER -- 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Everything below here is an attempt to output the solution to the puzzle
;; in a more elegant and familiar style.


;; pretify
;; Takes a puzzle P and prints it out in a prettier manner
;; Utilises a lot of helper functions.

(define pretify
	(lambda (P)
		(mapRowsToDisplay (insertHorizontals (insertVerticles (createRows P))))
))

;; createRows
;; Takes a puzzle P and creates rows based on keeping track of square index.

(define createRows
	(lambda (P)
		  (createRowsNextSquare P 0)
))

;; createRowsNextSquare
;; Recursive function. Takes the remainder of the puzzle as R, and the current square's index S
;; Returns an empty list if there are no elements
;; Creates a new sublist if the item is the last element in a row, with the element in the current square and a new line marker 
;; and cons that list to the list of rows. Otherwise it inserts the current element into the first position of the first row.

(define createRowsNextSquare
	(lambda (R S)
		(cond
			[ (null? R)				`()		]
			[ (eq? 8 (modulo S 9))	(cons (list (car R) "\n") (createRowsNextSquare (cdr R) (+ S 1)))	]
			[ #t					(insertIntoFirstSubList (car R) (createRowsNextSquare (cdr R) (+ S 1))) ]
)))

;; insertIntoFirstSubList
;; Takes an element or number E and puts it at the front of the first sublist of the list L.

(define insertIntoFirstSubList
	(lambda (E L)
		(cons (cons E (car L)) (cdr L))
))

;; insertVerticles
;; uses a helper function to put vertical bars into rows R starting at square 0 in each row.

(define insertVerticles
	(lambda (R)
		(cond
			[ (null? R)							`()	]
			[ #t		(cons (insertVerticlesInRow (car R) 0)	(insertVerticles (cdr R)))	]
)))

;; insertVerticlesInRow
;; Takes a Row R and a square index S and simply inserts "|" before any square whose index is divisible by 3

(define insertVerticlesInRow 
	(lambda (R S)
		(cond
			[ (null? R)							`()	]
			[ (eq? 0 (modulo S 3))		(append (list "|" (car R)) (insertVerticlesInRow (cdr R) (+ S 1)))	]
			[ #t						(cons (car R)	(insertVerticlesInRow (cdr R) (+ S 1)))			]
)))

;; insertHorizontals
;; Takes a list of rows and recursively steps through them. If the remaining rows can be evenly divided by 3 it puts a 
;; horizontal bar above them. It also adds a bar at the bottom by using the empty list stopping condition.

(define insertHorizontals
	(lambda (R)
		(define hbar `(------------- "\n" ))
		(cond
			[ (null? R)							(list hbar)	]
			[ (eq? 0 (modulo (length R) 3))      (append (list hbar (car R) ) (insertHorizontals (cdr R)))	]
			[  #t								(cons (car R) (insertHorizontals (cdr R)))	]
)))

;; mapRowsToDisplay
;; Takes a list of rows R and maps each one to diplay one at a time. 


(define mapRowsToDisplay
	(lambda (R)
		(map display (car R) )
		(cond
			[ (null? (cdr R))	"Q.E.D"	]
			[ #t		(mapRowsToDisplay (cdr R))	]
)))


;;;;; FINAL ADDITION ;;;;;;;;;
;; shorthand for (pretify (solve x))

(define ps
	(lambda (P)
		(pretify (solve P))
))
	