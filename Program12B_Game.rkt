;Moumita Laskar

;Global list
(define MVLGame ())


;;MVLGame function will initialize the list
;; the list contains of 3 parameters
;; Matrix for the game, player status and a column stack
(define (MVLStartGame)
  (begin
    (set! MVLGame (MVLInsertColStack (MVLInsertBoard ())))
    (display "Life is a mysterious game")
    (newline)
    #t
  )
)

;functions to set the MVLGame list
; the 1st player is always X and starts with the colour Black(marked with B moves) in the game
(define (MVLInsertBoard lst)
  (if (null? lst)
      (cons (cons '((0 0 0 0 0 0 0) (0 0 0 0 0 0 0) (0 0 0 0 0 0 0) (0 0 0 0 0 0 0) (0 0 0 0 0 0 0) (0 0 0 0 0 0 0)) "X") ())
      #t
  )
)

;;ColStack function keets track for the filled row for a particular column.
;; Initially all the values of the column is 0, and is increments as we drop the coin in the column 
(define (MVLInsertColStack lst)
    (if (null? lst)
        (cons '("ColStack" 0 0 0 0 0 0 0) ())
        (cons (car lst) (MVLInsertColStack (cdr lst)))
    )
)


;#################################### GETTERS ##########################################################



;get colstack list
(define (MVLGetColStack)
  (cdr (car (cdr MVLGame)))
)

;get boardGame 
(define (MVLGetMatrix)
  (car (car MVLGame))
)

;get current user
(define (MVLGetUser)
  (cdr (car MVLGame))
)



;get the row of a given column
(define (MVLGetRow lst pos)
  (cond
     ((null? lst) ())
     ((= 1 pos) (car lst))
     (#t (MVLGetRow (cdr lst) (- pos 1)))
   )
)

; MVLGetCell function returns the value for a particular row and column
(define (MVLGetCell Matrix Row Column)
  (if (null? Matrix)
      ()
      (MVLGetCol (MVLGetCol Matrix Row) Column)
  )
)

; helper function for MVLGetCell
(define (MVLGetCol lst pos)
  (cond  
    ((null? lst) ())
    ((= 1 pos) (car lst))
    (#t (MVLGetCol (cdr lst) (- pos 1)))
  )
)



;######################################### Setters ###############################################

;;MVLSetCell updates the Matrix for a particular row and column with the Item
;;This function is used in MVLMarkMove.
(define (MVLSetCell Matrix Row Col Item)
   (if (null? Matrix)
       ()
       (if (= 1 Row)
           (cons (MVLModifyRow (car Matrix) Col Item) (MVLSetCell (cdr Matrix) (- Row 1) Col Item))
           (cons (car Matrix) (MVLSetCell (cdr Matrix) (- Row 1) Col Item))
       )
   )
)

;helper for MVLSetCell
(define (MVLModifyRow lst col item)
   (if (null? lst)
        ()
        (if (= 1 col)
            (cons item (MVLModifyRow (cdr lst) (- col 1) item))
            (cons (car lst) (MVLModifyRow (cdr lst) (- col 1) item))
        )
    )
)

     
;;MVLUpdateColStack increments the row status for the column.
;; This function is used to get the row for the next move in a particular column
(define (MVLUpdateColStack lst col)
  (if (null? lst)
      ()
      (if(equal? (car (car lst)) "ColStack")
         (cons (MVLIncrRow (car lst) (+ col 1)) (MVLUpdateColStack (cdr lst) col))
         (cons (car lst) (MVLUpdateColStack (cdr lst) col))
      )
   )
)

; helper function for MVLUpdateColStack
; creates a new list with the incremented value 
(define (MVLIncrRow lst pos)
    (if (null? lst)
        ()
        (if (= 1 pos)
            (cons (MVLincr lst) (MVLIncrRow (cdr lst) (- pos 1)))
            (cons (car lst) (MVLIncrRow (cdr lst) (- pos 1)))
         )
    )
)

; helper for MVLUpdateColStack
(define (MVLincr  lst)
  (+ (car lst) 1)
)


; MVLUpdatePlayer updates the list with user who will make the next move.
(define (MVLUpdatePlayer lst usr)
   (if (null? lst)
       ()
       (if (equal? (cdr (car lst)) usr) 
            (cons (MVLModPair (car lst) usr) (MVLUpdatePlayer (cdr lst) usr)) 
            (cons (car lst) (MVLUpdatePlayer (cdr lst) usr))
        )
    )
)

;helper function for MVLUpdatePlayer. Its creating a new list with player status updated
(define (MVLModPair Pair usr)
   (if (equal? usr "X")
       (cons (car Pair) "Y")
       (cons (car Pair) "X")
   )        
)






;;MVLUpdateMatrix will update a particular cell with the coin dropped.
;; User X has colour Black and User Y has colour Yellow
(define (MVLUpdateMatrix lst row col)
    (if (null? lst)
        ()
        (if (equal? (cdr (car lst)) "X")
            (cons (cons (MVLSetCell (car (car lst)) (- 6 row) col "B") (MVLGetUser)) (MVLUpdateMatrix (cdr lst) row col))
            (if (equal? (cdr (car lst)) "Y")
                (cons (cons (MVLSetCell (car (car lst)) (- 6 row) col "Y") (MVLGetUser)) (MVLUpdateMatrix (cdr lst) row col))
                (cons (car lst) (MVLUpdateMatrix (cdr lst) row col))
            )
        )
    )
)




;################################### Begining of MAIN SUBROUTINE #############################################

;; MVLMarkMove
;; Input : column on which the move was made
;; Output: updated column (same as input)
;; This function updated the Matrix, player status and ColStack for a particular move

(define (MVLMarkMove col)
    (begin
       (set! MVLGame (MVLUpdatePlayer (MVLUpdateColStack (MVLUpdateMatrix MVLGame (MVLGetRow  (MVLGetColStack) col) col) col) (MVLGetUser)))
        col
    )
)


;; MVLLegalMoveP
;; Input : column to check for legal move
;; Output: True or False
;; This function returns true if a move is legal for a columm
;;(legal means the column is not filled up and player still has the chnace to drop the coin).
;; If its not legal, it returns false

(define (MVLLegalMoveP col)
  (if (or (> col 7) (< col 1))
      #f
      (if (> (MVLGetRow  (MVLGetColStack) col) 5)
         #f    ;if the row is full, return false i.e it is NOT a legal move
         #t
      )
  )
)

;; MVLMakeMove
;; Input : void
;; Output: Column on which the coin was dropped in the last move by the Computer
;; This function call the game program to choose a best move.

(define (MVLMakeMove)
    (MVLAIChooseMove)
)

;; MVLWinP
;; Input : Column
;; Output: True or False
;; This function checks if the last move for a given column resulted in a Win

(define (MVLWinP col)
  (if (> (MVLGetMaxSearchRes col) 3)
      #t   ; return true if the max search result > 3 i.e 4 or greater; we got the winner 
      #f
  )
)


;; MVLWillWinP
;; Input : column
;; Output: True or False
;; This function checkes, if a move in the column will result in a win for the Computer
;; It checks the user and searches the win state for that particular user, from the column given

(define (MVLWillWinP col)
  (if (equal? (MVLGetUser) "X")
        (if (> (MVLCanWinGame col "B" 2) 0) 
            #t
            #f
        )
        (if (equal? (MVLGetUser) "Y")
            (if (> (MVLCanWinGame col "Y" 2) 0)
                #t
                #f
            )
        )
   )
)


;; MVLShowGame
;; Input : None
;; output: None
;; This function displays the Matrix or the Board Game 
(define (MVLShowGame)
   (newline)
   (MVLShowGameX (MVLGetMatrix))
)


;#################################### HELPER FUNCTIONS ######################################

; MVLGenerateRand
; This function generates a random number,
; if the generated column is not legal, it will generate random number again
; this process continues till valid column is selected or the game is over

(define (MVLGenerateRand)
    (MVLGetLegalColX (MVLGenerateRandX))
)

; MVLGetLegalColX
; helper function for MVLGenerateRand
; This function returns the col, if its a legal move, else tries to generate another random value      
(define (MVLGetLegalColX col)
    (if (MVLLegalMoveP col) 
        col
        (MVLGenerateRand)
    )
)

; helper function for MVLGenerateRand
(define (MVLGenerateRandX)
   (+ 1 (random 7))
)

; helper function for MVLShowGame
(define (MVLShowGameX lst)
   (if (null? lst)
      ()
      (begin
        (display (car lst))
        (newline)
        (MVLShowGameX (cdr lst))
        #t
      )
  )
)

;; ################################### SEARCH LOGIC ######################################

;; MVLVertSearch
;; This functions returns the number of coins of the same colour placed together vertically 
(define (MVLVertSearch Matrix row col)
    (if (equal? (MVLGetCell Matrix row col) "B")
        (MVLVertSearchX Matrix row col "B")
        (if (equal? (MVLGetCell Matrix row col) "Y")
           (MVLVertSearchX Matrix row col "Y")
            0
        )
    )
)

;helper for Vertical Search
(define (MVLVertSearchX Matrix row col usr)
  (cond
    ((or (< row 0) (> row 6)) 0)
    ((equal? (MVLGetCell Matrix row col) 0) 0)
    ((not (equal? (MVLGetCell Matrix row col) usr)) 0)
    ((equal? (MVLGetCell Matrix row col) usr) (+ 1 (MVLVertSearchX Matrix (+ 1 row) col usr)))
  )
)

; MVLHorizonal Search
; Returns the total number of same colour coin together placed horizonally
(define (MVLHorizonalSearch Matrix row col)
    (if (equal? (MVLGetCell Matrix row col) "B")
        ( + (MVLHorizontalLSearch Matrix row col "B") (MVLHorizontalRSearch Matrix row (+ 1 col) "B"))
        (if (equal? (MVLGetCell Matrix row col) "Y")
           (+ (MVLHorizontalLSearch Matrix row col "Y") (MVLHorizontalRSearch Matrix row (+ 1 col) "Y"))
           0
        )
    )
)

;helper for MVLHorizonal Search
;This function searches the left side of the given column for the count

(define (MVLHorizontalLSearch Matrix row col usr)
    (cond
       ((or (< col 1) (> col 7)) 0)
       ((or (< row 1) (> row 6)) 0)
       ((equal? (MVLGetCell Matrix row col) 0) 0)
       ((not (equal? (MVLGetCell Matrix row col) usr)) 0)
       ((equal? (MVLGetCell Matrix row col) usr) (+ 1 (MVLHorizontalLSearch Matrix row (- col 1) usr)))
    )
)

;helper for MVLHorizonal Search
;This function searches the right side of the given column for the count

(define (MVLHorizontalRSearch Matrix row col usr)
    (cond
       ((or (< col 1) (> col 7)) 0)
       ((or (< row 1) (> row 6)) 0)
       ((equal? (MVLGetCell Matrix row col) 0) 0)
       ((not (equal? (MVLGetCell Matrix row col) usr)) 0)
       ((equal? (MVLGetCell Matrix row col) usr) (+ 1 (MVLHorizontalRSearch Matrix row (+ col 1) usr)))
    )
)

;MVLRtDiagonalSearch
;Right diagonal search, this will count the same type of coloured coin in the upward right diagonal and downward left diagonal

(define (MVLRtDiagonalSearch Matrix row col)
    (if (equal? (MVLGetCell Matrix row col) "B")
        ( + (MVLRtPosDiagonal Matrix row col "B") (MVLLtNegDiagonal Matrix (+ 1 row) (- col 1) "B")) 
        (if (equal? (MVLGetCell Matrix row col) "Y")
            (+ (MVLRtPosDiagonal Matrix row col "Y") (MVLLtNegDiagonal Matrix (+ 1 row) (- col 1) "Y"))
            0
        )
    )
)

;helper for MVLRtDiagonalSearch
; This function searches the right upward(positive) diagonal

(define (MVLRtPosDiagonal Matrix row col usr)
  (cond
      ((or (< col 1) (> col 7)) 0)
      ((or (< row 1) (> row 6)) 0)
      ((equal? (MVLGetCell Matrix row col) 0) 0)
      ((not (equal? (MVLGetCell Matrix row col) usr)) 0)
      ((equal? (MVLGetCell Matrix row col) usr) (+ 1 (MVLRtPosDiagonal Matrix (- row 1) (+ col 1) usr)))
  )
)




; helper for MVLRtDiagonalSearch
; This function searches the left downward(negative) diagonal

(define (MVLLtNegDiagonal Matrix row col usr)
 (cond
      ((or (< col 1) (> col 7)) 0)
      ((or (< row 1) (> row 6)) 0)
      ((equal? (MVLGetCell Matrix row col) 0) 0)
      ((not (equal? (MVLGetCell Matrix row col) usr)) 0)
      ((equal? (MVLGetCell Matrix row col) usr) (+ 1 (MVLLtNegDiagonal Matrix (+ row 1) (- col 1) usr)))
  )
)



;MVLLeftDiagonalSearch
;Left diagonal search, this will count the same type of coloured coin in upward left diagonal and downward right diagonal

(define (MVLLeftDiagonalSearch Matrix row col)
    (if (equal? (MVLGetCell Matrix row col) "B")
        ( + (MVLLtPosDiagonal Matrix row col "B") (MVLRtNegDiagonal Matrix (+ 1 row) (+ col 1) "B")) ;; incrementing the row and dec the col 'coz the value of col is already added in 1st part
        (if (equal? (MVLGetCell Matrix row col) "Y")
            (+ (MVLLtPosDiagonal Matrix row col "Y") (MVLRtNegDiagonal Matrix (+ 1 row) (+ col 1) "Y"))
            0
        )
    )
)

;helper for MVLLeftDiagonalSearch
;This function searches the left upward(positive) diagonal

(define (MVLLtPosDiagonal Matrix row col usr)
  (cond
      ((or (< col 1) (> col 7)) 0)
      ((or (< row 1) (> row 6)) 0)
      ((equal? (MVLGetCell Matrix row col) 0) 0)
      ((not (equal? (MVLGetCell Matrix row col) usr)) 0)
      ((equal? (MVLGetCell Matrix row col) usr) (+ 1 (MVLLtPosDiagonal Matrix (- row 1) (- col 1) usr)))
  )
)




;helper for MVLLeftDiagonalSearch
;This function searches the right negative(downward) diagonal

(define (MVLRtNegDiagonal Matrix row col usr)
 (cond
      ((or (< col 1) (> col 7)) 0)
      ((or (< row 1) (> row 6)) 0)
      ((equal? (MVLGetCell Matrix row col) 0) 0)
      ((not (equal? (MVLGetCell Matrix row col) usr)) 0)
      ((equal? (MVLGetCell Matrix row col) usr) (+ 1 (MVLRtNegDiagonal Matrix (+ row 1) (+ col 1) usr)))
  )
)



;MVLGetMaxSearchRes
;This function returns the maximum number of same coloured coin together, for a given column

(define (MVLGetMaxSearchRes col)
  (MVLFindMax
        (MVLFindMax
            (MVLVertSearch (MVLGetMatrix) (- 7 (MVLGetRow  (MVLGetColStack) col)) col)
            (MVLHorizonalSearch (MVLGetMatrix) (- 7 (MVLGetRow  (MVLGetColStack) col)) col)
        )
        (MVLFindMax
            (MVLRtDiagonalSearch (MVLGetMatrix) (- 7 (MVLGetRow  (MVLGetColStack) col)) col)
            (MVLLeftDiagonalSearch (MVLGetMatrix) (- 7 (MVLGetRow  (MVLGetColStack) col)) col)
        )

   )
)

;helper for MVLGetMaxSearchRes

(define (MVLFindMax a b)
  
  (if(> a b)
        a
        b
  )
)


;MVLCanWinGame
;This function will check that if any given move(particular column) will result in a win for the usr,
;Returns column, if the number of adjacent coins of same colour is greater than maxVal, else returns 0
;This function is a helper function for Computer's next move, MVLWillWinP and MVLcheckWinMove, MVLCanWinGameX

(define (MVLCanWinGame col usr maxVal)
    (if (and
            (equal? (MVLGetCell (MVLGetMatrix) (- (- 7 (MVLGetRow  (MVLGetColStack) col)) 1) col) 0) ; if the cell is empty
            (>
                (MVLFindMax
                    (MVLFindMax
                        (+ (MVLHorizontalLSearch (MVLGetMatrix) (- 6 (MVLGetRow  (MVLGetColStack) col)) (- col 1) usr) (MVLHorizontalRSearch (MVLGetMatrix) (- 6 (MVLGetRow  (MVLGetColStack) col)) (+ 1 col) usr))
                        (MVLVertSearchX (MVLGetMatrix) (- 7 (MVLGetRow  (MVLGetColStack) col) ) col usr)
                     )
                     (MVLFindMax
                         (+ (MVLLtPosDiagonal (MVLGetMatrix) (- (- 6 (MVLGetRow  (MVLGetColStack) col)) 1) (- col 1) usr) (MVLRtNegDiagonal (MVLGetMatrix) (- 7 (MVLGetRow  (MVLGetColStack) col)) (+ col 1) usr))
                         (+ (MVLRtPosDiagonal (MVLGetMatrix) (- (- 6 (MVLGetRow  (MVLGetColStack) col)) 1) (+ col 1) usr) (MVLLtNegDiagonal (MVLGetMatrix) (- 7 (MVLGetRow  (MVLGetColStack) col)) (- col 1) usr))
                     )
                 )
              maxVal
            )
        )
        col
        0
    )
)



;MVLcheckWinMove
;This function searches all the column for the best possible move
;Best possible move is, if for a given move, if the adjacent same coloured coin is greater than maxVal or not
; This function internally calls MVLCanWinGame

(define (MVLcheckWinMove maxVal)
   (if (equal? (MVLGetUser) "X")  
       (MVLcheckGameOverMove '(4 5 3 2 6 1 7) "B" maxVal) ;MOUMITA
       (MVLcheckGameOverMove '(4 5 3 2 6 1 7) "Y" maxVal)
   )
)

(define (MVLcheckOponentWinMove maxVal)
   (if (equal? (MVLGetUser) "X")   
       (MVLcheckGameOverMove '(4 5 3 2 6 1 7) "Y" maxVal)
       (MVLcheckGameOverMove '(4 5 3 2 6 1 7) "B" maxVal)
   )
)

;MVLcheckGameOverMove
;Retuns column for the best move, else returns null

(define (MVLcheckGameOverMove lst usr maxVal)
   (if (null? lst)
        0
        (if (MVLLegalMoveP (car lst))
            (if (> (MVLCanWinGame (car lst) usr maxVal) 0)
                (car lst)
                (MVLcheckGameOverMove (cdr lst) usr maxVal)
            )
            (MVLcheckGameOverMove (cdr lst) usr maxVal)
        )
    )
)

;MVLCanWinGameX
;This functions checks if there is atleast 2 adjacent coin of the same colour
; if so, returns the coin, else returns 0

(define (MVLCanWinGameX col)
  (if (or (< col 1) (> col 7)) 
      -1
      (if (equal? (MVLGetUser) "X") 
         (MVLCanWinGame col "Y" 1)
         (MVLCanWinGame col "B" 1)
      )
   )
)





;MVLOpWin
;This function checkes, if Computer's move will facilitate the oponent to make a winning move

(define (MVLOpWin col)
    (if (equal? (MVLGetUser) "X")
      (MVLOpWinX col "Y")
      (MVLOpWinX col "B")
    )
)

;helper function for MVLOpWin

(define (MVLOpWinX col usr)
  (if (>
          (MVLFindMax
              (+
                   (MVLHorizontalLSearch (MVLGetMatrix) (- (- 6 (MVLGetRow  (MVLGetColStack) col)) 1) (- col 1) usr)
                   (MVLHorizontalRSearch (MVLGetMatrix) (- (- 6 (MVLGetRow  (MVLGetColStack) col)) 1) (+ 1 col) usr)
              )
              (MVLFindMax
                   (+
                        (MVLLtPosDiagonal (MVLGetMatrix) (- (- 6 (MVLGetRow  (MVLGetColStack) col)) 2) (- col 1) usr) ; changed row 1 to 2
                        (MVLRtNegDiagonal (MVLGetMatrix) (- 6 (MVLGetRow  (MVLGetColStack) col)) (+ col 1) usr)
                   )
                   (+
                        (MVLRtPosDiagonal (MVLGetMatrix) (- (- 6 (MVLGetRow  (MVLGetColStack) col)) 2) (+ col 1) usr) ; changed row 1 to 2
                        (MVLLtNegDiagonal (MVLGetMatrix) (- 6 (MVLGetRow  (MVLGetColStack) col)) (- col 1) usr)
                   )
               )
           )
           2
        )
        0
        col
   )
)
 

; MVLGameOver
;Checks if a game is over or not
;Its checks the colstack, i,e every column and returns true if the all the values in the columns is greater or equal to 6 i.e filled

(define (MVLIsGameOver)
  (if (MVLGameOverX (MVLGetColStack))
      #t
      #f
  )
)

;helper for MVLIsGameOver

(define (MVLGameOverX lst)
    (if (null? lst)
        #t          ; list is null nut we didnt return => all values in colstack is 6
        (if (> (car lst) 5)       ; if the car lst = 6, then try the next one else return #f3
            (MVLGameOverX (cdr lst))
            #f
        )
    )
)
       
   
;Computer will choose the moves from the below logic
(define (MVLAIChooseMove)
  (cond
    ((> (MVLcheckWinMove 2) 0) (MVLMarkMove (MVLcheckWinMove 2)))
    ((> (MVLcheckOponentWinMove 2) 0) (MVLMarkMove (MVLcheckOponentWinMove 2)))
     
    ((MVLLegalMoveP (MVLCanWinGameX (MVLOpWin 4))) (MVLMarkMove 4))
    ((MVLLegalMoveP (MVLCanWinGameX (MVLOpWin 5))) (MVLMarkMove 5))
    ((MVLLegalMoveP (MVLCanWinGameX (MVLOpWin 3))) (MVLMarkMove 3))
    ((MVLLegalMoveP (MVLCanWinGameX (MVLOpWin 2))) (MVLMarkMove 2))
    ((MVLLegalMoveP (MVLCanWinGameX (MVLOpWin 6))) (MVLMarkMove 6))
    ((MVLLegalMoveP (MVLCanWinGameX (MVLOpWin 7))) (MVLMarkMove 7))
    ((MVLLegalMoveP (MVLCanWinGameX (MVLOpWin 1))) (MVLMarkMove 1))
                    
       
    ((MVLLegalMoveP (MVLOpWin 4)) (MVLMarkMove 4))
    ((MVLLegalMoveP (MVLOpWin 5)) (MVLMarkMove 5))
    ((MVLLegalMoveP (MVLOpWin 3)) (MVLMarkMove 3))
    ((MVLLegalMoveP (MVLOpWin 6)) (MVLMarkMove 6))
    ((MVLLegalMoveP (MVLOpWin 2)) (MVLMarkMove 2))
    ((MVLLegalMoveP (MVLOpWin 1)) (MVLMarkMove 1))
    ((MVLLegalMoveP (MVLOpWin 7)) (MVLMarkMove 7))

    ( #t (MVLMarkMove (MVLGenerateRand)))
  
  )
)




;################################ END ##########################################