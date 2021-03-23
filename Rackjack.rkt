#lang Racket
(define faces '(2 3 4 5 6 7 8 9 10 J Q K A))
(define suits '(♠ ♥ ♦ ♣))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   Function:  make-deck - generate a list contained 52 poker cards
;   Returns:   a list contained 52 poker cards
;   Example:   (make-deck) returns ((2 . ♠) (2 . ♥) ... (A . ♦) (A . ♣))
(define make-deck 
  (lambda ()
    ;   Function:  make-card - generate the poker card represented by face and suit
    ;   Parameter: face      - the face of card
    ;              suit      - the suit of card
    ;   Returns:   the poker card represented by face and suit
    ;   Example:   (make-card 2 ♦) retruns '(2 . ♦)
    (letrec ([make-card
              (lambda (face suit)
                (cons face suit))])            
      ; END OF LETREC
      (for*/list ([i faces]
                  [j suits])
        (make-card i j)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   Function:  eval-hand  - evaluate the list of poker cards
;   Parameter: playerhand - a list of poker cards
;   Returns:   the value of the list of poker cards
;   Example:   (eval-hand '((A . ♦) (9 . ♥) (A . ♣))) returns 21
(define eval-hand
  (lambda (playerhand)
    ;   Function:  eval-hand-pre  - before evaluate the list of poker cards, do a pre-evaluation, which is
    ;                               kind of statistic of the hand
    ;   Parameter: playerhand     - a list of poker cards
    ;   Returns:   the pair for evaluating the list of poker cards, car part represent the base value,
    ;              cdr part represent the number of Aces
    ;   Example:   (eval-hand-pre '((A . ♦) (9 . ♥) (A . ♣))) returns '(11 . 2)
    (letrec ([eval-hand-pre
              (lambda (playerhand)
                ; Base case: return '(0 . 0) if playerhand is null
                (if (null? playerhand)
                    (cons 0 0)
                    ; Recursive case: add the first card to the statistic. add base value to car, if it is A,
                    ;                 add 1 to cdr
                    (let                        
                        ; letting variables, for making code more readable and improve performance
                        ([head (caar playerhand)]
                         [tail (eval-hand-pre (cdr playerhand))])
                      (cond
                        ; Ace case: add 1 to car and cdr
                        [(eqv? 'A head) (cons (+ (car tail) 1) (+ (cdr tail) 1))]
                        ; Number case: add value to car
                        [(number? head) (cons (+ (car tail) head) (cdr tail))]
                        ; Letter case: add value, which is 10, to car
                        [else (cons (+ (car tail) 10) (cdr tail))]))))])
      ; END OF LETREC
      ; letting variables, for making code more readable and improve performance
      (let ([hand-stt (eval-hand-pre playerhand)])
        ;Doing final calculation for determine the best value the hand should be
        (if (or (> (car hand-stt) 11) (= (cdr hand-stt) 0))            
            ; IF   the base value is > 11 or there is no Ace in hand, then the best value is the base value
            (car hand-stt)
            ; ELSE the base value added 10 is the best value 
            (+ (car hand-stt) 10))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   Function:    deal! - deal two card from deck as return
;   Parameter:   deck  - a list of cards
;   Returns:     a list contain two cards, which are top two card of deck
;   Side effect: remove two card from top of deck
;   Example:     (deal! (make-deck)) return ((2 . ♠)(2 . ♥)) 
(define-syntax-rule
  (deal! deck)
  (let ([return (take deck 2)])
    (begin (set! deck (drop deck 2))
           return)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   Function:    hit! - deal one card from deck to hand
;   Parameter:   deck - a list of cards
;                hand - a list contain the cards of hand from player or dealer
;   Returns:     not intereted on return
;   Side effect: remove a card from top of deck to hand 
(define-syntax-rule
  (hit! deck hand)
  (begin (set! hand (append hand (list (car deck))))
         (set! deck (cdr deck))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   Function:    show-hand   - show the hand in certain way
;   Parameter:   hand        - a list contain the cards of hand from player or dealer
;                how         - should be 'Part or 'Full, anything other than 'Full will be consider as 'Part
;                description - the string which will displayed before the cards
;   Returns:     not intereted on return
;   Side effect: display the hand, if 'Full, display all as normal, else, the first card will be *****
(define show-hand
  (lambda (hand how description)
    ;   Function:    display-lst - display the item in the lst line by line
    ;   Parameter:   lst         - a list contain diplayable item 
    ;   Returns:     not intereted on return
    ;   Side effect: display the lst
    (letrec ([display-lst
              (lambda (lst)
                (when (not (null? lst))
                  (begin (printf "~a\n" (car lst))
                         (display-lst (cdr lst)))))])
      ; END OF LETREC
      ; display the description first
      (begin (printf "~a\n" description)
             (if (eqv? how 'Full)
                 (display-lst hand)
                 (begin (display "*****\n")
                        (display-lst (cdr hand))))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   Function:    main   - the Balckjack game
;   Returns:     not intereted on return
;   Side effect: display text that let player interact with
(define main
  (lambda ()
    (letrec ([gamedeck (shuffle (make-deck))]
             [playerhand (deal! gamedeck)]
             [dealerhand (deal! gamedeck)]
             ;   Function:    get-answer - get answer from user, should be h or s, else will be asked again
             ;   Returns:     h or s
             [get-answer
              (lambda ()
                (begin
                  (display "Hit or Stand? (Enter \"h\" or \"s\")\n")
                  (let ([answer (read-line)])
                    ; Valid case:
                    (if (or (equal? answer "h") (equal? answer "s"))
                        answer
                        ; Invalid case: ask input again 
                        (begin
                          (display "ERROR: Invalid input. Please enter again.\n")
                          (get-answer))))))]
             ;   Function:    player-turn - keep asking hit or stand untill stand or Burst,
             ;                modified playerhand and gamedeck if player hit
             ;   Returns:     0 if player Burst, else no return
             ;   Side effect: some card from gamedeck might be hit to playerhand 
             [player-turn
              (lambda ()
                ; Hit case: there is no Stand case, Stand case will just end this funciton without return
                (when (equal? "h" (get-answer))
                  (begin
                    (hit! gamedeck playerhand)
                    ; Burst case:
                    (if (> (eval-hand playerhand) 21)
                        0
                        ; Not Burst case: also the recursive case
                        (begin
                          (show-hand playerhand 'Full "Player's hand:")
                          (player-turn))))))]
             ;   Function:    dealer-turn - determin what dealer will do
             ;   Returns:     0 if dealer Burst, else no return
             ;   Side effect: some card from gamedeck might be hit to dealerhand 
             [dealer-turn
              (lambda ()
                ; Hit case: there is no Stand case, Stand case will just end this funciton without return
                (when (< (eval-hand dealerhand) 17)
                    (begin
                      (hit! gamedeck dealerhand)
                      ; Burst case:
                      (if (> (eval-hand dealerhand) 21)
                          0
                          ; Not Burst case: also the recursive case
                          (dealer-turn)))))]
             ;   Function:    end-game - display the end message of each game
             ;   Parameter:   result   - should be -1, 0 or 1, repersent player loses, draw or wins
             ;   Returns:     not intereted on return
             ;   Side effect: display the ending message of each game
             [end-game
              (lambda (result)
                (begin
                  (display "===================\nGame Result:\n")
                  (printf "\nDealer Score: ~a\n" (eval-hand dealerhand))
                  (show-hand dealerhand 'Full "Dealer's hand:")
                  (printf "\nPlayer Score: ~a\n" (eval-hand playerhand))
                  (show-hand playerhand 'Full "Player's hand:")
                  (cond 
                      [(= result 1)(display "\n=  Player Wins!   =\n")]
                      [(= result -1)(display "\n=  Player Loses!  =\n")]
                      [else (display "\n=      Draw!      =\n")])
                  (display "===================\n")))])
      ; END OF LETREC
      (begin
        ; display stating message and hand situation
        (display "===================\n= Rackjack Start! =\n===================\n")
        (show-hand dealerhand 'Part "Dealer's hand:")
        (show-hand playerhand 'Full "Player's hand:")
        (if (eqv? (player-turn) 0)
            ; Burst case:
            (begin
              (display "Player Busts!\n")
              ; Display lose message
              (end-game -1))
            ; Start Dealer's turn
            (begin
              (display "Dealer's turn\n")
              (if (eqv? (dealer-turn) 0)
                  ; Burst case:
                  (begin
                    (display "Dealer Busts!\n")
                    ; Display win message
                    (end-game 1))
                  ; No one Burst case: compare scores, and display coresponding message
                  (let ([player-score (eval-hand playerhand)]
                        [dealer-score (eval-hand dealerhand)])
                      (cond [(> player-score dealer-score) (end-game 1)]
                            [(< player-score dealer-score) (end-game -1)]
                            [else (end-game 0)])))))
        ; determine continue or not
        (display "Play again? y/n\n")
        (if (equal? "y" (read-line))
            (main)
            (display "===================\n=    Game Over    =\n===================\n"))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Run main
(main)
