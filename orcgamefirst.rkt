#lang racket
;structs
(struct player (health agility strength) #:mutable)
(struct orc-world (player lom attack# target) #:mutable)

;monters struct
(struct monster (image [health #:mutable]))
(struct orc monster (club))
(struct hydra monster ())
(struct slime monster (sliminess))
(struct brigand monster ())

;player
(define MAX-HEALTH 35)
(define MAX-AGILITY 35)
(define MAX-STRENGTH 35)
(define INSTRUCTION-TEXT-SIZE 20)
(define ATTACK-COLOR "red")
(define MESSAGES-SIZE 5)
(define MESSAGE-COLOR "blue")


(define (player-update! setter selector mx)
 (lambda (player delta)
   (setter player (interval+ (selector player) delta mx))))

;functions interval+:
(define (interval+ a b MAX)
  (min MAX (max 0 (+ a b))))

;the functions to update the atributs from the player!
; healthy:
 (define player-health+ 
   (player-update! set-player-health! player-health MAX-HEALTH))

; agility:
 (define player-agility+
   (player-update! set-player-agility! player-agility MAX-AGILITY))

;strenght:
 (define player-strength+
   (player-update! set-player-strength! player-strength MAX-STRENGTH))

;functions which inicialize the game
(define (initialize-orc-world)
  (define player0 (initialize-player))
  (define lom0 (initialize-monsters))
  (orc-world player0 lom0 (random-number-of-attacks player0) 0))

(define (initialize-monsters)
 (build-list
 MONSTER# 
 (lambda (_)
 (define health (random+ MONSTER-HEALTH0))
 (case (random 4)
 [(0) (orc ORC-IMAGE health (random+ CLUB-STRENGTH))]
 [(1) (hydra HYDRA-IMAGE health)]
 [(2) (slime SLIME-IMAGE health (random+ SLIMINESS))]
 [(3) (brigand BRIGAND-IMAGE health)]))))

;end game.
(define (end-of-orc-battle? w)
 (or (win? w) (lose? w)))

;rendering game functions.
(define (render-orc-battle w);while de game happens acontece
 (render-orc-world w (orc-world-target w) (instructions w)))

(define (render-the-end w);when it finishes
 (render-orc-world w #f (message (if (lose? w) LOSE WIN))))

;function render-orc-world: main of rendering
(define (render-orc-world w t additional-text)
  (define i-player (render-player (orc-world-player w)))
  (define i-monster (render-monsters (orc-world-lom w) t))
  (above V-SPACER
         (beside H-SPACER
                 i-player
                 H-SPACER H-SPACER H-SPACER 
                 (above i-monster
                        V-SPACER V-SPACER V-SPACER 
                        additional-text)
                 H-SPACER)
         V-SPACER))

(define (instructions w);show the instructions. A definir instruction text.
 (define na (number->string (orc-world-attack# w)))
 (define ra (string-append REMAINING na))
 (define txt (text ra INSTRUCTION-TEXT-SIZE ATTACK-COLOR))
 (above txt INSTRUCTION-TEXT))

(define (message str)
 (text str MESSAGES-SIZE MESSAGE-COLOR))

;keyboard control
(define (player-acts-on-monsters w k)
  (cond 
    [(zero? (orc-world-attack# w)) (void)]
    [(key=? "s" k) (stab w)]
    [(key=? "h" k) (heal w)]
    [(key=? "f" k) (flail w)]
    [(key=? "e" k) (end-turn w)]
    [(key=? "n" k) (initialize-orc-world)]
    [(key=? "right" k) (move-target w +1)]
    [(key=? "left" k) (move-target w -1)]
    [(key=? "down" k) (move-target w (+ PER-ROW))]
    [(key=? "up" k) (move-target w (- PER-ROW))])
  (give-monster-turn-if-attack#=0 w)
  w)

;
(define (initialize-player) 
 (player MAX-HEALTH MAX-AGILITY MAX-STRENGTH))

;
(define (random-number-of-attacks p)
 (random-quotient (player-agility p) ATTACKS#))

;
(define (random-quotient x y)
 (define div (quotient x y))
 (if (> 0 div) 0 (random+ (add1 div))))

;
(define (random+ n)
 (add1 (random n)))

;-------


;main function --- BIG BANG ---

(define (start) 
  (big-bang (initialize-orc-world)
    (on-key player-acts-on-monsters) 
    (to-draw render-orc-battle)
    (stop-when end-of-orc-battle? render-the-end)))

;image

(define (instructions w)
 (define na (number->string (orc-world-attack# w)))
 (define ra (string-append REMAINING na))
 (define txt (text ra INSTRUCTION-TEXT-SIZE ATTACK-COLOR))
 (above txt INSTRUCTION-TEXT))
(define (message str)
 (text str MESSAGES-SIZE MESSAGE-COLOR))

;rendering section

(define (render-orc-world w t additional-text)
 (define i-player (render-player (orc-world-player w)))
 (define i-monster (render-monsters (orc-world-lom w) t))
 (above V-SPACER
        (beside H-SPACER
                i-player
                H-SPACER H-SPACER H-SPACER 
                (above i-monster
                       V-SPACER V-SPACER V-SPACER 
                       additional-text)
                H-SPACER)
        V-SPACER))

