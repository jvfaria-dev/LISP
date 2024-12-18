#lang racket
(require 2htdp/universe 2htdp/image)

;structs
(struct player (health agility strength) #:mutable)
(struct orc-world (player lom attack# target) #:mutable)

;monster struct
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
(define V-SPACER (rectangle 0 10 "solid" "white"))
(define H-SPACER (rectangle 10 0 "solid" "white"))
(define STRENGTH-COLOR "purple")
(define AGILITY-COLOR "green")
(define HEALTH-COLOR "red")
(define HEALTH-BAR-WIDTH 8)
(define HEALTH-BAR-HEIGHT 2)
(define HEALTH-SIZE 4)
(define PER-ROW 3)
(define HEALING 8)
(define MONSTER# 6)
(define MONSTER-HEALTH 20)
(define CLUB-STRENGTH 12)
(define SLIMINESS 5)


(define LOSE (text 7 "red"))
(define WIN (text 7 "green"))

;image
(define STRENGTH (text "STRENGTH" 5 "blue"))
(define AGILITY (text "AGILITY" 5 "green"))
(define HEALTH (text "HEALTH" 5 "red"))

(define ORC (bitmap "orc.png") )
(define HYDRA (bitmap "hydra.png") )
(define SLIME (bitmap "slime.png") )
(define BRIGAND (bitmap "brigand.png") )

(define PIC-LIST (list ORC HYDRA SLIME BRIGAND) )
(define w (apply max (map image-width PIC-LIST) ) )
(define h (apply max (map image-height PIC-LIST) ) )

(define FRAME ((rectangle w h 'outline 'white) ))
(define TARGET (circle (- (/ w 2) 2) 'outline 'blue) )

(define PLAYER-IMAGE (bitmap "player.png") )
(define ORC-IMAGE (overlay ORC FRAME) )
(define HYDRA-IMAGE (overlay HYDRA FRAME) )
(define SLIME-IMAGE (overlay SLIME FRAME) )
(define BRIGAND-IMAGE (overlay BRIGAND FRAME) )
 

(define (player-update! setter selector mx)
 (lambda (player delta)
   (setter player (interval+ (selector player) delta mx))))

(define (interval+ a b MAX)
  (min MAX (max 0 (+ a b))))

(define (interval- a b MIN)
  (max MIN (min 35 (- a b))))

;healthy:
 (define player-health+ 
   (player-update! set-player-health! player-health MAX-HEALTH))

;agility:
 (define player-agility+
   (player-update! set-player-agility! player-agility MAX-AGILITY))

;strenght:
 (define
   player-strength+
   (player-update! set-player-strength! player-strength MAX-STRENGTH))

;world inicialize
(define (initialize-orc-world)
  (define player0 (initialize-player))
  (define lom0 (initialize-monsters))
  (orc-world player0 lom0 (random-number-of-attacks player0) 0))

(define (initialize-monsters)
 (build-list
 MONSTER# 
 (lambda (_)
 (define health (random+ MONSTER-HEALTH))
 (case (random 4)
 [(0) (orc ORC-IMAGE health (random+ CLUB-STRENGTH))]
 [(1) (hydra HYDRA-IMAGE health)]
 [(2) (slime SLIME-IMAGE health (random+ SLIMINESS))]
 [(3) (brigand BRIGAND-IMAGE health)]))))

;end game
(define (end-of-orc-battle? w)
 (or (win? w) (lose? w)))

;rendering game
(define (render-orc-battle w)
 (render-orc-world w (orc-world-target w) (instructions w)))

(define (render-the-end w)
 (render-orc-world w #f (message (if (lose? w) LOSE WIN))))

;main rendering
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

;player rendering
(define (render-player p)
  (define s (player-strength p))
  (define a (player-agility p))
  (define h (player-health p))
  (above/align 
   "left"
   (status-bar s MAX-STRENGTH STRENGTH-COLOR STRENGTH)
   V-SPACER
   (status-bar a MAX-AGILITY AGILITY-COLOR AGILITY)
   V-SPACER
   (status-bar h MAX-HEALTH HEALTH-COLOR HEALTH)
   V-SPACER V-SPACER V-SPACER
   PLAYER-IMAGE))

;status
(define (status-bar v-current v-max color label)
 (define w (* (/ v-current v-max) HEALTH-BAR-WIDTH))
 (define f (rectangle w HEALTH-BAR-HEIGHT 'solid color))
 (define b (rectangle HEALTH-BAR-WIDTH HEALTH-BAR-HEIGHT))
 (define bar (overlay/align "left" "top" f b))
 (beside bar H-SPACER (text label HEALTH-SIZE color)))

(define INSTRUCTION-TEXT "Instructions:")
(define REMAINING "Remaining attacks: ")
(define (instructions w)
 (define na (number->string (orc-world-attack# w)))
 (define ra (string-append REMAINING na))
 (define txt (text ra INSTRUCTION-TEXT-SIZE ATTACK-COLOR))
 (above txt INSTRUCTION-TEXT))

(define (message str)
 (text str MESSAGES-SIZE MESSAGE-COLOR))

;monster rendering
(define MONSTER-COLOR "orange") ; You can adjust the color as needed
(define DEAD-TEXT "DEAD")
(define (render-monsters lom with-target)
  (define target
    (if (number? with-target) 
        (list-ref lom with-target)
        'a-silly-symbol-that-cannot-be-eq-to-an-orc))
  (define (render-one-monster m)
    (define image
      (if (eq? m target)
          (overlay TARGET (monster-image m))
          (monster-image m)))
    (define health (monster-health m))
    (define health-bar
      (if (= health 0)
          (overlay DEAD-TEXT (status-bar 0 1 'white ""))
          (status-bar health MONSTER-HEALTH MONSTER-COLOR "")))
    (above health-bar image))
  (arrange (map render-one-monster lom)))

(define (arrange lom)
 (cond
 [(empty? lom) empty-image]
 [else (define r (apply beside (take lom PER-ROW)))
 (above r (arrange (drop lom PER-ROW)))]))

;if win
(define (all-dead? lom)
  (andmap (lambda (m) (= (monster-health m) 0)) lom))
(define (win? w)
 (all-dead? (orc-world-lom w)))
;if lose
(define (lose? w) 
 (player-dead? (orc-world-player w)))
;if die
(define (player-dead? p)
 (or (= (player-health p) 0) 
 (= (player-agility p) 0)
 (= (player-strength p) 0)))

;end turn
(define (end-turn w)
 (set-orc-world-attack#! w 0))

(define (heal w)
 (decrease-attack# w)
 (player-health+ (orc-world-player w) HEALING))

;stab
(define STAB-DAMAGE 10) ; You can adjust the damage value as needed
(define (stab w)
 (decrease-attack# w)
 (define target 
 (list-ref (orc-world-lom w) (orc-world-target w)))
 (define damage 
 (random-quotient (player-strength (orc-world-player w)) 
 STAB-DAMAGE))
 (damage-monster target damage))

;flail
(define (monster-alive? m)
  (> (monster-health m) 0))
(define FLAIL-DAMAGE 5) ; You can adjust the damage value as needed
(define (flail w)
 (decrease-attack# w)
 (define target (current-target w))
 (define alive (filter monster-alive? (orc-world-lom w)))
 (define pick# 
 (min
 (random-quotient (player-strength (orc-world-player w)) 
 FLAIL-DAMAGE)
 (length alive)))
 (define getem (cons target (take alive pick#)))
 (for-each (lambda (m) (damage-monster m 1)) getem))

;decrease attack
(define (decrease-attack# w)
 (set-orc-world-attack#! w (sub1 (orc-world-attack# w))))

;damage-monster
(define (damage-monster m delta)
 (set-monster-health! m (interval- (monster-health m) delta)))

;current target
(define (current-target w)
 (list-ref (orc-world-lom w) (orc-world-target w)))

;move-target
(define (move-target w delta) 
 (define new (+ (orc-world-target w) delta))
 (set-orc-world-target! w (modulo new MONSTER#)))

(define (give-monster-turn-if-attack#=0 w)
 (when (zero? (orc-world-attack# w))
 (define player (orc-world-player w))
 (all-monsters-attack-player player (orc-world-lom w))
 (set-orc-world-attack#! w (random-number-of-attacks player))))

(define HEALTH-DAMAGE 5) ; You can adjust the damage value as needed
(define AGILITY-DAMAGE 5) ; You can adjust the damage value as needed
(define STRENGTH-DAMAGE 5) ; You can adjust the damage value as needed


(define (all-monsters-attack-player player lom) 
  (define (one-monster-attacks-player m)
    (cond
      [(orc? m)
       (player-health+ player (random(orc-club m)))]
      [(hydra? m)
       (player-health+ player (random(monster-health m)))]
      [(slime? m) 
       (player-health+ player -1)
       (player-agility+ player 
                        (random(slime-sliminess monster)))]
      [(brigand? m) 
       (case (random 3)
         [(0) (player-health+ player HEALTH-DAMAGE)]
         [(1) (player-agility+ player AGILITY-DAMAGE)]
         [(2) (player-strength+ player STRENGTH-DAMAGE)])])) 
  (define live-monsters (filter monster-alive? lom))
  (for-each one-monster-attacks-player live-monsters))


;keyboard
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
(define ATTACKS# 10) ; You can adjust the value as needed
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


;funcao principal --- BIG BANG ---

(define (start) 
  (big-bang (initialize-orc-world)
    (on-key player-acts-on-monsters) 
    (to-draw render-orc-battle)
    (stop-when end-of-orc-battle? render-the-end)))