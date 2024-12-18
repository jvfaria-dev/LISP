#lang Racket
(require 2htdp/universe 2htdp/image)

;structs
(struct player (health agility strength) #:mutable)
(struct orc-world (player lom attack# target) #:mutable)

;struct dos monstros
(struct monster (image [health #:mutable]))
(struct orc monster (club))
(struct hydra monster ())
(struct slime monster (sliminess))
(struct brigand monster ())

;variáveis globais
;player
(define MAX-HEALTH 35)
(define MAX-AGILITY 35)
(define MAX-STRENGTH 35)
(define INSTRUCTION-TEXT-SIZE 30)
(define ATTACK-COLOR "red")
(define MESSAGES-SIZE 20)
(define MESSAGE-COLOR "blue")
(define V-SPACER (rectangle 0 10 "solid" "white"))
(define H-SPACER (rectangle 10 0 "solid" "white"))
(define STRENGTH-COLOR "purple")
(define AGILITY-COLOR "green")
(define HEALTH-COLOR "red")
(define HEALTH-BAR-WIDTH 16)
(define HEALTH-BAR-HEIGHT 5)
(define HEALTH-SIZE 10)
(define PER-ROW 3)
(define HEALING 8)
(define MONSTER# 6)
(define MONSTER-HEALTH 20)
(define CLUB-STRENGTH 12)
(define SLIMINESS 5)
(define STAB-DAMAGE 8)
(define FLAIL-DAMAGE 9)
(define HEALTH-DAMAGE -4)
(define AGILITY-DAMAGE -5)
(define STRENGTH-DAMAGE -6)
(define ATTACKS# 5)

;

(define LOSE "VOCE PERDEU")
(define WIN "VOCE VENCEU!!!!")

;imagens
(define STRENGTH "STRENGTH")
(define AGILITY "AGILITY")
(define HEALTH "HEALTH")
(define REMAINING "Ataques restantes: ")
(define INSTRUCTION-TEXT (text "S para esfaquear, F para flail, H para saúde, A para agilidade e G para fortalecer" 3 "blue"))
(define DEAD "FALECEU")
(define DEAD-TEXT-SIZE 8)
(define DEAD-TEXT (text DEAD DEAD-TEXT-SIZE "red"))
(define MONSTER-COLOR "black")

(define ORC (bitmap "graphics2/orc.png") )
(define HYDRA (bitmap "graphics2/hydra.png") )
(define SLIME (bitmap "graphics2/slime.bmp") )
(define BRIGAND (bitmap "graphics2/brigand.bmp") )

(define PIC-LIST (list ORC HYDRA SLIME BRIGAND) )
(define w (apply max (map image-width PIC-LIST) ) )
(define h (apply max (map image-height PIC-LIST) ) )

(define FRAME (rectangle w h 'outline 'white) )
(define TARGET (circle (- (/ w 2) 2) 'outline 'blue) )

(define PLAYER-IMAGE (bitmap "graphics2/player.bmp") )
(define ORC-IMAGE (overlay ORC FRAME) )
(define HYDRA-IMAGE (overlay HYDRA FRAME) )
(define SLIME-IMAGE (overlay SLIME FRAME) )
(define BRIGAND-IMAGE (overlay BRIGAND FRAME) )
 

;funções do jogo (de acordo com o livro Real of Racket)

;A funcao player uptade serve para atualizar os valores dentro do struct de um player sem criar funcoes especificas para cada atributo.
(define (player-update! setter selector mx)
 (lambda (player delta)
   (setter player (interval+ (selector player) delta mx))))

;Funcao interval+ e interval-:
(define (interval+ a b MAX)
  (min MAX (max 0 (+ a b))))

(define (interval- a b MIN)
  (max MIN (min 35 (- a b))))

;As funcoes para atualizar os atributos do player utilizando a funcao player-update!
; vida:
 (define player-health+ 
   (player-update! set-player-health! player-health MAX-HEALTH))

; agilidade:
 (define player-agility+
   (player-update! set-player-agility! player-agility MAX-AGILITY))

;força:
 (define player-strength+
   (player-update! set-player-strength! player-strength MAX-STRENGTH))

;As funcoes que inicializam o mundo do jogo. (lom é list of monsters)
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

;A funcao que termina o jogo.
(define (end-of-orc-battle? w)
 (or (win? w) (lose? w)))

;As funcoes de renderização do jogo.
(define (render-orc-battle w);enquanto o jogo acontece
 (render-orc-world w (orc-world-target w) (instructions w)))

(define (render-the-end w);quando ele termina
 (render-orc-world w #f (message (if (lose? w) LOSE WIN))))

;Funcao render-orc-world: principal de renderizacao
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

;funcao para renderização do player
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

;status bar
(define (status-bar v-current v-max color label)
 (define w (* (/ v-current v-max) HEALTH-BAR-WIDTH))
 (define f (rectangle w HEALTH-BAR-HEIGHT "solid" "red"))
 (define b (rectangle 20 8 "solid" "white"))
 (define bar (overlay/align "left" "top" f b))
 (beside bar H-SPACER (text label HEALTH-SIZE color)))

(define (instructions w);mostra as instruções. A definir instruction text.
 (define na (number->string (orc-world-attack# w)))
 (define ra (string-append REMAINING na))
 (define txt (text ra INSTRUCTION-TEXT-SIZE ATTACK-COLOR))
 (above txt INSTRUCTION-TEXT))

(define (message str)
 (text str MESSAGES-SIZE MESSAGE-COLOR))

;funcao de renderização dos mostros:
(define (render-monsters lom with-target)
 ;; the currently targeted monster (if needed) 
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

;Funcao que organiza a lista de monstros
(define (arrange lom)
 (cond
 [(empty? lom) empty-image]
 [else (define r (apply beside (take lom PER-ROW)))
 (above r (arrange (drop lom PER-ROW)))]))

;Funcao que verifica se vencemos:
(define (win? w)
 (all-dead? (orc-world-lom w)))
;funcao que verifica se perdemos:
(define (lose? w) 
 (player-dead? (orc-world-player w)))
;funcao que verifica se morremos:
(define (player-dead? p)
 (or (= (player-health p) 0) 
 (= (player-agility p) 0)
 (= (player-strength p) 0)))

;fucao que verifica se todos os monstros estao mortos:
(define (all-dead? lom)
 (not (ormap monster-alive? lom)))

(define (monster-alive? m)
 (> (monster-health m) 0))

;funcao para terminar o turno:
(define (end-turn w)
 (set-orc-world-attack#! w 0))

;funcao para curar o jogador:
(define (heal w)
 (decrease-attack# w)
 (player-health+ (orc-world-player w) HEALING))

;funcao stab
(define (stab w)
 (decrease-attack# w)
 (define target 
 (list-ref (orc-world-lom w) (orc-world-target w)))
 (define damage 
 (random-quotient (player-strength (orc-world-player w)) 
 STAB-DAMAGE))
 (damage-monster target damage))

;funcao flail
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

;funcao decrease attack
(define (decrease-attack# w)
 (set-orc-world-attack#! w (sub1 (orc-world-attack# w))))

;funcao damage-monster
(define (damage-monster m delta)
 (set-monster-health! m (interval- (monster-health m) delta 0)))

;funcao current target
(define (current-target w)
 (list-ref (orc-world-lom w) (orc-world-target w)))

;funcao move-target
(define (move-target w delta) 
 (define new (+ (orc-world-target w) delta))
 (set-orc-world-target! w (modulo new MONSTER#)))

;funcao do turno dos monstros
(define (give-monster-turn-if-attack#=0 w)
 (when (zero? (orc-world-attack# w))
 (define player (orc-world-player w))
 (all-monsters-attack-player player (orc-world-lom w))
 (set-orc-world-attack#! w (random-number-of-attacks player))))

;funcao all-monsters-attack
(define (all-monsters-attack-player player lom) 
  (define (one-monster-attacks-player m)
    (cond
      [(orc? m)
       (player-health+ player (random- (orc-club m)))]
      [(hydra? m)
       (player-health+ player (random- (monster-health m)))]
      [(slime? m) 
       (player-health+ player -1)
       (player-agility+ player 
                        (random- (slime-sliminess m)))]
      [(brigand? m) 
       (case (random 3)
         [(0) (player-health+ player HEALTH-DAMAGE)]
         [(1) (player-agility+ player AGILITY-DAMAGE)]
         [(2) (player-strength+ player STRENGTH-DAMAGE)])])) 
  (define live-monsters (filter monster-alive? lom))
  (for-each one-monster-attacks-player live-monsters))

;funcao random -
(define (random- limite)
  (random limite))


;A funcao para usar o teclado no jogo
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


;funcao principal --- BIG BANG ---

(define (start) 
  (big-bang (initialize-orc-world)
    (on-key player-acts-on-monsters) 
    (to-draw render-orc-battle)
    (stop-when end-of-orc-battle? render-the-end)))
