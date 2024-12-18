#lang racket
(require 2htdp/universe 2htdp/image)
;os structs principais
(struct campo(pombo poops))
(struct pombo(dir segs))
(struct posn(x y))
(struct poops(loc expire))
;variáveis fixas
(define HEIGHT 1500)
(define WIDTH 1500)
(define TICK-RATE 0.1)
(define MT-SCENE(empty-scene WIDTH HEIGHT))

(define poop-img (bitmap "POMBO/poop.png"))
(define poop-IMG
  (scale 0.1 poop-img))
(define pombo-direita-img (bitmap "POMBO/pombo-direita.png"))
(define pombo-direita
  (scale 0.1 pombo-direita-img))
(define pombo-esquerda-img (bitmap "POMBO/pombo-esquerda.png"))
(define pombo-esquerda
  (scale 0.1 pombo-esquerda-img))
(define ovo-img (bitmap "POMBO/ovo.png"))
(define ovo
  (scale 0.2 ovo-img))

(define (next-campo w)
  (define pato(campo-pombo w))
  (define ovos(campo-poops w))
  (define poop-resgatado(resgatavel pombo poops))
  (if poop-resgatado
      (campo (grow pombo) (age-poops (resgatar poops poop-resgatado)))
      (campo (anda pombo) (age-poops poops))))

(define (resgatavel pombo poops)
 (cond [(empty? poops) #f]
 [else (if (close? (pombo-head pombo) (first poops))
           (first poops)
           (resgatavel pombo (rest poops)))]))

(define (close? s g)
 (posn=? s (poops-loc g)))

(define (resgatar poops poop-resgatado)
 (cons (fresh-poop) (remove poop-resgatado poops)))

(define (grow pt)
 (pato (pombo-dir pt) 
 (cons (next-head pt) (pombo-segs pt))))
  
(define (anda pt)
 (pato (pombo-dir pt)
 (cons (next-head pt) (all-but-last (pombo-segs pt)))))

(define (all-but-last segs)
 (cond [(empty? (rest segs)) empty]
       [else (cons (first segs) (all-but-last (rest segs)))]))

(define (next-head pt)
 (define head (pombo-head pt))
 (define dir (pombo-dir pt))
 (cond [(string=? dir "up") (posn-move head 0 -1)]
       [(string=? dir "down") (posn-move head 0 1)]
       [(string=? dir "left") (posn-move head -1 0)]
       [(string=? dir "right") (posn-move head 1 0)]))

(define (posn-move p dx dy)
 (posn (+ (posn-x p) dx)
 (+ (posn-y p) dy)))

(define (age-poops poops)
 (rot (renew poops)))

(define (rot poops)
 (cond [(empty? poops) empty]
       [else (cons (decay (first poops)) (rot (rest poops)))]))

(define (decay poop)
  (define expire (poops-expire poop))
  (if (> expire 0)
      (ovos (poops-loc poop) (- expire 1))
      (fresh-poop)))

(define (fresh-poop)
 (poops (posn (add1 (random (sub1 40)))
 (add1 (random (sub1 40))))
 100))

(define (renew poops)
 (cond [(empty? poops) empty]
       [(rotten? (first poops))
        (cons (fresh-poop) (renew (rest poops)))]
       [else
        (cons (first poops) (renew (rest poops)))]))

(define (rotten? poop)
  (<= (poops-expire poop) 0))

(define (direct-pato w ke)
 (cond [(dir? ke) (world-change-dir w ke)]
 [else w]))

(define (dir? x)
 (or (key=? x "up") 
 (key=? x "down") 
 (key=? x "left") 
 (key=? x "right")))

(define (world-change-dir w d)
 (define the-pombo (campo-pombo w))
 (cond [(and (opposite-dir? (pombo-dir the-pombo) d)
             (cons? (rest (pombo-segs the-pombo))))
        (stop-with w)]
       [else 
        (campo (pombo-change-dir the-pombo d) (campo-poops w))]))

(define (opposite-dir? d1 d2)
 (cond [(string=? d1 "up") (string=? d2 "down")]
 [(string=? d1 "down") (string=? d2 "up")]
 [(string=? d1 "left") (string=? d2 "right")]
 [(string=? d1 "right") (string=? d2 "left")]))

(define (render-campo w)
 (pombo+scene (campo-pombo w)
 (poop-list+scene (campo-poops w) MT-SCENE)))

(define (pombo+scene pombo scene)
 (define pombo-body-scene
 (img-list+scene (pombo-body pombo) ovo scene))
 (define dir (pombo-dir pombo))
 (img+scene (pombo-head pombo) 
 (cond [(string=? "up" dir) pombo-direita]
 [(string=? "down" dir) pombo-direita]
 [(string=? "left" dir) pombo-esquerda]
 [(string=? "right" dir) pombo-direita])
 pombo-body-scene))

(define (img-list+scene posns img scene)
 (cond [(empty? posns) scene]
 [else (img+scene 
 (first posns)
 img 
 (img-list+scene (rest posns) img scene))]))

(define (img+scene posn img scene)
 (place-image img 
 (* (posn-x posn) 20)
 (* (posn-y posn) 20)
 scene))

(define (poop-list+scene poops scene)
 (define (get-posns-from-poop poops)
 (cond [(empty? poops) empty]
       [else (cons (poops-loc (first poops))
                   (get-posns-from-poop (rest poop)))]))
  (img-list+scene (get-posns-from-poop poops) poop-IMG scene))

(define (dead? w)
 (define pombo (campo-pombo w))
(or (self-colliding? pombo) (wall-colliding? pombo)))

(define (render-end w)
 (overlay (text "Game Over" 40 "black")
 (render-campo w)))

  (define (self-colliding? pombo)
 (cons? (member (pombo-head pombo) (pombo-body pombo))))

(define (wall-colliding? pombo)
 (define x (posn-x (pombo-head pombo)))
 (define y (posn-y (pombo-head pombo)))
 (or (= 0 x) (= x 1500)
 (= 0 y) (= y 1500)))

(define (posn=? p1 p2)
 (and (= (posn-x p1) (posn-x p2))
 (= (posn-y p1) (posn-y p2))))

(define (pombo-head pt)
 (first (pombo-segs pt)))

(define (pombo-body pt)
 (rest (pombo-segs pt)))

(define (pombo-tail pt)
 (last (pombo-segs pt)))

(define (pombo-change-dir pt d)
 (pombo d (pombo-segs pt)))

(define start-pombo
  (big-bang (campo (pombo "right" (list (posn 1 1)))
                   (list (fresh-poop)
                         (fresh-poop)
                         (fresh-poop)
                         (fresh-poop)
                         (fresh-poop)))
            (on-tick next-campo TICK-RATE)
            (on-key direct-pato)
            (to-draw render-campo)
            (stop-when dead?)))