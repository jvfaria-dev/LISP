#lang racket
(require 2htdp/universe 2htdp/image)
;main structs
(struct campo(pombo ovos))
(struct pombo(dir segs))
(struct posn(x y))
(struct ovos(loc expire))
;fixed variables
(define HEIGHT 35)
(define WIDTH 68)
(define TICK-RATE 0.1)
(define SEG-SIZE 20)
(define MT-SCENE(empty-scene (* SEG-SIZE WIDTH) (* SEG-SIZE HEIGHT)))


(define milho-img (bitmap "imagens/milho.png"))
(define milho-IMG
  (scale 0.07 milho-img))
(define pombo-direita-img (bitmap "imagens/pombo-direita.png"))
(define pombo-direita
  (scale 0.07 pombo-direita-img))
(define pombo-esquerda-img (bitmap "imagens/pombo-esquerda.png"))
(define pombo-esquerda
  (scale 0.07 pombo-esquerda-img))
(define body-img (bitmap "imagens/body.png"))
(define body
  (scale 0.4 body-img))

(define (next-campo w)
  (define pombo(campo-pombo w))
  (define ovos(campo-ovos w))
  (define milho-resgatado(resgatavel pombo ovos))
  (if milho-resgatado
      (campo (grow pombo) (age-ovos (resgatar ovos milho-resgatado)))
      (campo (anda pombo) (age-ovos ovos))))

(define (resgatavel pombo ovos)
 (cond [(empty? ovos) #f]
 [else (if (close? (pombo-head pombo) (first ovos))
           (first ovos)
           (resgatavel pombo (rest ovos)))]))

(define (close? s g)
 (posn=? s (ovos-loc g)))

(define (resgatar ovos milho-resgatado)
 (cons (fresh-milho) (remove milho-resgatado ovos)))

(define (grow pt)
 (pombo (pombo-dir pt) 
 (cons (next-head pt) (pombo-segs pt))))
  
(define (anda pt)
 (pombo (pombo-dir pt)
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

(define (age-ovos ovos)
 (rot (renew ovos)))

(define (rot ovos)
 (cond [(empty? ovos) empty]
       [else (cons (decay (first ovos)) (rot (rest ovos)))]))

(define (decay milho)
  (define expire (ovos-expire milho))
  (if (> expire 0)
      (ovos (ovos-loc milho) (- expire 1))
      (fresh-milho)))

(define (fresh-milho)
 (ovos (posn (add1 (random (sub1 WIDTH)))
 (add1 (random (sub1 HEIGHT))))
 1000))

(define (renew ovos)
 (cond [(empty? ovos) empty]
       [(rotten? (first ovos))
        (cons (fresh-milho) (renew (rest ovos)))]
       [else
        (cons (first ovos) (renew (rest ovos)))]))

(define (rotten? milho)
  (<= (ovos-expire milho) 0))

(define (direct-pombo w ke)
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
        (campo (pombo-change-dir the-pombo d) (campo-ovos w))]))

(define (opposite-dir? d1 d2)
 (cond [(string=? d1 "up") (string=? d2 "down")]
 [(string=? d1 "down") (string=? d2 "up")]
 [(string=? d1 "left") (string=? d2 "right")]
 [(string=? d1 "right") (string=? d2 "left")]))

(define (render-campo w)
 (pombo+scene (campo-pombo w)
 (milho-list+scene (campo-ovos w) MT-SCENE)))

(define (pombo+scene pombo scene)
 (define pombo-body-scene
   (img-list+scene (pombo-body pombo) body scene))
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
 (* (posn-x posn) SEG-SIZE)
 (* (posn-y posn) SEG-SIZE)
 scene))

(define (milho-list+scene ovos scene)
 (define (get-posns-from-milho ovos)
 (cond [(empty? ovos) empty]
       [else (cons (ovos-loc (first ovos))
                   (get-posns-from-milho (rest ovos)))]))
  (img-list+scene (get-posns-from-milho ovos) milho-IMG scene))

(define (dead? w)
 (define pombo (campo-pombo w))
  (or (self-colliding? pombo) (wall-colliding? pombo)))

(define (render-end w)
 (overlay (text "Game Over" 40 "red")
 (render-campo w)))

(define (self-colliding? pombo)
  (let ((head (pombo-head pombo))
        (body (pombo-body pombo)))
    (and (not (empty? body))
         (posn-member? head body))))

(define (posn-member? posn posn-list)
  (cond [(empty? posn-list) #f]
        [(posn=? posn (first posn-list)) #t]
        [else (posn-member? posn (rest posn-list))]))

(define (wall-colliding? pombo)
  (define x (posn-x (pombo-head pombo)))
  (define y (posn-y (pombo-head pombo)))
  (or (= 0 x) (= x WIDTH)
      (= 0 y) (= y HEIGHT)))

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
                   (list (fresh-milho)
                         (fresh-milho)
                         (fresh-milho)
                         (fresh-milho)
                         (fresh-milho)))
            (on-tick next-campo TICK-RATE)
            (on-key direct-pombo)
            (to-draw render-campo)
            (stop-when dead? render-end)))
   
