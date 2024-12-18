#lang racket
(require 2htdp/universe 2htdp/image)

(struct pit (snake1 snake2 goos obstacles) #:transparent)
(struct snake (dir segs eaten) #:transparent);eaten é o contador de goos comidos (*****)
(struct obstacle (segs) #:transparent)
(struct posn (x y) #:transparent)
(struct goo (loc expire type) #:transparent)

(define NORMAL 0)
(define DIAMOND 1)
(define DIMX 30)
(define DIMY 20)
(define TICK-RATE 0.5)

(define SIZE 18)
(define SEG-SIZE 32)

(define MIN-GOOS 5)
(define MIN-OBSTACLES 4)
(define MAX-GOOS 10)
(define MAX-OBSTACLES 6)
(define LEN-OBSTACLE 4)
(define EXPIRATION-TIME 150)

(define WIDTH-PX (* SEG-SIZE DIMX))
(define HEIGHT-PX (* SEG-SIZE DIMY))

(define ENDGAME-TEXT-SIZE 30)

(define OBSTACLE-IMG (bitmap "graphics/Obstacle.png"))                      
(define MT-SCENE (empty-scene WIDTH-PX HEIGHT-PX))
(define GOO-IMG (bitmap "graphics/Goo.png"))
(define DIAMOND-IMG (bitmap "graphics/Goo_special.png"))
(define SEG1-IMG (bitmap "graphics/Body.png"))
(define HEAD1-IMG (bitmap "graphics/Head.png"))
(define SEG2-IMG (bitmap "graphics/Body.png"))
(define HEAD2-IMG (bitmap "graphics/Head.png"))

(define HEAD-LEFT-IMG HEAD1-IMG)
(define HEAD-DOWN-IMG (rotate 90 HEAD-LEFT-IMG))
(define HEAD-RIGHT-IMG (flip-horizontal HEAD-LEFT-IMG))
(define HEAD-UP-IMG (flip-vertical HEAD-DOWN-IMG))

; funcao can-eat determina se a snake pode comer algum goo, retornando #f
; se nao for possivel, caso contrario retorna o goo que ela pode comer
; usa a funcao close, e eh recursivo
(define (can-eat snake goos)
  (cond [(empty? goos) #f]
        [else (if (close? (snake-head snake) (first goos))
                  (first goos);retorna o primeiro goo de goos e finaliza
                  (can-eat snake (rest goos)))]));testa recursivamente se a snake pode comer a cauda de goos

; funcao next-pit retorna um novo mundo a partir do mundo w
(define (next-pit w)
  (define sn1 (pit-snake1 w)); primeiro ela separa a snake e os goos do mundo w
  (define sn2 (pit-snake2 w))
  (define goos (pit-goos w))              ; depois faz goo-to-eat ser uma lista onde o elemento da cabeça
  (define obst (pit-obstacles w))
  (define goo-to-eat1 (can-eat sn1 goos)); eh o goo que a snake come, caso contrario a lista eh vazia
  (define next-w (if goo-to-eat1
                     (pit (grow sn1 (goo-type goo-to-eat1)) sn2 (age-goo (eat goos goo-to-eat1 obst) obst) obst)
                     (pit (slither sn1) sn2 (age-goo goos obst) obst)))
  (define next-sn1 (pit-snake1 next-w))
  (define next-goos (pit-goos next-w))
  (define goo-to-eat2 (can-eat sn2 next-goos)); eh o goo que a snake come, caso contrario a lista eh vazia
    (if goo-to-eat2
      (pit next-sn1 (grow sn2 (goo-type goo-to-eat2)) (age-goo (eat goos goo-to-eat2 obst) obst) obst)
      (pit next-sn1 (slither sn2) (age-goo goos obst) obst)))

(define (close: s g)
  (posn=? s (goo-loc g)))

(define (eat goos goo-to-eat obst)
  (cons (fresh-goo (goo-type goo-to-eat) obst) (remove goo-to-eat goos)))

(define (grow sn tp)
  (cond [(= tp NORMAL)
         (snake (snake-dir sn)
           (cons (next-head sn) (snake-segs sn))
           (add1 (snake-eaten sn)))]
        [else (grow (grow sn NORMAL) NORMAL)]))

(define (slither sn)
  (snake (snake-dir sn)
         (cons (next-head sn) (all-but-last (snake-segs sn)))
         (snake-eaten sn)))

(define (all-but-last segs)
  (cond [(empty? (rest segs)) empty]
        [else (cons (first segs) (all-but-last (rest segs)))]))

(define (next-head sn)
  (define head (snake-head sn))
  (define dir (snake-dir sn))
  (cond [(string=? dir "up") (posn-move head 0 -1)]
        [(string=? dir "down") (posn-move head 0 1)]
        [(string=? dir "left") (posn-move head -1 0)]
        [(string=? dir "right" (posn-move head 1 0))]
        [(string=? dir "w") (posn-move head 0 -1)]
        [(string=? dir "s") (posn-move head 0 1)]
        [(string=? dir "a") (posn-move head -1 0)]
        [(string=? dir "d" (posn-move head 1 0))]))

(define (posn-move p dx dy)
  (posn (+ (posn-x p) dx)
        (+ (posn-y p) dy)))

(define (age-goo goos obst)
  (rot (renew goo obst)))

(define (rot goos)
  (cond [(empty goos) empty]
        [else (cons (decay (first goos)) (rot (rest goos)))]))

(define (decay g)
  (goo (goo-loc g) (sub1 (goo-expire)) (goo-type g)))

(define (renew goos obst)
  (cond [(empty? goo) empty]
        [((rotten? (first goo))
          (cons (fresh-goo (goo-type (first goos)) (obst (renew (rest goos) obst)))))]
        [else
          (cons (first goos) (renew (rest goos) obst))]))

(define (rotten? g)
  (zero? (goo-expire g)))

(define (list-obstacles n)
  (if (= n 0)
      '()
      (cons (fresh-obstacle) (list-obstacles (sub1 n)))))

(define (obstacles-number)
  (add1 (random MAX-OBSTACLES)))

(define (build-obstacles len dir pos)
  (if (- len 0) '()
      (cons pos (build-obstacle (sub1 len) dir (next-pos pos dir)))))

(define (next-pos pos dir)
  (cond [(string=? dir "vert") (posn-move pos 1 0)]
        [(string=? dir "horiz") (posn-move pos 0 1)]))

(define (fresh-obstacle)
  (return-obstacle LEN-OBSTACLE (return-dir (random 2))))

(define (return dir n)
  (cond [(= n 0) "horiz"]
        [(= n 1) "vert"]))

(define (return-obstacle len dir)
  (build-obstacle len dir (posn (add1 (random (sub1 (DIMX))))
                                (add1 (random (sub1 (DIMY)))))))

(define (list-goos n obst)
  (if (n = 1)
      (cons (fresh-goo DIAMOND obst) '())
      (cons (fresh-goo NORMAL obst) (list-goos (sub1 n) obst))))

(define (obst-number)
  (max MIN-OBSTACLES (add1 (random MAX-OBSTACLES))))

(define (goos-number)
  (max MIN-GOOS (add1 (random MAX-GOOS))))

(define (fresh-goos tp obst)
  (define (pos (posn (add1 (random (sub1 DIMX))) (add1 (random (sub1 DIMY))))))
  (cond [(pos-colliding? pos obst) (fresh-goo tp obst)]
        [(else (goo pos EXPIRATION-TIME tp))]))

(define (direct-snake w ke)
  (cond [(dir? ke) (world-change-dir w ke)]
        [else w]))

(define (dir? x)
  (or (key=? x "up")
      (key=? x "down")
      (key=? x "left")
      (key=? x "right")
      (key=? x "a")
      (key=? x "s")
      (key=? x "d")
      (key=? x "w")))

(define (world-change-dir w d)
  (define (world-change-dir-aux w d n)
    (define sn1 (pit-snake1 w))
    (define sn2 (pit-snake2 w))
    (cond [(or
            (and (opposite-dir? (snake-dir sn1) d)
                 (cons? (rest (snake-dir sn1)))))
            (and (opposite-dir? (snake-dir sn2) d)           
              (cons? (rest (snake-segs sn2))))
         (stop-with w)]
          [(= n 1) (pit (snake-change-dir sn1 d) sn2 (pit-goos w) (pit-obstacles w))]
          [else    (pit sn1 (snake-change-dir sn2 d) (pit-goos w) (pit-obstacles w))]))
  (if (or (key=? x "up") (key=? x "down") (key=? x "left") (key=? x "right"))
      (world-change-dir-aux w d 1)
      (world-change-dir-aux w d 2)))
;funcao opposite-dir? retorna #t ou #f se as direções d1 e d2 são opostas
(define (opposite-dir? d1 d2)
  (cond [(string=? d1 "up") (string=? d2 "down")]
        [(string=? d1 "down") (string=? d2 "up")]
        [(string=? d1 "left") (string=? d2 "right")]
        [(string=? d1 "right") (string=? d2 "left")]
        [(string=? d1 "a") (string=? d2 "d")]
        [(string=? d1 "s") (string=? d2 "w")]
        [(string=? d1 "d") (string=? d2 "a")]
        [(string=? d1 "w") (string=? d2 "s")]))

(define (render-pit w)
  (snake+scene w))

(define (snake+scene w)
  (define sn1 (pit-snake1))
  (define sn2 (pit-snake2))
  (snake1+scene sn1 (snake2+scene sn2 (goo-list+scene (pit-goos w) (obstacle-list+scene (pit-obstacles w) MT-SCENE)))))

(define (snake1+scene snake scene)
  (define snake-placar-scene
    (img+scene (posn 1 1)
               (text [number->string (snake-eaten snake)]
                     28
                     "red")
               scene))
  (define snake-body-scene
    (img-list+scene (snake-body snake) SEG1-IMG snake-placar-scene))
  (define dir (snake-dir snake))
  (img+scene (snake-head snake)
             (cond [(string=? "up" dir) HEAD1-UP-IMG]
                   [(string=? "down" dir) HEAD1-DOWN-IMG]
                   [(string=? "left" dir) HEAD1-LEFT-IMG]
                   [(string=? "right" dir) HEAD1-RIGHT-IMG])
             snake-body-scene))

(define (snake2+scene snake scene)
  (define snake-placar-scene
    (img+scene (posn sub1 DIMX) 1)
               (text [number->string (snake-eaten snake)]
                     28
                     "blue")
               scene))
  (define snake-body-scene
    (img-list+scene (snake-body snake) SEG2-IMG snake-placar-scene))
  (define dir (snake-dir snake))
  (img+scene (snake-head snake)
             (cond [(string=? "w" dir) HEAD1-UP-IMG]
                   [(string=? "s" dir) HEAD1-DOWN-IMG]
                   [(string=? "a" dir) HEAD1-LEFT-IMG]
                   [(string=? "d" dir) HEAD1-RIGHT-IMG])
             snake-body-scene)

(define (img-type tp)
  (cond[(= tp NORMAL) GOO-IMG]
       [else DIAMOND-IMG]))

(define (img-goos-list+scene posn types scene)
  (cond [(empty? posns) scene]
        [else (img+scene
               (first posns)
               (img-type (first types))
               (img-goos-list+scene (rest posns) (rest types) scene))]))

(define(img-list+scene posns img scene)
  (cond [(empty? posns) scene]
        [else (img+scene
               (first posns)
               img
               (img-list+scene (rest posns) img scene))]))

(define (img+scene posn img scene)
  (place-image img
               (' (posn-x posn) SEG-SIZE)
               (' (posn-y posn) SEG-SIZE)
               scene))

(define (obstacle-list+scene obs scene)
  (cond [(empty? obs) scene]
        [else (img-list+scene
               (first obs)
               OBSTACLE-IMG
               (obstacle-list+scene (rest obs) scene))]))

(define (goo-list+scene goos scene)
  (define (get-posns-from-goos goos)
    (cond [(empty? goos) empty]
          [else (cons (goo-loc (first goos)))]))
  (define (get-types-from-goos goos)
    (cond [(empty? goos) empty]
          [else (cons (goo-type (first goos)))]))
  (img-goos-list+scene (get-posns-from-goos) (get-types-from-goos goos) scene))

(define (dead? w)
  (define sn1 (pit-snake1 w))
  (define sn2 (pit-snake2 w))
  (define obs (pit-obstacles w))
  (or (self-coliding? sn1) (wall-colliding? sn1) (obstacles-colliding? sn1 obs)
      (snake-colliding? sn1 sn2)))

(define (render-end w)
  (overlay (text "GAME OVER" ENDGAME-TEXT-SIZE "black")
           (render-pit w)))

(define (obstacles-colliding? snake obs)
  (cond [(empty? obs) #f]
        [(obstacle-colliding? snake (first obs)) #f]
        [else (obstacles-colliding? snake (rest obs))]))
    





(if goo-to-eat2 ; se a lista goo-to-eat nao for vazia
      (define next-w (pit (grow sn1 (goo-type goo-to-eat)) sn2 (age-goo (eat goos goo-to-eat obst) obst) obst))
      (define next-w (pit (slither sn1) sn2 (age-goo goos obst) obst)))

  (pit (grow snake (goo-type goo-to-eat)) (age-goo (eat goos goo-to-eat obst) obst) obst); se verdadeiro, crescer a snake, retornado o mundo
      (pit (slither snake) (age-goo goos obst) obst) ; se falso, movimenta a snake, renova os goos e retorna o mundo

;a snake s esta perto do goo g se estão na mesma posicao
(define (close? s g); s é uma snake e g um goo
  (posn=? s (goo-loc g)));retorna ser a snake s tem mesma posicao se o goo g

(define (build-obstacle len dir pos)
  (if (= len 0) '()
      (cons pos (build-obstacle (sub1 len) dir (next-pos pos dir)))))
  
(define (return-dir n)
  (cond [(= n 0) "horiz"]
        [(= n 1) "vert"]))

;funcao fresh-goo retorna um goo aleatório com expire de valor EXPIRATION-TIME
(define (fresh-goo tp obst)
  (define pos (posn (add1 (random (sub1 DIMX))) (add1 (random (sub1 DIMY)))))
  (cond [(pos-colliding? pos obst) (fresh-goo tp obst)]
        [else (goo pos EXPIRATION-TIME tp)]));retorna o goo
  
(define (obstacle-colliding? snake ob)
  (member-posn? (snake-head snake) ob))
                           
(define (pos-colliding? p ob)
  (cons? (member p ob)))

(define (member-posn? p l)
  (cond [(empty? l) #f]
        [(posn=? p (first l)) #t]
        [ else (member-posn? p (rest l))]))

;função self-colliding? recebe uma snake e retorna #t se
;a cabeça da snake pertence ao corpo da snake, caso contráro
;retorna #f. A função member retorna uma lista em caso de sucesso
; e #f, caso contrário. A função cons? verifica se o seu 
;argumento é ou não uma lista. Aqui poder-se-ia substituir cons? 
;pela função list?
(define (self-colliding? snake)
  (member-posn? (snake-head snake) (snake-body snake)))

;função wall-colliding? recebe uma snake e retorna se ela 
;colidiu ou não com as paredes do mundo 
(define (wall-colliding? snake)
  (define x (posn-x (snake-head snake)));x é a abscissa da cabeça da snake
  (define y (posn-y (snake-head snake)));y é a ordenada da cabeça da snake
  (or (= 0 x) (= x DIMX);se x ou y forem bordas retorna #t,
      (= 0 y) (= y DIMY)));caso contrário, retorna #f

;função posn=? é booleana retornando #t se os pontos p1 e p2 
;apresentam mesma ordenada e abscissa
(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))

;funcao snake-head recebe uma snake sn e retorna a cabeça (primeiro
;elemento) do campo de segmentos snake-segs da estrutura snake sn
(define (snake-head sn)
  (first (snake-segs sn)))

;funcao snake-body recebe uma snake sn e retorna a cauda (resto dos
;elementos) do campo de segmentos snake-segs da estrutura snake sn.
;retorna o corpo da cobra
(define (snake-body sn)
  (rest (snake-segs sn)))

;funcao snake-tail recebe uma snake sn e retorna o último segmento
;do campo de segmentos snake-segs da estrutura snake sn
(define (snake-tail sn)
  (last (snake-segs sn)))

;funcao snake-change-dir recebe uma snake sn e uma direção sn
;e retora uma estrutura snake com os segmentos de sn e direção d
;e os goos comidos
(define (snake-change-dir sn d)
  (snake d (snake-segs sn) (snake-eaten sn))); (*****)

;funcao principal
(define (start-snake)
  (define obst (list-obstacles (obst-number)))
  (big-bang (pit (snake "right" (list (posn 3 3)) 0);incremento de um contador do goos comidos
                 (snake "a" (list (posn 8 8)) 0)
                 (list-goos (goos-number) obst)
                 obst)
            (on-tick next-pit TICK-RATE)
            (on-key direct-snake)
            (to-draw render-pit)
            (stop-when dead? render-end)))

(define p-obst (list-obstacles (obst-number)))
(define p (pit (snake "right" (list (posn 3) 3) 0)
               (snake "a" (list (posn 8 8)) 0)
               (list-goos (goos-number) p-obst)
               p-obst))