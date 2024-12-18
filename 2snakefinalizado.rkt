#lang racket
(require 2htdp/universe 2htdp/image)

(struct pit (snake1 snake2 goos obstacles)#:transparent)
(struct snake (dir segs eaten)#:transparent)
(struct obstacle (segs)#:transparent)
(struct posn (x y)#:transparent)
(struct goo (loc expire type)#:transparent)

(define NORMAL 0)
(define DIAMOND 1)
(define DIMX 30)
(define DIMY 20)
(define TICK-RATE 0.1)

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
(define SEG1-IMG (bitmap "graphics/Body1.png"))
(define HEAD1-IMG (bitmap "graphics/Head1.png"))
(define SEG2-IMG (bitmap "graphics/Body2.png"))
(define HEAD2-IMG (bitmap "graphics/Head2.png"))

(define HEAD1-LEFT-IMG HEAD1-IMG)
(define HEAD1-DOWN-IMG (rotate 90 HEAD1-LEFT-IMG))
(define HEAD1-RIGHT-IMG (flip-horizontal HEAD1-LEFT-IMG))
(define HEAD1-UP-IMG (flip-vertical HEAD1-DOWN-IMG))
(define HEAD2-LEFT-IMG HEAD2-IMG)
(define HEAD2-DOWN-IMG (rotate 90 HEAD2-LEFT-IMG))
(define HEAD2-RIGHT-IMG (flip-horizontal HEAD2-LEFT-IMG))
(define HEAD2-UP-IMG (flip-vertical HEAD2-DOWN-IMG))

(define (can-eat snake goos)
  (cond [(empty? goos) #f]
        [else (if (close? (snake-head snake) (first goos))
                  (first goos)
                  (can-eat snake (rest goos)))]))

(define (next-pit w)
  (define snake1 (pit-snake1 w))
  (define snake2 (pit-snake2 w))
  (define goos (pit-goos w))       
  (define obst (pit-obstacles w))
  (define goo-to-eat1 (can-eat snake1 goos))
  (define goo-to-eat2 (can-eat snake2 goos))
  (cond [goo-to-eat1
         (pit (grow snake1 (goo-type goo-to-eat1)) (slither snake2) (age-goo (eat goos goo-to-eat1 obst) obst) obst)]
        [goo-to-eat2
         (pit (slither snake1) (grow snake2 (goo-type goo-to-eat2)) (age-goo (eat goos goo-to-eat2 obst) obst) obst)]
        [else
         (pit (slither snake1) (slither snake2) (age-goo goos obst) obst)]))
    

(define (close? s g)
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
        [(string=? dir "right") (posn-move head 1 0)]
        [(string=? dir "w") (posn-move head 0 -1)]
        [(string=? dir "s") (posn-move head 0 1)]
        [(string=? dir "a") (posn-move head -1 0)]
        [(string=? dir "d") (posn-move head 1 0)]))

(define (posn-move p dx dy)
  (posn (+ (posn-x p) dx)
        (+ (posn-y p) dy)))

(define (age-goo goos obst)
  (rot (renew goos obst)))

(define (rot goos)
  (cond [(empty? goos) empty]
        [else (cons (decay (first goos)) (rot (rest goos)))]))

(define (decay g)
  (goo (goo-loc g) (sub1 (goo-expire g)) (goo-type g)))

(define (renew goos obst)
  (cond [(empty? goos) empty]
        [(rotten? (first goos))
         (cons (fresh-goo (goo-type (first goos)) obst) (renew (rest goos) obst))]
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

(define (build-obstacle len dir pos)
  (if (= len 0) '()
      (cons pos (build-obstacle (sub1 len) dir (next-pos pos dir)))))

(define (next-pos pos dir)
  (cond [(string=? dir "vert") (posn-move pos 1 0)]
        [(string=? dir "horiz") (posn-move pos 0 1)]))
       
(define (fresh-obstacle)
  (return-obstacle LEN-OBSTACLE (return-dir (random 2))))
  
(define (return-dir n)
  (cond [(= n 0) "horiz"]
        [(= n 1) "vert"]))
    
(define (return-obstacle len dir)  
  (build-obstacle len dir (posn (add1 (random (sub1 DIMX)))
                                (add1 (random (sub1 DIMY))))))
  
       
(define (list-goos n obst)
  (if (= n 1) 
      (cons (fresh-goo DIAMOND obst) '())
      (cons (fresh-goo NORMAL obst) (list-goos (sub1 n) obst))))

(define (obst-number)
  (max MIN-OBSTACLES (add1 (random MAX-OBSTACLES))))


(define (goos-number)
  (max MIN-GOOS (add1 (random MAX-GOOS))))

(define (fresh-goo tp obst)
  (define pos (posn (add1 (random (sub1 DIMX))) (add1 (random (sub1 DIMY)))))
  (cond [(pos-colliding? pos obst) (fresh-goo tp obst)]
        [else (goo pos EXPIRATION-TIME tp)]))


(define (direct-snake w ke)
  (cond [(dir? ke) (world-change-dir w ke)]
        [else w]))

(define (dir? x)
  (or (key=? x "up")
      (key=? x "down")
      (key=? x "left")
      (key=? x "right")
      (key=? x "w")
      (key=? x "s")
      (key=? x "a")
      (key=? x "d")))

(define (world-change-dir w d)
  (define (world-change-dir-aux w d n)
    (define sn1 (pit-snake1 w))
    (define sn2 (pit-snake2 w))
    (cond [(or
            (and (opposite-dir? (snake-dir sn1) d)
                 (cons? (rest (snake-segs sn1))))
            (and (opposite-dir? (snake-dir sn2) d)
                 (cons? (rest (snake-segs sn2)))))
           (stop-with w)]
          [(= n 1)
           (pit (snake-change-dir sn1 d) sn2 (pit-goos w) (pit-obstacles w))]
          [else
           (pit sn1 (snake-change-dir sn2 d) (pit-goos w) (pit-obstacles w))]))
    (if (or (key=? d "up")(key=? d "down")(key=? d "left")(key=? d "right"))
      (world-change-dir-aux w d 1)
      (world-change-dir-aux w d 2)))

(define (opposite-dir? d1 d2)
  (cond [(string=? d1 "up") (string=? d2 "down")]
        [(string=? d1 "down") (string=? d2 "up")]
        [(string=? d1 "left") (string=? d2 "right")]
        [(string=? d1 "right") (string=? d2 "left")]
        [(string=? d1 "w") (string=? d2 "s")]
        [(string=? d1 "s") (string=? d2 "w")]
        [(string=? d1 "a") (string=? d2 "d")]
        [(string=? d1 "d") (string=? d2 "a")]))

(define (render-pit w)
  (snakes+scene w))

(define (snakes+scene w)
  (define sn1 (pit-snake1 w))
  (define sn2 (pit-snake2 w))
  (snake1+scene sn1 (snake2+scene sn2 (goo-list+scene (pit-goos w) (obstacle-list+scene (pit-obstacles w) MT-SCENE)))))

(define (snake1+scene snake scene)
  (define snake-placar-scene
    (img+scene (posn 1 1) (text (number->string (snake-eaten snake)) 26 "red") scene))
  (define snake-body-scene
    (img-list+scene (snake-body snake) SEG1-IMG snake-placar-scene))
  (define dir
    (snake-dir snake))
  (img+scene (snake-head snake)
             (cond [(string=? "up" dir) HEAD1-UP-IMG]
                   [(string=? "down" dir) HEAD1-DOWN-IMG]
                   [(string=? "left" dir) HEAD1-LEFT-IMG]
                   [(string=? "right" dir) HEAD1-RIGHT-IMG])
             snake-body-scene))

(define (snake2+scene snake scene)
  (define snake-placar-scene
    (img+scene (posn (- DIMX 1) 1) (text (number->string (snake-eaten snake)) 26 "blue") scene))
  (define snake-body-scene
    (img-list+scene (snake-body snake) SEG2-IMG snake-placar-scene))
  (define dir
    (snake-dir snake))
  (img+scene (snake-head snake)
             (cond [(string=? "w" dir) HEAD2-UP-IMG]
                   [(string=? "s" dir) HEAD2-DOWN-IMG]
                   [(string=? "a" dir) HEAD2-LEFT-IMG]
                   [(string=? "d" dir) HEAD2-RIGHT-IMG])
             snake-body-scene))
(define (img-type tp)
  (cond [(= tp NORMAL) GOO-IMG]
        [else DIAMOND-IMG]))

(define (img-goos-list+scene posns types scene) 
  (cond [(empty? posns) scene]
        [else (img+scene
               (first posns)
               (img-type (first types))
               (img-goos-list+scene (rest posns) (rest types) scene))]))

  
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

(define (obstacle-list+scene obs scene)
  (cond [(empty? obs) scene]
        [else (img-list+scene
               (first obs)
               OBSTACLE-IMG
               (obstacle-list+scene (rest obs) scene))]))

  
(define (goo-list+scene goos scene)
  (define (get-posns-from-goos goos)
    (cond [(empty? goos) empty]
          [else (cons (goo-loc (first goos))
                      (get-posns-from-goos (rest goos)))]))
  (define (get-types-from-goos goos)
    (cond [(empty? goos) empty]
          [else (cons (goo-type (first goos))
                      (get-types-from-goos (rest goos)))]))
  (img-goos-list+scene (get-posns-from-goos goos) (get-types-from-goos goos) scene))

(define (dead? w)
  (define sn1 (pit-snake1 w))
  (define sn2 (pit-snake2 w))
  (define obs (pit-obstacles w))
  (or (self-colliding? sn1) (wall-colliding? sn1) (obstacles-colliding? sn1 obs)
      (self-colliding? sn2) (wall-colliding? sn2) (obstacles-colliding? sn2 obs)
      (snake-colliding? sn1 sn2)))

(define (snake-colliding? sn1 sn2)
  (or
   (member-posn? (snake-head sn1) (snake-body sn2))
   (member-posn? (snake-head sn2) (snake-body sn1))))

(define (render-end w)
  (overlay (text "GAME OVER" ENDGAME-TEXT-SIZE "black")
           (render-pit w)))


(define (obstacles-colliding? snake obs)
  (cond [(empty? obs) #f]
        [(obstacle-colliding? snake (first obs)) #t]
        [else (obstacles-colliding? snake (rest obs))]))
  
(define (obstacle-colliding? snake ob)
  (member-posn? (snake-head snake) ob))
                           
(define (pos-colliding? p ob)
  (cons? (member p ob)))

(define (member-posn? p l)
  (cond [(empty? l) #f]
        [(posn=? p (first l)) #t]
        [ else (member-posn? p (rest l))]))

(define (self-colliding? snake)
  (member-posn? (snake-head snake) (snake-body snake)))

(define (wall-colliding? snake)
  (define x (posn-x (snake-head snake)))
  (define y (posn-y (snake-head snake)))
  (or (= 0 x) (= x DIMX)
      (= 0 y) (= y DIMY)))

(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))

(define (snake-head sn)
  (first (snake-segs sn)))

(define (snake-body sn)
  (rest (snake-segs sn)))

(define (snake-tail sn)
  (last (snake-segs sn)))

(define (snake-change-dir sn d)
  (snake d (snake-segs sn) (snake-eaten sn)))

(define (start-snake)
  (define obst (list-obstacles (obst-number)))
  (big-bang (pit (snake "right" (list (posn 3 3)) 0)
                 (snake "a" (list (posn 27 17)) 0)
                 (list-goos (goos-number) obst)
                 obst)
    (on-tick next-pit TICK-RATE)
    (on-key direct-snake)
    (to-draw render-pit)
    (stop-when dead? render-end)))
  
  
