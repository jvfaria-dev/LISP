;;jogo + placar + número inicial de goos aleatório + goo especial (diamante)

#lang racket
(require 2htdp/universe 2htdp/image)

(struct pit (snake goos obstacles))
(struct snake (dir segs eaten));eaten é o contador de goos comidos (*****)
(struct obstacle (segs))
(struct posn (x y))
(struct goo (loc expire type))

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
(define SEG-IMG (bitmap "graphics/Body.png"))
(define HEAD-IMG (bitmap "graphics/Head.png"))

(define HEAD-LEFT-IMG HEAD-IMG)
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
  (define snake (pit-snake w)); primeiro ela separa a snake e os goos do mundo w
  (define goos (pit-goos w))              ; depois faz goo-to-eat ser uma lista onde o elemento da cabeça
  (define obst (pit-obstacles w))
  (define goo-to-eat (can-eat snake goos)); eh o goo que a snake come, caso contrario a lista eh vazia
  (if goo-to-eat ; se a lista goo-to-eat nao for vazia
      (pit (grow snake (goo-type goo-to-eat)) (age-goo (eat goos goo-to-eat obst) obst) obst); se verdadeiro, crescer a snake, retornado o mundo
      (pit (slither snake) (age-goo goos obst) obst))) ; se falso, movimenta a snake, renova os goos e retorna o mundo

;a snake s esta perto do goo g se estão na mesma posicao
(define (close? s g); s é uma snake e g um goo
  (posn=? s (goo-loc g)));retorna ser a snake s tem mesma posicao se o goo g

;comer o goo goo-to-eat da lista de goos é remover o goo-to-eat da lista goos
;e inserir um novo goo aleatorio na nova lista de goos
(define (eat goos goo-to-eat obst)
  (cons (fresh-goo (goo-type goo-to-eat) obst) (remove goo-to-eat goos)));retorna uma lista com um goo aleatorio na 
                           ;cabeça e na cauda a lista goos sem o goo que pode ser comido

;para crescer a snake sn basta criar uma nova snake com a mesma direcao
;e com segmentos (proxima cabeça) e os segmentos anteriores
(define (grow sn tp)
  (cond [(= tp NORMAL) 
         (snake (snake-dir sn) ;retorna uma snake com mesma direção
           (cons (next-head sn) (snake-segs sn))
           (add1 (snake-eaten sn)))];insere um novo segmento na cabeça de sn
        [else (grow (grow sn NORMAL) NORMAL)]))


;para movimentar a snake sn basta criar uma nova snake com a mesma direcao
;e com segmentos (proxima cabeça) e os segmentos anteriores menos o último
(define (slither sn)
  (snake (snake-dir sn)
         (cons (next-head sn) (all-but-last (snake-segs sn)))
         (snake-eaten sn)))

;funcao que retira o ultimo segmento da lista de segmentos da snake
(define (all-but-last segs)
  (cond [(empty? (rest segs)) empty]
        [else (cons (first segs) (all-but-last (rest segs)))]))

;funcao next-head recebe uma snake sn separa sua cabeça e sua direcao
;depois dependo da direcao retorna a proxima posicao da cabeca
(define (next-head sn)
  (define head (snake-head sn));head é o primeiro segumento da snake sn
  (define dir (snake-dir sn));dir é a direção da snake sn
  (cond [(string=? dir "up") (posn-move head 0 -1)];dependendo da direção dir
        [(string=? dir "down") (posn-move head 0 1)];será retornada uma estrutura
        [(string=? dir "left") (posn-move head -1 0)];como a próxima posição da
        [(string=? dir "right") (posn-move head 1 0)]));cabeça 

;a funcao posn-move aplica a um ponto p um deslocamento em x (dx) e em
;y (dy), retornando um no ponto
(define (posn-move p dx dy)
  (posn (+ (posn-x p) dx);retorna a estrutura posn com a nova posicao
        (+ (posn-y p) dy)))

;a funcao age-goo controla a idade dos goos, renovando-se quando expirarem
(define (age-goo goos obst)
  (rot (renew goos obst)));aplica a funcao rot, no resultado da função renew aplicada a goos

;a funcao rot aplica a funcao decay em todos os goos
(define (rot goos)
  (cond [(empty? goos) empty] ;se vazio goos parar e retornar empty (vazio)
        [else (cons (decay (first goos)) (rot (rest goos)))]));caso contrário,
        ;retorna uma lista formada com o primeiro elemento dos goos com expire subtraído 
        ;de 1 e a lista resultante de rot (recursivo) aplicado à cauda dos goos

;a funcao decay recebe um goo g e retorna um novo goo com expire reduzido de 1
(define (decay g)
  (goo (goo-loc g) (sub1 (goo-expire g)) (goo-type g)));mantem o loc de g e reduz o expire

;a funcao renew faz a renovacao da lista de goos trocando
;os goos expirados por goos novos aleatorios, retornado uma lista de goos
(define (renew goos obst)
  (cond [(empty? goos) empty];se a lista goos for vazia, retorna empty
        [(rotten? (first goos));se o primeiro de goos estiver expirado
         (cons (fresh-goo (goo-type (first goos)) obst) (renew (rest goos) obst))];retorna uma lista com o novo goo aleatório
        [else                                   ;como cabeça e cauda resultante do renew recursivo com a cauda da lista goos   
         (cons (first goos) (renew (rest goos) obst))]));senão, retorna uma lista com o mesmo goo da cabeça de goos
                                                   ;e cauda resultante do renew recursivo com a cauda da lista goos   
            
;a funcao rotten? verifa se um goo g está expirado (ou seja) tem expire zero
(define (rotten? g)
  (zero? (goo-expire g)));retorna #t ou #f

;a função recursiva list-obstacles retorna uma lista de n obstacles genéricos 
(define (list-obstacles n)
  (if (= n 0) 
      '()
      (cons (fresh-obstacle) (list-obstacles (sub1 n)))))

;funcao obstacles-number retorna o número de obstáculos que terá o jogo (***)
(define (obstacles-number)
  (add1 (random MAX-OBSTACLES)))

(define (build-obstacle len dir pos)
  (if (= len 0) '()
      (cons pos (build-obstacle (sub1 len) dir (next-pos pos dir)))))

(define (next-pos pos dir)
  (cond [(string=? dir "vert") (posn-move pos 1 0)]
        [(string=? dir "horiz") (posn-move pos 0 1)]))
       
;funcao fresh-obstacle retorna um obstacle aleatório com uma direcao 
(define (fresh-obstacle)
  (return-obstacle LEN-OBSTACLE (return-dir (random 2))))
  
(define (return-dir n)
  (cond [(= n 0) "horiz"]
        [(= n 1) "vert"]))
    
(define (return-obstacle len dir)  
  (build-obstacle len dir (posn (add1 (random (sub1 DIMX)))
                                (add1 (random (sub1 DIMY))))))
  
       
;a função recursiva list-goos retorna uma lista de n goos genéricos 
(define (list-goos n obst)
  (if (= n 1) 
      (cons (fresh-goo DIAMOND obst) '())
      (cons (fresh-goo NORMAL obst) (list-goos (sub1 n) obst))))

;funcao obst-number retorna o número de goos que terá o jogo (***)
(define (obst-number)
  (max MIN-OBSTACLES (add1 (random MAX-OBSTACLES))))


;funcao goos-number retorna o número de goos que terá o jogo (***)
(define (goos-number)
  (max MIN-GOOS (add1 (random MAX-GOOS))))

;funcao fresh-goo retorna um goo aleatório com expire de valor EXPIRATION-TIME
(define (fresh-goo tp obst)
  (define pos (posn (add1 (random (sub1 DIMX))) (add1 (random (sub1 DIMY)))))
  (cond [(pos-colliding? pos obst) (fresh-goo tp obst)]
        [else (goo pos EXPIRATION-TIME tp)]));retorna o goo

;funcao direct-snake 
(define (direct-snake w ke)
  (cond [(dir? ke) (world-change-dir w ke)]
        [else w]))

;funcao dir? retorna #t ou #f verificando se x é são teclas de direção
(define (dir? x)
  (or (key=? x "up")
      (key=? x "down")
      (key=? x "left")
      (key=? x "right")));retorna o resultado do or

;funcao world-change-dir recebe como argumentos o mundo w e a direção d
;retornando um novo mundo onde a snake tem a direção d
(define (world-change-dir w d)
  (define the-snake (pit-snake w));determina a snake do mundo w (mundo é a picture)
  (cond [(and (opposite-dir? (snake-dir the-snake) d);se a direção for a oposta
              (cons? (rest (snake-segs the-snake))));e existirem segmentos na cauda da snake
         (stop-with w)];fim de jogo, parar com o mundo w
        [else ;caso contrário
         (pit (snake-change-dir the-snake d)
              (pit-goos w)
              (pit-obstacles w))]));retorna um novo mundo
          ;onde a snake tem nova direção e os goos são mantidos

;funcao opposite-dir? retorna #t ou #f se as direções d1 e d2 são opostas
(define (opposite-dir? d1 d2)
  (cond [(string=? d1 "up") (string=? d2 "down")];testa o d1 e retorna do teste do d2
        [(string=? d1 "down") (string=? d2 "up")];veja que não foi necessário o uso 
        [(string=? d1 "left") (string=? d2 "right")];de and
        [(string=? d1 "right") (string=? d2 "left")]))

;funcao render-pit recebe um mundo w, retornando-o em um mundo inicial MT-SCENE
(define (render-pit w)
  (snake+scene (pit-snake w)
               (goo-list+scene (pit-goos w)
               (obstacle-list+scene (pit-obstacles w) MT-SCENE))))

;funcao snake+scene recebe uma snake e uma cena scene, retornando
;uma nova cena com a snake adicionada à cena scene
(define (snake+scene snake scene)
  (define snake-placar-scene 
    (img+scene (posn 1 1 ) (text (number->string (snake-eaten snake)) 28 "red") scene)); coloca o placar (*****)
  (define snake-body-scene
    (img-list+scene (snake-body snake) SEG-IMG snake-placar-scene))
  (define dir (snake-dir snake))
  (img+scene (snake-head snake);insere a cabeça da snake na scene
             (cond [(string=? "up" dir) HEAD-UP-IMG]
                   [(string=? "down" dir) HEAD-DOWN-IMG]
                   [(string=? "left" dir) HEAD-LEFT-IMG]
                   [(string=? "right" dir) HEAD-RIGHT-IMG])
             snake-body-scene))


;função img-type retorna um bitmap para o goo conforme seu tipo
(define (img-type tp)
  (cond [(= tp NORMAL) GOO-IMG]
        [else DIAMOND-IMG]))

;funcao imp-goos-list+scene recebe uma lista de posições posns, tipos das imagnes e uma cena scene
;então retorna uma cena nova com goos do tipo type inseridos nas posns na cena original scene
(define (img-goos-list+scene posns types scene) 
  (cond [(empty? posns) scene]
        [else (img+scene
               (first posns)
               (img-type (first types))
               (img-goos-list+scene (rest posns) (rest types) scene))]))

  

;funcao imp-list+scene recebe uma lista de posições posns, uma imagem img e uma cena scene
;então retorna uma cena nova com a imagem img inserida nas posns na cena original scene
(define (img-list+scene posns img scene)
  (cond [(empty? posns) scene]
        [else (img+scene
               (first posns)
               img
               (img-list+scene (rest posns) img scene))]))

;funcao imp+scene coloca a imagem img na posn na cena scene
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


;função goo-list+scene coloca a lista de goos na cena scene   
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
  (define snake (pit-snake w))
  (define obs (pit-obstacles w))
  (or (self-colliding? snake) (wall-colliding? snake) (obstacles-colliding? snake obs)))

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
                 (list-goos (goos-number) obst)
                 obst)
            (on-tick next-pit TICK-RATE)
            (on-key direct-snake)
            (to-draw render-pit)
            (stop-when dead? render-end)))
