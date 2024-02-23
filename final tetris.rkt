;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |final tetris|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;;;; Bin-Packing Game

(define SCENE (empty-scene 200 400))

;;; Data Definitions

(define-struct brick [x y color])
;; A Brick is a (make-brick Number Number Color)

;; A Pt (2D Point) is a (make-posn Integer Integer)

(define-struct tetra [center bricks])
;; A Tetra is a (make-tetra Pt Bricks)
;; The center point is the point around which the tetra rotates when it spins

;; A Bricks (Set of Bricks) is one of:
; - empty
; - (cons Brick Bricks)
; order does not matter

(define-struct world [tetra pile score])
;; A World is a (make-world Tetra Bricks Number)
;; The set of bricks represents the pile of bricks at the bottom of the screen

;;; Examples
(define BRICK1 (make-brick 100 50 'green))
(define BRICK2 (make-brick 50 390 'blue))
(define BRICK3 (make-brick 70 300 'pink))
(define BRICK4 (make-brick 60 20 'purple))
(define BRICK5 (make-brick 60 20 'red))
(define BRICK6 (make-brick 100 390 'cyan))
(define BRICK7 (make-brick 20 100 'pink))
(define BRICK8 (make-brick 100 10 'purple))
(define BRICK9 (make-brick 0 50 'blue))
(define BRICKS1 (cons BRICK1 (cons BRICK2 '())))
(define BRICKS2 (cons BRICK1 '()))
(define BRICKS3 (cons BRICK1 (cons BRICK3 '())))
(define BRICKS4 (cons BRICK1 (cons BRICK2 (cons BRICK4 '()))))
(define BRICKS5 (cons BRICK2 (cons BRICK5 '())))
(define BRICKS6 (cons BRICK1 (cons BRICK3 (cons BRICK6 '()))))
(define BRICKS7 (cons BRICK2 (cons BRICK9 '())))
(define BRICKS8 (cons BRICK7 (cons BRICK8 '())))
(define BRICKS20 (list BRICK4 BRICK5))
(define PT1 (make-posn 100 50))
(define PT2 (make-posn 50 250))
(define PT3 (make-posn 60 20))
(define TICK-RATE 0.3)

;; Tetra Functions

; Number Number -> Tetra
; create tetra O
(define (tetra-O x y)
  (make-tetra (make-posn x y)
              (list (make-brick x y 'green)
                    (make-brick (+ x 20) y 'green)
                    (make-brick (+ x 20) (+ y 20) 'green)
                    (make-brick x (+ y 20) 'green))))

(define O-START (tetra-O 100 10))
(check-expect (tetra-O 100 30) (make-tetra (make-posn 100 30)
                                           (list (make-brick 100 30 'green)
                                                 (make-brick 120 30 'green)
                                                 (make-brick 120 50 'green)
                                                 (make-brick 100 50 'green))))
(check-expect (tetra-O 20 40) (make-tetra (make-posn 20 40)
                                          (list (make-brick 20 40 'green)
                                                (make-brick 40 40 'green)
                                                (make-brick 40 60 'green)
                                                (make-brick 20 60 'green))))

; Number Number -> Tetra
; create tetra I
(define (tetra-I x y)
  (make-tetra (make-posn x y)
              (list (make-brick x y 'blue)
                    (make-brick (+ x 20) y 'blue)
                    (make-brick (+ x 40) y 'blue)
                    (make-brick (+ x 60) y 'blue))))

(define I-START (tetra-I 100 10))
(check-expect (tetra-I 100 30) (make-tetra (make-posn 100 30)
                                           (list (make-brick 100 30 'blue)
                                                 (make-brick 120 30 'blue)
                                                 (make-brick 140 30 'blue)
                                                 (make-brick 160 30 'blue))))
(check-expect (tetra-I 20 40) (make-tetra (make-posn 20 40)
                                          (list (make-brick 20 40 'blue)
                                                (make-brick 40 40 'blue)
                                                (make-brick 60 40 'blue)
                                                (make-brick 80 40 'blue))))
 
; Number Number -> Tetra
; create tetra L
(define (tetra-L x y)
  (make-tetra (make-posn x y)
              (list (make-brick x y 'purple)
                    (make-brick x (- y 20) 'purple)
                    (make-brick (- x 20) y 'purple)
                    (make-brick (- x 40) y 'purple))))

(define L-START (tetra-L 100 10))
(check-expect (tetra-L 100 30) (make-tetra (make-posn 100 30)
                                           (list (make-brick 100 30 'purple)
                                                 (make-brick 100 10 'purple)
                                                 (make-brick 80 30 'purple)
                                                 (make-brick 60 30 'purple))))
(check-expect (tetra-L 40 40) (make-tetra (make-posn 40 40)
                                          (list (make-brick 40 40 'purple)
                                                (make-brick 40 20 'purple)
                                                (make-brick 20 40 'purple)
                                                (make-brick 0 40 'purple))))

; Number Number -> Tetra
; create tetra J
(define (tetra-J x y)
  (make-tetra (make-posn x y)
              (list (make-brick x y 'cyan)
                    (make-brick x (- y 20) 'cyan)
                    (make-brick (+ x 20) y 'cyan)
                    (make-brick (+ x 40) y 'cyan))))

(define J-START (tetra-J 100 10))
(define J1 (tetra-J 20 200))
(check-expect (tetra-J 100 30) (make-tetra (make-posn 100 30)
                                           (list (make-brick 100 30 'cyan)
                                                 (make-brick 100 10 'cyan)
                                                 (make-brick 120 30 'cyan)
                                                 (make-brick 140 30 'cyan))))
(check-expect (tetra-J 20 40) (make-tetra (make-posn 20 40)
                                          (list (make-brick 20 40 'cyan)
                                                (make-brick 20 20 'cyan)
                                                (make-brick 40 40 'cyan)
                                                (make-brick 60 40 'cyan))))
                                                
; Number Number -> Tetra
; create tetra T
(define (tetra-T x y)
  (make-tetra (make-posn x y)
              (list (make-brick x y 'orange)
                    (make-brick (+ x 20) y 'orange)
                    (make-brick (- x 20) y 'orange)
                    (make-brick x (- y 20) 'orange))))

(define T-START (tetra-T 100 10))
(define T1 (tetra-T 100 30))
(check-expect (tetra-T 100 30) (make-tetra (make-posn 100 30)
                                           (list (make-brick 100 30 'orange)
                                                 (make-brick 120 30 'orange)
                                                 (make-brick 80 30 'orange)
                                                 (make-brick 100 10 'orange))))
(check-expect (tetra-T 20 40) (make-tetra (make-posn 20 40)
                                          (list (make-brick 20 40 'orange)
                                                (make-brick 40 40 'orange)
                                                (make-brick 0 40 'orange)
                                                (make-brick 20 20 'orange))))

; Number Number -> Tetra
; create tetra Z
(define (tetra-Z x y)
  (make-tetra (make-posn x y)
              (list (make-brick x y 'pink)
                    (make-brick (- x 20) y 'pink)
                    (make-brick x (+ y 20) 'pink)
                    (make-brick (+ x 20) (+ y 20) 'pink))))

(define Z-START (tetra-Z 100 10))
(define Z-WALL (tetra-Z 0 50))
(check-expect (tetra-Z 100 30) (make-tetra (make-posn 100 30)
                                           (list (make-brick 100 30 'pink)
                                                 (make-brick 80 30 'pink)
                                                 (make-brick 100 50 'pink)
                                                 (make-brick 120 50 'pink))))
(check-expect (tetra-Z 20 40) (make-tetra (make-posn 20 40)
                                          (list (make-brick 20 40 'pink)
                                                (make-brick 0 40 'pink)
                                                (make-brick 20 60 'pink)
                                                (make-brick 40 60 'pink))))

; Number Number -> Tetra
; create tetra S
(define (tetra-S x y)
  (make-tetra (make-posn x y)
              (list (make-brick x y 'red)
                    (make-brick (+ x 20) y 'red)
                    (make-brick x (+ y 20) 'red)
                    (make-brick (- x 20) (+ y 20) 'red))))

(define S-START (tetra-S 100 10))
(define S-MID (tetra-S 100 250))
(define S-END (tetra-S 100 390))
(check-expect (tetra-S 100 250) (make-tetra (make-posn 100 250)
                                            (list (make-brick 100 250 'red)
                                                  (make-brick 120 250 'red)
                                                  (make-brick 100 270 'red)
                                                  (make-brick 80 270 'red))))
(check-expect (tetra-S 20 40) (make-tetra (make-posn 20 40)
                                          (list (make-brick 20 40 'red)
                                                (make-brick 40 40 'red)
                                                (make-brick 20 60 'red)
                                                (make-brick 0 60 'red))))

;; To-Draw Helpers

; brick-image : Brick -> Image
; create a brick image
(define (brick-image b)
  (overlay (square 20 'outline 'black)
           (square 20 'solid (brick-color b))))

(check-expect (brick-image BRICK1) (overlay (square 20 'outline 'black)
                                            (square 20 'solid 'green)))
(check-expect (brick-image BRICK5) (overlay (square 20 'outline 'black)
                                            (square 20 'solid 'red)))

; brick+scene : Brick Image -> Image
; place a brick on a scene at a point
(define (brick+scene b bg)
  (place-image (brick-image b)
               (brick-x b) (brick-y b)
               bg))

(check-expect (brick+scene BRICK1 SCENE) (place-image (brick-image BRICK1)
                                                      100 50
                                                      SCENE))
(check-expect (brick+scene BRICK5 SCENE) (place-image (brick-image BRICK5)
                                                      60 20
                                                      SCENE))

; bricks+scene : Bricks Image -> Image
; place bricks on a scene
(define (bricks+scene lob bg)
  (foldr brick+scene bg lob))

(check-expect (bricks+scene '() SCENE) SCENE)
(check-expect (bricks+scene BRICKS2 SCENE) (place-image (brick-image BRICK1)
                                                        100 50
                                                        SCENE))

; tetra-image : Tetra Image -> Image
; create an image of a tetra
(define (tetra-image t bg)
  (bricks+scene (tetra-bricks t) bg))

(check-expect (tetra-image S-START SCENE) (bricks+scene (tetra-bricks S-START) SCENE))
(check-expect (tetra-image Z-WALL SCENE) (bricks+scene (tetra-bricks Z-WALL) SCENE))

; world-image : World -> Image
; create an image of a world
(define (world-image w)
  (tetra-image (world-tetra w) (bricks+scene (world-pile w) SCENE)))

(check-expect (world-image WORLD8) (tetra-image (world-tetra WORLD8)
                                                (bricks+scene (world-pile WORLD8) SCENE)))
(check-expect (world-image WORLD10) (tetra-image (world-tetra WORLD10)
                                                 (bricks+scene (world-pile WORLD10) SCENE)))
                 
;; On-Tick Helpers

; next-center : Pt -> Pt
; move the pt down
(define (next-center pt)
  (make-posn (posn-x pt) (+ (posn-y pt) 20)))

(check-expect (next-center PT1) (make-posn 100 70))
(check-expect (next-center PT2) (make-posn 50 270))

; next-brick : Brick -> Brick
; move the brick down
(define (next-brick b)
  (make-brick (brick-x b) (+ (brick-y b) 20) (brick-color b)))

(check-expect (next-brick BRICK1) (make-brick 100 70 'green))
(check-expect (next-brick BRICK3) (make-brick 70 320 'pink))

; next-bricks : Bricks -> Bricks
; move the bricks down
(define (next-bricks lob)
  (map next-brick lob))

(check-expect (next-bricks BRICKS2) (list (make-brick 100 70 'green)))
(check-expect (next-bricks BRICKS3) (list (make-brick 100 70 'green) (make-brick 70 320 'pink)))

; next-tetra : Tetra -> Tetra
; move the tetra down
(define (next-tetra t)
  (make-tetra (next-center (tetra-center t))
              (next-bricks (tetra-bricks t))))

(check-expect (next-tetra S-START) (make-tetra (make-posn 100 30)
                                               (list (make-brick 100 30 'red)
                                                     (make-brick 120 30 'red)
                                                     (make-brick 100 50 'red)
                                                     (make-brick 80 50 'red))))
(check-expect (next-tetra S-MID) (make-tetra (make-posn 100 270)
                                             (list (make-brick 100 270 'red)
                                                   (make-brick 120 270 'red)
                                                   (make-brick 100 290 'red)
                                                   (make-brick 80 290 'red))))

; on-bottom-brick? : Brick -> Boolean
; is the brick at the bottom of the game?
(define (on-bottom-brick? b)
  (= (brick-y b) 390))

(check-expect (on-bottom-brick? BRICK1) #f)
(check-expect (on-bottom-brick? BRICK2) #t)

; on-bottom-bricks? Bricks -> Boolean
; are the bricks at the bottom of the game?
(define (on-bottom-bricks? lob)
  (ormap on-bottom-brick? lob))

(check-expect (on-bottom-bricks? BRICKS1) #t)
(check-expect (on-bottom-bricks? BRICKS2) #f)

; on-bottom-tetra? : Tetra -> Boolean
; is the tetra at the bottom of the game?
(define (on-bottom-tetra? t)
  (on-bottom-bricks? (tetra-bricks t)))

(check-expect (on-bottom-tetra? S-START) #f)
(check-expect (on-bottom-tetra? S-END) #t)

; rando : Number -> Tetra
; create a random tetra 
(define (rando x)
  (cond
    [(= x 0) O-START]
    [(= x 1) I-START]
    [(= x 2) L-START]
    [(= x 3) J-START]
    [(= x 4) T-START]
    [(= x 5) Z-START]
    [(= x 6) S-START]))

(check-expect (rando 6) S-START)
(check-expect (rando 3) J-START)

; touching? : Brick Brick -> Boolean
; are the 2 bricks touching each other?
(define (touching? a b)
  (and (= (brick-x a) (brick-x b))
       (= (brick-y a) (brick-y b))))

(check-expect (touching? BRICK1 BRICK2) #f)
(check-expect (touching? BRICK4 BRICK5) #t)

; touching-pile? : Brick Bricks -> Boolean
; does the brick touch the bricks?
(define (touching-pile? a lob)
  (ormap (λ (x) (touching? a x)) lob))

(check-expect (touching-pile? BRICK2 BRICKS1) #t)
(check-expect (touching-pile? BRICK4 BRICKS3) #f)

; touching-bricks? : Bricks Bricks -> Boolean
; do 2 bricks touch each other?
(define (touching-bricks? alob blob)
  (ormap (λ (x) (touching-pile? x blob)) alob))

(check-expect (touching-bricks? BRICKS1 BRICKS2) #t)
(check-expect (touching-bricks? BRICKS2 BRICKS5) #f) 

; overlaps? : Tetra Bricks -> Boolean
; do the tetras and pile overlap?
(define (overlaps? t pile)
  (touching-bricks? (tetra-bricks t) pile))

(check-expect (overlaps? S-START BRICKS2) #f)
(check-expect (overlaps? S-END BRICKS6) #t)
   
; next-world: World-> World
; presents the next world with the tetra falling
(define (next-world w)
  (cond
    [(or (overlaps? (next-tetra (world-tetra w)) (world-pile w))
         (on-bottom-tetra? (world-tetra w)))
     (delete-rows (make-world (rando (random 7))
                              (append (tetra-bricks (world-tetra w))
                                      (world-pile w))
                              (score (world-pile w))))]
    [else (make-world (next-tetra (world-tetra w))
                      (world-pile w)
                      (score (world-pile w)))]))

(check-expect (next-world WORLD8) (make-world (next-tetra (world-tetra WORLD8))
                                              (world-pile WORLD8)
                                              (score (world-pile WORLD8))))
                                                                                                 
;; On-Key Helpers

; brick-left : Brick -> Brick
; move a brick to the left
(define (brick-left b)
  (make-brick (- (brick-x b) 20) (brick-y b) (brick-color b)))

(check-expect (brick-left BRICK1) (make-brick 80 50 'green))
(check-expect (brick-left BRICK3) (make-brick 50 300 'pink))

; bricks-left : Bricks -> Bricks
; move bricks to the left
(define (bricks-left lob)
  (map brick-left lob))
  
(check-expect (bricks-left BRICKS1) (list (brick-left BRICK1) (brick-left BRICK2)))
(check-expect (bricks-left BRICKS3) (list (brick-left BRICK1) (brick-left BRICK3)))

; pt-left : Pt -> Pt
; move the center to the left
(define (pt-left pt)
  (make-posn (- (posn-x pt) 20) (posn-y pt)))

(check-expect (pt-left PT1) (make-posn 80 50))
(check-expect (pt-left PT2) (make-posn 30 250))

; tetra-left : Tetra -> Tetra
; move a tetra to the left
(define (tetra-left t)
  (make-tetra (pt-left (tetra-center t)) (bricks-left (tetra-bricks t))))

(check-expect (tetra-left S-MID)
              (make-tetra (make-posn 80 250) (list (make-brick 80 250 'red)
                                                   (make-brick 100 250 'red)
                                                   (make-brick 80 270 'red)
                                                   (make-brick 60 270 'red))))
(check-expect (tetra-left T1) (make-tetra (make-posn 80 30)
                                          (list (make-brick 80 30 'orange)
                                                (make-brick 100 30 'orange)
                                                (make-brick 60 30 'orange)
                                                (make-brick 80 10 'orange))))
                                                
; world-left : World -> World
; move the world to the left
(define (world-left w)
  (cond [(hit-wall-world? w) w]
        [else (make-world (tetra-left (world-tetra w))
                          (world-pile w)
                          (score (world-pile w)))]))

(check-expect (world-left WORLD10) WORLD10)
(check-expect (world-left WORLD8) (make-world (tetra-left (world-tetra WORLD8))
                                              (world-pile WORLD8)
                                              (score (world-pile WORLD8))))
; brick-right : Brick -> Brick
; move a brick to the right
(define (brick-right b)
  (make-brick (+ (brick-x b) 20) (brick-y b) (brick-color b)))

(check-expect (brick-right BRICK1) (make-brick 120 50 'green))
(check-expect (brick-right BRICK3) (make-brick 90 300 'pink))

; bricks-right : Bricks -> Bricks
; move bricks to the right
(define (bricks-right lob)
  (map brick-right lob))

(check-expect (bricks-right BRICKS1) (list (brick-right BRICK1) (brick-right BRICK2)))
(check-expect (bricks-right BRICKS3) (list (brick-right BRICK1) (brick-right BRICK3)))

; pt-right : Pt -> Pt
; move the center to the right
(define (pt-right pt)
  (make-posn (+ (posn-x pt) 20)  (posn-y pt)))

(check-expect (pt-right PT1) (make-posn 120 50))
(check-expect (pt-right PT2) (make-posn 70 250))

; tetra-right : Tetra -> Tetra
; move a tetra to the right
(define (tetra-right t)
  (make-tetra (pt-right (tetra-center t)) (bricks-right (tetra-bricks t))))

(check-expect (tetra-right S-MID)
              (make-tetra (make-posn 120 250) (list (make-brick 120 250 'red)
                                                    (make-brick 140 250 'red)
                                                    (make-brick 120 270 'red)
                                                    (make-brick 100 270 'red))))
(check-expect (tetra-right J1)
              (make-tetra (make-posn 40 200) (list (make-brick 40 200 'cyan)
                                                   (make-brick 40 180 'cyan)
                                                   (make-brick 60 200 'cyan)
                                                   (make-brick 80 200 'cyan))))

; world-right : World -> World
; move the world to the right
(define (world-right w)
  (cond [(hit-wall-world? w) w]
        [else (make-world (tetra-right (world-tetra w))
                          (world-pile w)
                          (score (world-pile w)))]))

(check-expect (world-right WORLD10) WORLD10)
(check-expect (world-right WORLD8) (make-world (tetra-right (world-tetra WORLD8))
                                               (world-pile WORLD8)
                                               (score (world-pile WORLD8))))
; hit-wall-brick? : Brick -> Boolean
; does a brick hit the wall?
(define (hit-wall-brick? b)
  (or (= (brick-x b) 0)
      (= (brick-x b) 200)))

(check-expect (hit-wall-brick? BRICK1) #f)
(check-expect (hit-wall-brick? BRICK9) #t)

; hit-wall-bricks? : Bricks -> Boolean
; do bricks hit the wall?
(define (hit-wall-bricks? lob)
  (ormap hit-wall-brick? lob))

(check-expect (hit-wall-bricks? BRICKS1) #f)
(check-expect (hit-wall-bricks? BRICKS7) #t)

; hit-wall-tetra? : Tetra -> Boolean
; does a tetra hit the wall?
(define (hit-wall-tetra? t)
  (hit-wall-bricks? (tetra-bricks t)))
 
(check-expect (hit-wall-tetra? S-START) #f)
(check-expect (hit-wall-tetra? Z-WALL) #t)

; hit-wall-world? : World -> Boolean
; does the world hit the wall?
(define (hit-wall-world? w)
  (hit-wall-tetra? (world-tetra w)))

(check-expect (hit-wall-world? WORLD8) #f)
(check-expect (hit-wall-world? WORLD10) #t)

; brick-rotate-ccw : Brick Pt -> Brick
; rotate the brick 90 counterclockwise around the posn.
(define (brick-rotate-ccw sqr c)
  (make-brick (+ (posn-x c)
                 (- (posn-y c)
                    (brick-y sqr)))
              (+ (posn-y c)
                 (- (brick-x sqr)
                    (posn-x c)))
              (brick-color sqr)))

(check-expect (brick-rotate-ccw BRICK1 PT1) (make-brick 100 50 'green))
(check-expect (brick-rotate-ccw BRICK4 PT3) (make-brick 60 20 'purple))

; bricks-rotate-ccw : Bricks Pt -> Bricks
; rotate bricks 90 counterclockwise around the posn 
(define (bricks-rotate-ccw lob c)
  (map (λ (x) (brick-rotate-ccw x c)) lob))

(check-expect (bricks-rotate-ccw BRICKS2 PT1) (list (make-brick 100 50 'green)))
(check-expect (bricks-rotate-ccw BRICKS20 PT3) (list (make-brick 60 20 'purple)
                                                     (make-brick 60 20 'red)))

; tetra-rotate-ccw-a : Tetra -> Tetra
; rotate a tetra 90 degrees counterclockwise around its center point
(define (tetra-rotate-ccw-a t)
  (make-tetra (tetra-center t) (bricks-rotate-ccw (tetra-bricks t) (tetra-center t))))

(check-expect (tetra-rotate-ccw-a S-MID) (make-tetra (tetra-center S-MID)
                                                     (bricks-rotate-ccw (tetra-bricks S-MID)
                                                                        (tetra-center S-MID))))
(check-expect (tetra-rotate-ccw-a J1) (make-tetra (tetra-center J1)
                                                  (bricks-rotate-ccw (tetra-bricks J1)
                                                                     (tetra-center J1))))

; tetra-rotate-cw-s : Tetra -> Tetra
; rotate a tetra 90 degrees clockwise around its center point
(define (tetra-rotate-cw-s t)
  (make-tetra (tetra-center t) (bricks-rotate-ccw
                                (bricks-rotate-ccw
                                 (bricks-rotate-ccw (tetra-bricks t) (tetra-center t))
                                 (tetra-center t))
                                (tetra-center t))))

(check-expect (tetra-rotate-cw-s S-MID) (make-tetra (tetra-center S-MID)
                                                    (bricks-rotate-ccw
                                                     (bricks-rotate-ccw
                                                      (bricks-rotate-ccw (tetra-bricks S-MID)
                                                                         (tetra-center S-MID))
                                                      (tetra-center S-MID))
                                                     (tetra-center S-MID))))
(check-expect (tetra-rotate-cw-s J1) (make-tetra (tetra-center J1)
                                                 (bricks-rotate-ccw
                                                  (bricks-rotate-ccw
                                                   (bricks-rotate-ccw (tetra-bricks J1)
                                                                      (tetra-center J1))
                                                   (tetra-center J1))
                                                  (tetra-center J1))))

; world-rotate-a : World -> World
; rotate a world 90 degrees counterclockwise around its center
(define (world-rotate-a w)
  (cond [(hit-wall-world? w) w]
        [else (make-world (tetra-rotate-ccw-a (world-tetra w))
                          (world-pile w)
                          (score (world-pile w)))]))

(check-expect (world-rotate-a WORLD10) WORLD10)
(check-expect (world-rotate-a WORLD8) (make-world (tetra-rotate-ccw-a (world-tetra WORLD8))
                                                  (world-pile WORLD8)
                                                  (score (world-pile WORLD8)))) 

; world-rotate-s : World -> World
; rotate a world 90 degrees clockwise around its center
(define (world-rotate-s w)
  (cond [(hit-wall-world? w) w]
        [else (make-world (tetra-rotate-cw-s (world-tetra w))
                          (world-pile w)
                          (score (world-pile w)))]))

(check-expect (world-rotate-s WORLD10) WORLD10)
(check-expect (world-rotate-s WORLD8) (make-world (tetra-rotate-cw-s (world-tetra WORLD8))
                                                  (world-pile WORLD8)
                                                  (score (world-pile WORLD8))))
; change : World Key -> KE
; keys moving tetra pieces
(define (change w a-key)
  (cond [(key=? a-key "left") (if (hit-wall-world? (world-left w))
                                  w
                                  (world-left w))]
        [(key=? a-key "right") (if (hit-wall-world? (world-right w))
                                   w
                                   (world-right w))]
        [(key=? a-key "a") (if (hit-wall-world? (world-rotate-a w))
                               w
                               (world-rotate-a w))]
        [(key=? a-key "s") (if (hit-wall-world? (world-rotate-s w))
                               w
                               (world-rotate-s w))]
        [else w]))

(check-expect (change WORLD10 "a") WORLD10)
(check-expect (change WORLD8 "s") (world-rotate-s WORLD8))
(check-expect (change WORLD10 "right") WORLD10)
(check-expect (change WORLD8 "left") (world-left WORLD8))

;; Stop-When Helpers

; hit-top-brick? : Brick -> Boolean
; does the brick hit the top of the game?
(define (hit-top-brick? b)
  (<= (brick-y b) 10))

(check-expect (hit-top-brick? BRICK8) #t)
(check-expect (hit-top-brick? BRICK1) #f)

; hit-top-bricks? : Bricks -> Boolean
; do the bricks hit the top of the game?
(define (hit-top-bricks? lob)
  (ormap hit-top-brick? lob))

(check-expect (hit-top-bricks? BRICKS8) #t)
(check-expect (hit-top-bricks? BRICKS1) #f)

; hit-top-tetra? : Tetra -> Boolean
; does the tetra hit the top of the game?
(define (hit-top-tetra? t)
  (hit-top-bricks? (tetra-bricks t)))

(check-expect (hit-top-tetra? S-START) #t)
(check-expect (hit-top-tetra? Z-WALL) #f)

; hit-top-world? : World -> Boolean
; does the world hit the top of the game?
(define (hit-top-world? w)
  (hit-top-bricks? (world-pile w)))

(check-expect (hit-top-world? WORLD10) #f)
(check-expect (hit-top-world? WORLD11) #t)


;; GAME

; score : Bricks -> Number
; calculate score of game (how many bricks are in the pile)
(define (score lob)
  (foldr (λ (x y) (if (brick? x) (+ 1 y) y)) 0 lob))

(check-expect (score BRICKS1) 2)
(check-expect (score '()) 0)

; game : WorldState -> Worldstate
; play the game
(define (game worldstate)
  (big-bang worldstate
    [to-draw world-image]
    [on-tick next-world TICK-RATE]
    [on-key change]
    [stop-when hit-top-world?]))

(define WORLD8 (make-world S-START '() 0))
(define WORLD9 (make-world L-START '() 0))
(define WORLD10 (make-world Z-WALL '() 0))
(define WORLD11 (make-world J1 BRICKS8 0))
(define BRICKSALL (list (make-brick 20 50 'pink)
                        (make-brick 40 50 'pink)
                        (make-brick 60 50 'pink)
                        (make-brick 80 50 'pink)
                        (make-brick 100 50 'pink)
                        (make-brick 120 50 'pink)
                        (make-brick 140 50 'pink)
                        (make-brick 160 50 'pink)
                        (make-brick 180 50 'pink)))
(define BRICKSALL1 (list (make-brick 20 70 'pink)
                         (make-brick 40 70 'pink)
                         (make-brick 60 70 'pink)
                         (make-brick 80 70 'pink)
                         (make-brick 100 70 'pink)
                         (make-brick 120 70 'pink)
                         (make-brick 140 70 'pink)
                         (make-brick 160 70 'pink)
                         (make-brick 180 70 'pink)))
(define PILE1 (append BRICKSALL BRICKS8))
(define PILE2 (append BRICKSALL BRICKSALL1 BRICKS8))
(define WORLD1 (make-world J1 PILE1 11))
(define WORLD2 (make-world J1 PILE2 0))

; full? : Number Bricks -> Boolean
; returns true if row is full of bricks
(define (full? n lob)
  (= 9 (length (filter (λ (x) (= (brick-y x) n)) lob))))

(check-expect (full? 50 BRICKS2) #f)
(check-expect (full? 50 BRICKSALL) #t)

; full-rows : Bricks -> [Listof Integers]
; y-coordinates that are full of bricks
(define (full-rows lob)
  (filter (λ (x) (full? x lob)) (build-list 20 (λ (x) (+ (* x 20) 10)))))

(check-expect (full-rows BRICKSALL) (list 50))             
(check-expect (full-rows BRICKS2) '())


; delete-row : Number World -> World
; remove bricks that hae the same y coordinate as given n value and move bricks above
; the n value down a row 
(define (delete-row n w)
  (make-world (world-tetra w)
              (map (λ (x) (if (< (brick-y x) n)
                              (next-brick x)
                              x))
                   (filter (λ (x) (not (= n (brick-y x)))) (world-pile w)))
              (score (world-pile w)))) 
                  
(check-expect (delete-row 50 WORLD1) (make-world (world-tetra WORLD1)
                                                 (list BRICK7 (next-brick BRICK8))
                                                 (score (world-pile WORLD1))))
(check-expect (delete-row 70 WORLD2) (make-world (world-tetra WORLD2)
                                                 (append BRICKSALL1
                                                         (list BRICK7 (next-brick BRICK8)))
                                                 (score (world-pile WORLD2))))

; delete-rows : World --> World
; deletes all the rows that are filled with bricks 
(define (delete-rows w)
  (foldr (λ (x y) (delete-row x y)) w (reverse (full-rows (world-pile w)))))

(check-expect (delete-rows WORLD2) (make-world (world-tetra WORLD2)
                                               (list
                                                (make-brick 20 100 'pink)
                                                (make-brick 100 50 'purple))
                                               (score (world-pile WORLD1))))
(check-expect (delete-rows WORLD1) (make-world (world-tetra WORLD1)
                                               (list BRICK7 (next-brick BRICK8))
                                               (score (world-pile WORLD1))))
















