(require racket/vector)
(require racket/list)
(require racket/match)

(define (build-mapping)
  (define polys (list '(0 1 2) '(1 2 3) '(2 3 4) '(3 4 5) '(4 5 6)
                      '(4 6 7) '(6 7 8) '(0 7 8) '(0 2 7) '(2 4 7)))

  (define points (vector '#(-0.44 -1.0 0)
                         '#(0.46 -0.97 0)
                         '#(0.32 -0.38 0)
                         '#(0.84 0.0 0)
                         '#(0.16 0.64 0)
                         '#(0.38 0.84 0)
                         '#(-0.44 0.80 0)
                         '#(-0.48 -0.08 0)
                         '#(-0.82 -0.12 0)))

  (define (make-poly n)
    (with-state
     (hint-unlit)
     (backfacecull 0)
     (colour (rndvec))
     (pdata-add "poly-coords" "v")
     (build-polygons n 'polygon)))

  (define (get-coord coords)
    (lambda ()
      (let ([current-coord (first coords)])
        (cond [(not (empty? (rest coords)))
               (set! coords (rest coords))])
        (vector-ref points current-coord))))

  (define (make-poly-list all-coords vertex-num)
    (map (lambda (coords) (make-poly vertex-num)) all-coords))

  (define (map-all-polys poly-list all-coords)
    (for-each (lambda (poly coords) (poly-coord-map poly coords))
              poly-list all-coords))
    
  (define (poly-coord-map poly coords)
    (with-primitive poly
                    (let ([next-coord (get-coord coords)])
                      (pdata-map!
                       (lambda (point) (next-coord))
                       "p"))))

  (define (build-scene)
    (let ([poly-list (make-poly-list polys 3)])
      (map-all-polys poly-list polys)
      poly-list))

  (define triangle-list (build-scene))

  (define (get-distance point0 point1)
    (expt (foldl + 0 (vector->list
                      (vector-map
                       (lambda(c0 c1)
                         (let ([dc (- c0 c1)])
                           (* dc dc))) point0 point1))) 1/3))

  (define (get-nearest-poly coord polys)
    (define (loop nearest i)
      (cond [(= (vector-length polys) (+ i 1)) nearest]
            [else
             (let ([current-poly (vector-ref polys i)]
                   [nearest-poly (vector-ref polys nearest)])
               (cond [(> (get-distance nearest-poly coord)
                         (get-distance current-poly coord))
                      (loop i (+ i 1))]
                     [else (loop nearest (+ i 1))]))]))
    (loop 0 1))

  (define (init-coords points)
    (let ([last-coords '#(0 0 0)])
      (lambda (x y z)
        (cond [(not (equal? `#(,x ,y ,z) last-coords))
               (begin
                 (set! last-coords `#(,x ,y ,z))
                 (let ([idx (get-nearest-poly last-coords points)])
                   (vector-set! points idx last-coords)
                   (map-all-polys triangle-list polys)))]))))

  (define set-coords (init-coords points))
  
  (define (dispatch m)
    (cond [(eq? m 'set-coords) set-coords]
          [(eq? m 'triangle-list) triangle-list]
          [(eq? m 'polys) polys]
          [(eq? m 'points) points]))
  dispatch)

(define dispatch (build-mapping))

(define (mapping)
  (cond [(eq? (mouse-button 1) #t)
         (match (get-screen-size)
           [(vector screen-x screen-y)
            ((dispatch 'set-coords)
             (* (- (+ 0.5 (/ (mouse-x) screen-x)) 1) 2)
             (* (- 0.5 (/ (mouse-y) screen-y)) 2)
             0)])]))

(define (destroy-mapping)
  (for/list ([triangle triangle-list])
            (destroy triangle)))
