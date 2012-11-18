(define all-coords
  (list (list '#(0 0 0) '#(0 1 0) '#(1 0 0))
        (list '#(1 0 0) '#(1 1 0) '#(2 0 0))))

(define-syntax-rule (first l) (list-ref l 0))
(define-syntax-rule (empty? l) (eq? l '()))
(define-syntax-rule (rest l) (cdr l))

(define (make-poly n)
  (with-state
   (hint-unlit)
   (hint-wire)
   (backfacecull 0)
   (wire-colour (vector 1 0 0))
   (colour (vector 1 1 1))
   (build-polygons n 'polygon)))

(define (get-coord coords)
  (lambda ()
    (let ([current-coord (first coords)])
      (cond [(not (empty? (rest coords)))
             (set! coords (rest coords))])
      current-coord)))

(define (make-poly-list all-coords vertex-num)
  (define (loop coords-list poly-list)
    (cond [(empty? coords-list) poly-list]
          [else (let ([poly (make-poly vertex-num)])
                  (poly-coord-map poly (first coords-list))
                  (loop (rest coords-list) (cons poly poly-list)))]))
  (loop all-coords '()))

(define (poly-coord-map poly coords)
  (with-primitive poly
                  (pdata-add "poly-coords" "v")
                  (let ([next-coord (get-coord coords)])
                    (pdata-map!
                     (lambda (coords)
                       (next-coord))
                     "poly-coords"))
                  (pdata-map!
                   (lambda (p coords) coords)
                   "p" "poly-coords")))

(define triangle-list (make-poly-list all-coords 3))

(define (mapping)
  (for/list ([triangle triangle-list])
            (with-primitive triangle
                            (rotate (vmul (vector 1 0 0) 1)))))

(define (destroy-mapping)
  (for/list ([triangle triangle-list])
            (destroy triangle)))
