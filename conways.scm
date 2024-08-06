(use-modules (ice-9 format)
             (ice-9 binary-ports)
             (ice-9 rdelim)
             (ice-9 match)
             (rnrs bytevectors))

; Direct to frame buffer
(define FB-DEV "/dev/fb0")

; Open fb as port
(define fb-port (open-file FB-DEV "r+b"))

; Scaling factor for pixels
(define SCALE 2)

; Draw pixel function
(define (draw-pixel x y colour)
  (let ((scaled-x (* x SCALE))
        (scaled-y (* y SCALE)))
    (do ((i 0 (+ i 1)))
        ((= i SCALE))
      (do ((j 0 (+ j 1)))
          ((= j SCALE))
        (let* ((offset (+ (* (+ scaled-y i) WIDTH 4) (* (+ scaled-x j) 4)))
               (bv (make-bytevector 4)))
          (bytevector-u8-set! bv 0 (logand colour #xFF))
          (bytevector-u8-set! bv 1 (logand (ash colour -8) #xFF))
          (bytevector-u8-set! bv 2 (logand (ash colour -16) #xFF))
          (bytevector-u8-set! bv 3 #xFF)
          (seek fb-port offset SEEK_SET)
          (put-bytevector fb-port bv))))))

; Draw line function
(define (draw-line x y length colour)
  (let ((scaled-x (* x SCALE))
        (scaled-y (* y SCALE))
        (scaled-length (* length SCALE)))
    (do ((i 0 (+ i 1)))
        ((= i SCALE))
      (let* ((offset (+ (* (+ scaled-y i) WIDTH 4) (* scaled-x 4)))
             (line (make-bytevector (* 4 scaled-length))))
        (do ((j 0 (+ j 1)))
            ((= j scaled-length))
          (bytevector-u8-set! line (* j 4) (logand colour #xFF))
          (bytevector-u8-set! line (+ (* j 4) 1) (logand (ash colour -8) #xFF))
          (bytevector-u8-set! line (+ (* j 4) 2) (logand (ash colour -16) #xFF))
          (bytevector-u8-set! line (+ (* j 4) 3) #xFF))
        (seek fb-port offset SEEK_SET)
        (put-bytevector fb-port line)))))

; Clear screen function
(define (clear-screen)
  (seek fb-port 0 SEEK_SET)
  (put-bytevector fb-port (make-bytevector (* WIDTH HEIGHT 4) 0)))

; Define canvas dimensions
(define CANVAS-WIDTH 100)
(define CANVAS-HEIGHT 100)

; Make canvas function
(define (make-canvas width height)
  (let ((canvas (make-vector height)))
    (do ((y 0 (+ y 1)))
        ((= y height) canvas)
      (let ((row (make-vector width)))
        (do ((x 0 (+ x 1)))
            ((= x width))
          (vector-set! row x (zero? (random 5))))
        (vector-set! canvas y row)))))

; Count neighbours function
(define (count-neighbours canvas x y width height)
  (let ((count 0))
    (do ((dy -1 (+ dy 1)))
        ((> dy 1) count)
      (do ((dx -1 (+ dx 1)))
          ((> dx 1))
        (if (and (not (and (zero? dx) (zero? dy)))
                 (>= (+ x dx) 0)
                 (< (+ x dx) width)
                 (>= (+ y dy) 0)
                 (< (+ y dy) height)
                 (vector-ref (vector-ref canvas (+ y dy)) (+ x dx)))
            (set! count (+ count 1)))))))

; Update canvas function
(define (update-canvas canvas width height)
  (let ((new-canvas (make-vector height)))
    (do ((y 0 (+ y 1)))
        ((= y height) new-canvas)
      (vector-set! new-canvas y (make-vector width))
      (do ((x 0 (+ x 1)))
          ((= x width))
        (let* ((cell (vector-ref (vector-ref canvas y) x))
               (neighbours (count-neighbours canvas x y width height)))
          (vector-set! (vector-ref new-canvas y) x
            (or (and cell (or (= neighbours 2) (= neighbours 3)))
                (and (not cell) (= neighbours 3)))))))))

; Draw canvas function
(define (draw-canvas canvas width height)
  (do ((y 0 (+ y 1)))
      ((= y height))
    (do ((x 0 (+ x 1)))
        ((= x width))
      (let ((start-x x)
            (current-value (vector-ref (vector-ref canvas y) x)))
        (while (and (< x width)
                    (equal? (vector-ref (vector-ref canvas y) x) current-value))
          (set! x (+ x 1)))
        (draw-line start-x y (- x start-x) (if current-value #xFFFFFF #x000000))
        (set! x (- x 1))))))

; Main function
(define (main canvas)
  (clear-screen)
  (draw-canvas canvas CANVAS-WIDTH CANVAS-HEIGHT)
  (usleep 16667)
  (main (update-canvas canvas CANVAS-WIDTH CANVAS-HEIGHT)))

(main (make-canvas CANVAS-WIDTH CANVAS-HEIGHT))
