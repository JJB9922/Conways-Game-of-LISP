#|
Playing with LISP (the SCHEME variant)
Writing a program that outputs a gradient in a TGA file
|#

;Utilities

(define (write-byte byte port)
        (write-char (integer->char (logand byte #xFF)) port)) ; write octet to the output port

(define (write-word word port)
        (write-byte (logand word #xFF) port) ; AND mask
        (write-byte (logand (ash word -8) #xFF) port)) ; shift & mask

#|
will write a TGA header for this example
please see paulbourke.net/dataformats/tga/
|#
(define (write-image-header width height port)
        (write-byte 0 port) ; ID Length
        (write-byte 0 port) ; Colour Map Type
        (write-byte 2 port) ; Data Type
        (write-word 0 port) ; Colour Map Origin
        (write-word 0 port) ; Colour Map Length
        (write-byte 0 port) ; Colour Map Depth
        (write-word 0 port) ; X origin
        (write-word 0 port) ; Y origin
        (write-word width port) ; Width
        (write-word height port) ; Height
        (write-byte 24 port) ; Bits per Pixel
        (write-byte 0 port)) ; Image descriptor

; Draw a pixel (TGA does BGR format)
(define (write-pixel r g b port)
        (write-byte b port)
        (write-byte g port)
        (write-byte r port))

(define (draw-gradient file width height)
  (with-output-to-file file ; gives us a "port" to draw to 
        (lambda () ; create anon function
          (let ((port (current-output-port)))
          ; the rest of this just recursively writes pixels to the file :)
          (write-image-header width height port)
          (let loop ((y 0))
            (when (< y height)
              (let loop ((x 0))
                (when (< x width)
                  (let ((r (inexact->exact(round (* 255 (/ x (- width 1))))))
                        (g (inexact->exact(round (* 255 (/ y (- height 1))))))
                        (b 128))
                  (write-pixel r g b port))
                (loop (+ x 1))))
              (loop (+ y 1))))))
        #:binary #t))

(draw-gradient "output.tga" 500 500)
