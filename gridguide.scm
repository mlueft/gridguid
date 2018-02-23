; 2008 - initial release
; 2018 - extended to accept complex intervals like 8 5 3 -1 2 4 as grid distance.
 
; this function is from Stackoverflow
(define (string-split str)
 
  (define (char->string c)
    (make-string 1 c)
  )
 
  (define (string-first-char str)
    (string-ref str 0)
  )
 
  (define (string-first str)
    (char->string (string-ref str 0))
  )
 
  (define (string-rest str)
    (substring str 1 (string-length str))
  )
 
  (define (string-split-helper str chunk lst)
    (cond
      ((string=? str "") (reverse (cons chunk lst)))
      (else
        (cond
           (
             (char=? (string-first-char str) #\space)
                 (string-split-helper (string-rest str) "" (cons chunk lst))
           )(else
             (string-split-helper (string-rest str) (string-append chunk (string-first str)) lst)
           )
        )
      )
    )
  )
 
  (string-split-helper str "" (list))
)
 
 
(define (script-fu-gridguide image drawable xspace yspace xoff yoff remove)
 
    (if (= remove 1)
        (script-fu-guides-remove image drawable)
    )
   
    (let* (
            (xspaces (string-split xspace))
          )
         (createvguides image drawable xoff xspaces)
    )
 
    (let* (
            (yspaces (string-split yspace))
          )
         (createhguides image drawable yoff yspaces)
    )   
    
)
 
(define (createvguides image drawable pos steps)
    (let* (
            (cpos pos)
            (i 0)
          )
          (while (< cpos (car (gimp-image-width image)))
                (gimp-image-add-vguide image cpos)
                (set! cpos (+ cpos (string->number (list-ref steps i))))
                (set! i (+ i 1))
                (if (>= i (length steps))
                    (set! i 0)
                )
          )
    )
)
 
(define (createhguides image drawable pos steps)
    (let* (
            (cpos pos)
            (i 0)
          )
          (while (< cpos (car (gimp-image-height image)))
                (gimp-image-add-hguide image cpos)
                (set! cpos (+ cpos (string->number (list-ref steps i))))
                (set! i (+ i 1))
                (if (>= i (length steps))
                    (set! i 0)
                )
          )
    )
)
 
 
(script-fu-register "script-fu-gridguide"
            _"_Gridguide"
            _"Creates a grid of guids"
            "Michael Lueftenegger"
            "2018, Michael Lueftenegger, http://www.lueftenegger.at"
            "Feb 09, 2018"
            "*"
           
            SF-IMAGE "Image" 0
            SF-DRAWABLE "Drawable" 0
           
            SF-STRING "x space" "8 4"
            SF-STRING "y space" "8 4"
            SF-VALUE  "x offset" "0"
            SF-VALUE  "y offset" "0"
            SF-TOGGLE "first remove all guides?" TRUE
)
(script-fu-menu-register "script-fu-gridguide" "<Image>/Image/Guides")  
