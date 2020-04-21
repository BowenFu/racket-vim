#lang typed/racket

(module+ test (require typed/rackunit))

(provide (all-defined-out))

;;;
;;; REPRENSENTATION
;;;

;(struct line (string length) #:transparent #:mutable)

; (struct lines (lines) #:transparent #:mutable)
; A lines being edited is represented as a list of lines.

(define-type Lines (Listof String))

(struct Buffer ([lines : Lines] [path : (Option String)] [cur : Point] [modified? : Boolean]) 
  #:transparent #:mutable)

; A Buffer is the basic unit of lines being edited.
; It contains a lines being edited.
; The Buffer has a name, so the user can refer to the Buffer.
; The Buffer might have an associated file:
;   path = #f     <=>  Buffer is not associated with a file
;   path = path   <=>  reads and writes to the file given by the path
; A Point is a position between two characters. 
; Insertions and deletion will happen at the Points (usually only one).
; If modified? is true, then the Buffer has been modied since the last
; read or save of the file.

(struct Point ([row : Natural] [col : Natural] [max-col : (U Natural '+inf.0)]) #:transparent)
; A Point rembers a position in the lines.

(module+ test
  (define illead-lines
    (list "Sing, O goddess, the anger of Achilles son of Peleus, that brought"
          "countless ills upon the Achaeans. Many a brave soul did it send hurrying"
          "down to Hades, and many a hero did it yield a prey to dogs and vultures,"
          "for so were the counsels of Jove fulfilled from the day on which the"
          "son of Atreus, king of men, and great Achilles, first fell out with"
          "one another."))
  
  ; recreate the same lines file from scratch
#;  (define (create-new-test-file path)
    (with-output-to-file path
      (λ() (for ([line illead-lines])
             (displayln line)))
      #:exists 'replace)))

;;;
;;; lines
;;;

; path->lines : path -> lines
;   create a lines with contents from the file given by path
(: path->lines (-> String Lines))
(define (path->lines path)
  (with-input-from-file path 
    (λ () (for/list : Lines ([l (in-lines)]) l))))

#;(module+ test
  (void (create-new-test-file "illead.txt"))
  (check-equal? (path->lines "illead.txt") illead-lines))

;;;
;;; Point
;;;

; Point-row-col : Point -> integer integer
;   return row and column number for the Point m in the Buffer b
(: Point-row-col (Point -> (Values Natural Natural)))
(define (Point-row-col m)
  (values (Point-row m) (Point-col m)))

;;;
;;; BUFFER
;;;

; new-Buffer : -> Buffer
;   create fresh Buffer without an associated file
(: new-Buffer (->* () (Lines (Option String)) Buffer))
(define (new-Buffer [lines '()] [path #f])
  (define p (Point 0 0 0))
  (define modified? #f)
  (Buffer lines path p modified?))

; save-Buffer : Buffer -> void
;   save contents of Buffer to associated file
;   do nothing if no file is associated
(: save-Buffer! (-> Buffer Void))
(define (save-Buffer! b)
  (define file (Buffer-path b))
  (when file
    (with-output-to-file file
      (λ () (for ([line (Buffer-lines b)])
              (displayln line)))
      #:exists 'replace)
    (set-Buffer-modified?! b #f)))

(module+ test
  (provide illead-Buffer)
  (define illead-Buffer (new-Buffer illead-lines "illead.txt"))
  (save-Buffer! illead-Buffer)
  (check-equal? (path->lines "illead.txt") illead-lines))

; read-Buffer : Buffer -> void
;   replace lines of Buffer with file contents
#;(define (read-Buffer! b)
  (define path (Buffer-path b))
  (unless path (error 'read-Buffer "no assoiated file: ~a" b))
  (define lines (path->lines path))
  (set-Buffer-lines! b lines)
  (set-Buffer-modified?! b #f))

#;(module+ test
  (void (create-new-test-file "illead.txt"))
  (define b (new-Buffer  '() "illead.txt"))
  (read-Buffer! b)
  (check-equal? b illead-Buffer))

; append-to-Buffer-from-file : Buffer path -> void
;   append contents of file given by the path p to the lines of the Buffer b
#;(define (append-to-Buffer-from-file b p)
  (define lines-to-append (path->lines p))
  (set-Buffer-lines! b (append (Buffer-lines b) lines-to-append))
  (set-Buffer-modified?! b #t))

#;(module+ test
  (void (create-new-test-file "illead.txt"))
  (define append-Buffer (new-Buffer '()))
  (append-to-Buffer-from-file append-Buffer "illead.txt")
  (append-to-Buffer-from-file append-Buffer "illead.txt")
  (save-Buffer! b) ; make sure the Buffer is unmodified before comparison
  (check-equal? (Buffer-lines append-Buffer) (append illead-lines illead-lines)))

(: Buffer-display (-> Buffer Void))
(define (Buffer-display b)
  (define (line-display {l : String})
    (displayln l))
  (define (lines-display {t : Lines})
    (for ([l t])
      (line-display l)))
  (lines-display (Buffer-lines b)))

#;(module+ test
  (Buffer-display illead-Buffer))

(: Buffer-substring-at (-> Buffer Natural Natural Natural String))
(define (Buffer-substring-at b row col0 col1)
  (define l (list-ref (Buffer-lines b) row))
  (if (>= (string-length l) col1)
      (substring l col0 col1)
      " "))


(: line-ref-char (-> String Natural Char))
(define (line-ref-char l col)
  (if (> (string-length l) col)
      (string-ref l col)
      #\space))

(module+ test
  (check-equal? (line-ref-char "abc" 0) #\a)
  (check-equal? (line-ref-char "abc" 1) #\b)
  (check-equal? (line-ref-char "abc" 2) #\c)
  (check-equal? (line-ref-char "abc" 3) #\space))

(: Point<? (-> Point Point Boolean))
(define (Point<? p1 p2)
  (define-values (row1 col1) (Point-row-col p1))
  (define-values (row2 col2) (Point-row-col p2))
  (cond
    [(= row1 row2) (< col1 col2)]
    [(< row1 row2) #t]
    [else #f]))

(module+ test
  (check-equal? (Point<? (Point 1 0 2) (Point 1 0 3)) #f)
  (check-equal? (Point<? (Point 1 1 2) (Point 1 0 3)) #f)
  (check-equal? (Point<? (Point 1 0 2) (Point 1 1 2)) #t))

;;; not compare max-col
(: min-Point (-> Point Point Point))
(define (min-Point p1 p2)
  (if (Point<? p1 p2) p1 p2))

(module+ test
  (check-equal? (min-Point (Point 1 0 2) (Point 1 0 3)) (Point 1 0 3))
  (check-equal? (min-Point (Point 1 1 2) (Point 1 0 3)) (Point 1 0 3))
  (check-equal? (min-Point (Point 1 0 2) (Point 1 1 2)) (Point 1 0 2)))

(: empty-lines? (-> Lines Boolean))
(define (empty-lines? lines)
  (or (empty? lines) (equal? lines '(""))))
