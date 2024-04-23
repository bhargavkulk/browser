#lang racket
;; test: https://browser.engineering/http.html
;; test: https://browser.engineering/examples/example3-sizes.html
(require racket/gui net/http-easy threading)

;;- Browser State --------------------------------------------------------------

(define *width* (make-parameter 1000))
(define *height* (make-parameter 1000))
(define *scroll* (make-parameter 0))
(define *url* (make-parameter #f))
(define *buffer* (make-parameter '()))

;;- Constant -------------------------------------------------------------------

(define SCROLL-STEP 100)
(define HSTEP 20)
(define VSTEP 20)

;;- Browser Window -------------------------------------------------------------

(define browser-frame%
  (class frame%
    (super-new)
    (define/override (on-size w h)
      (super on-size w h)
      (*width* w)
      (*height* h))))

(define frame (new browser-frame% [label "Browser"]
                   [width (*width*)]
                   [height (*height*)]))

(define browser (new vertical-panel% [parent frame]))
(define url-bar (new horizontal-panel% [parent browser]))
(define url-entry (new text-field% [parent url-bar] [label "URL"]))
(define url-button
  (new button% [parent url-bar] [label "Go"]
       [callback (lambda (_b _e)
                   (enter-url dc (send url-entry get-value)))]))
(define scroll-up (new button% [parent url-bar] [label "Up"]
                       [callback (lambda (_b _e)
                                   (*scroll*
                                    (if (zero? (*scroll*))
                                        (*scroll*)
                                        (- (*scroll*) SCROLL-STEP)))
                                   (draw-page dc))]))

(define scroll-down (new button% [parent url-bar] [label "Down"]
                       [callback (lambda (_b _e)
                                   (*scroll* (+ (*scroll*) SCROLL-STEP))
                                   (draw-page dc))]))

(define web-page (new canvas% [parent browser]
                      [min-height (round (/ (* 11 (*height*)) 12))]
                      [paint-callback
                       (lambda (c d)
                         (when (*url*)
                           (enter-url d (*url*))))]))

(define dc (send web-page get-dc))

;;- Layout Functions -----------------------------------------------------------

;; LineToken{x, token, font}
;; Token{x, y, word, font}
;; Line = Listof[LineToken]
;; DrawBuffer = Listof[Token]

;; flush : Line * DrawBuffer * Number * DrawingCtxt -> DrawBuffer * Number
;; flushes a full line buffer, calculates the the y coordinate of each token
(define (flush line buf y dc)
  (cond
    [(empty? line)
     (list y buf)]
    [else 
     (define descents
       (for/list ([thing (in-list line)])
         (match-define (list x w f) thing)
         (get-font-descent dc f w)))
     (define ascents
       (for/list ([thing (in-list line)])
         (match-define (list x w f) thing)
         (get-font-ascent dc f w)))
     (define max-descent (apply max descents))
     (define max-ascent (apply max ascents))
     (define baseline (+ y (* 1.25 max-ascent)))
     (define (loop line buf ascents)
       (match line
         ['() (list (+ baseline (* max-descent 1.25)) buf)]
         [(list (list x w f) tail ...)
          (define new-y (- baseline (first ascents)))
          (loop tail (cons (list x new-y w f) buf) (rest ascents))]))
     (loop (reverse line) buf (reverse ascents))]))

;; text->dlist: DrawingCtxt * Line * DrawBuffer * String * Number * Number * [Font ...]
;;           -> Line * DrawBuffer * Number * Number
(define (text->dlist dc ln buf text x y ffam fsize fweight fstyle)
  (define font (make-font #:size fsize
                          #:family ffam
                          #:style fstyle
                          #:weight fweight))
  
  (let loop ([words (string-split text)]
             [cursor-x x]
             [cursor-y y]
             [line ln]
             [buffer buf])
    (if (empty? words)
        (list line buffer cursor-x cursor-y)
        (let* ([word (first words)]
               [new-line (cons (list cursor-x word font) line)]
               [w (get-font-width dc font word)]
               [new-x (+ cursor-x w (get-font-width dc font " "))]
               [new-y (+ cursor-y (* (get-font-size dc) 1.25))])
          (cond
            [(> (+ cursor-x w) (- (*width*) (* 5 HSTEP)))
             (match-define (list ny nbuffer) (flush new-line buffer cursor-y dc))
             (loop (rest words) HSTEP ny '() nbuffer)]
            [else (loop (rest words) new-x cursor-y new-line buffer)])))))

;; text->dlist: Number * Number * Line * DrawBuffer * [Font ...]
;;           -> Void
(define (layout-new dc tokens)
  (define (loop tokens x y line buf ffam fsize fweight fstyle ignore?)
    (cond
      [(empty? tokens)
       (match-define (list ny nbuffer) (flush line buf y dc))
       (*buffer* nbuffer)]
      [else
       (define token (first tokens))
       (define ts (rest tokens))
       (define font (list fsize fweight fstyle))
       (cond
         [(and (equal? (first token) 'tag)
               (string-prefix? (second token) "style"))
          (loop ts x y line buf ffam fsize fweight fstyle #t)]
         [(and (equal? (first token) 'tag)
               (string-prefix? (second token) "/style"))
          (loop ts x y line buf ffam fsize fweight fstyle #f)]
         [ignore?
          (loop ts x y line buf ffam fsize fweight fstyle ignore?)]
         [(equal? (first token) 'text)
          (match-define (list n-line nbuffer nx ny)
            (text->dlist dc line buf (second token) x y ffam fsize fweight fstyle))
          (loop ts nx ny n-line nbuffer ffam fsize fweight fstyle ignore?)]
         [(equal? (second token) "i")
          (loop ts x y line buf ffam fsize fweight 'italic ignore?)]
         [(equal? (second token) "/i")
          (loop ts x y line buf ffam fsize fweight 'normal ignore?)]
         [(equal? (second token) "b")
          (loop ts x y line buf ffam fsize 'bold fstyle ignore?)]
         [(equal? (second token) "/b")
          (loop ts x y line buf ffam fsize 'normal fstyle ignore?)]
         [(equal? (second token) "big")
          (loop ts x y line buf ffam (+ fsize 4) fweight fstyle ignore?)]
         [(equal? (second token) "/big")
          (loop ts x y line buf ffam (- fsize 4) fweight fstyle ignore?)]
         [(equal? (second token) "small")
          (loop ts x y line buf ffam (- fsize 2) fweight fstyle ignore?)]
         [(equal? (second token) "/small")
          (loop ts x y line buf ffam (- fsize 2) fweight fstyle ignore?)]
         [(equal? (second token) "/p")
          (match-define (list ny nbuffer) (flush line buf y dc))
          (loop ts HSTEP (+ ny (* 2 VSTEP)) '() nbuffer ffam fsize fweight fstyle ignore?)]
         [(string-prefix? (second token) "pre")
          (loop ts x y line buf 'modern fsize fweight fstyle ignore?)]
         [(equal? (second token) "/pre")
          (match-define (list ny nbuffer) (flush line buf y dc))
          (loop ts HSTEP (+ ny (* 2 VSTEP)) '() nbuffer 'default fsize fweight fstyle ignore?)]
         [(equal? (second token) "br")
          (match-define (list ny nbuffer) (flush line buf y dc))
          (loop ts HSTEP ny '() nbuffer ffam fsize fweight fstyle ignore?)]
         [(string-prefix? (second token) "h1")
          (loop ts HSTEP y '() buf ffam (+ fsize 6) fweight fstyle ignore?)]
         [(equal? (second token) "/h1")
          (match-define (list ny nbuffer) (flush line buf y dc))
          (loop ts HSTEP ny '() nbuffer ffam (- fsize 6) fweight fstyle ignore?)]
         [(string-prefix? (second token) "h2")
          (loop ts HSTEP y '() buf ffam (+ fsize 4) fweight fstyle ignore?)]
         [(equal? (second token) "/h2")
          (match-define (list ny nbuffer) (flush line buf y dc))
          (loop ts HSTEP ny '() nbuffer ffam (- fsize 4) fweight fstyle ignore?)]
         [(equal? (second token) "/li")
          (match-define (list ny nbuffer) (flush line buf y dc))
          (loop ts HSTEP ny '() nbuffer ffam fsize fweight fstyle ignore?)]
         [(equal? (second token) "/ul")
          (match-define (list ny nbuffer) (flush line buf y dc))
          (loop ts HSTEP (+ ny (* 2 VSTEP)) '() nbuffer ffam fsize fweight fstyle ignore?)]
         [else (loop ts x y line buf ffam fsize fweight fstyle ignore?)])]))
  (loop tokens HSTEP VSTEP '() '() 'default 16 'normal 'normal #f))

;;- Font Functions -------------------------------------------------------------

(define (get-font-size dc)
  (define font (send dc get-font))
  (send font get-size #t))

(define (get-font-width dc font word)
  (send dc set-font font)
  (define-values (width height ascent descent)
    (send dc get-text-extent word))
  width)

(define (get-font-descent dc font word)
  (send dc set-font font)
  (define-values (width height descent ascent)
    (send dc get-text-extent word))
  (identity descent))

(define (get-font-ascent dc font word)
  (send dc set-font font)
  (define-values (width height descent ascent)
    (send dc get-text-extent word))
  (identity (- height descent)))

;;- Drawing Functions ----------------------------------------------------------

(define (draw-page dc)
  (send dc clear)
  (for ([coords (in-list (*buffer*))])
    (match-define (list x y c f) coords)
    (send dc set-font f)
    (unless (or (> y (+ (*scroll*) (*height*)))
                (< (+ y VSTEP) (*scroll*)))
      (send dc draw-text c x (- y (*scroll*))))))

;;- HTML Parser ----------------------------------------------------------------

(define (lex-tags body)
  (define (lex-helper chars buffer in-tag in-pre result)
    (cond
      [(empty? chars)
       (if (and (not in-tag) (not (string=? buffer "")))
           (append result (list (list 'text buffer)))
           result)]
      [(char=? (first chars) #\<)
       (cond
         [(string=? buffer "")
          (lex-helper (cdr chars) "" #t in-pre result)]
         [in-pre
          (lex-helper (cdr chars) "" #t in-pre (append result (escape-ln buffer)))]
         [else
          (lex-helper (cdr chars) "" #t in-pre (append result (list (list 'text (escape buffer)))))])]
      [(char=? (first chars) #\>)
       (cond
         [(string=? buffer "") (lex-helper (rest chars) "" #f in-pre result)]
         [(string-prefix? buffer "pre")
          (lex-helper (rest chars) "" #f #t (append result (list (list 'tag buffer))))]
         [(string-prefix? buffer "/pre")
          (lex-helper (rest chars) "" #f #f (append result (list (list 'tag buffer))))]
         [else (lex-helper (rest chars) "" #f in-pre (append result (list (list 'tag buffer))))])]
      [else
       (lex-helper (rest chars) (string-append buffer (string (first chars))) in-tag in-pre result)]))
  
  (lex-helper (string->list body) "" #f #f '()))

(define (escape-ln str)
  (define splits (string-split str "\n"))
  (define (loop splits)
    (match splits
      ['() '()]
      [(list split) (list (list 'text (escape split)))]
      [(list split splits ...) (cons (list 'text (escape split))
                                     (cons (list 'tag "br")
                                           (loop splits)))]))
  (loop splits))
  

(define (escape text)
  (~> text
      (string-replace _ "&lt;"   "<")
      (string-replace _ "&gt;"   ">")
      (string-replace _ "&quot;" "\"")
      (string-replace _ "&#39;"  "'")
      (string-replace _ "&nbsp;" " ")
      (string-replace _ "&#91;"  "[")
      (string-replace _ "&#93;"  "]")))

;;- Main Loop ------------------------------------------------------------------

(define (enter-url dc url)
  (*url* url)
  (define res (get url))
  (*scroll* 0)
  (*buffer* '())

  (define page
    (~> res
        response-body
        bytes->string/utf-8
        lex-tags))
  (layout-new dc page)
  
  (draw-page dc))

(send frame show #t)
