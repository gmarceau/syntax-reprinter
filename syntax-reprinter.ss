(module syntax-reprinter racket
  (require (lib "list.ss")
           (lib "boundmap.ss" "syntax"))
  
  (provide syntax-reprint syntax-reprint-many)
  
  
  ;; We need to maintain our current position.
  (define-struct pos (line column))
  
  ;; pos-newline: pos -> pos
  ;; moves a-pos down one row.
  (define (pos-newline a-pos)
    (make-pos (add1 (pos-line a-pos)) 0))
  
  ;; pos-forward-column: pos [number=1] -> pos
  ;; pushes a-pos forward by n columns.
  (define pos-forward-column
    (case-lambda
      [(a-pos)
       (pos-forward-column a-pos 1)]
      [(a-pos n)
       (make-pos (pos-line a-pos) (+ n (pos-column a-pos)))]))
  
  
  ;; pos-stx-printed: pos syntax -> pos
  (define (pos-stx-printed a-pos stx)
    ;; TODO: handle strings with internal newlines
    (make-pos (pos-line a-pos)
              (+ (syntax-span stx) (pos-column a-pos))))
  
  
  ;; In order to handle the syntactic abbreviations in a nice way,
  ;; we keep a mapping them here:
  (define syntax-abbreviations (make-module-identifier-mapping))
  (define eq-abbreviations (make-hash))
  (for-each (lambda (abbv+string)
              (module-identifier-mapping-put! syntax-abbreviations
                                              (first abbv+string)
                                              (second abbv+string))
              (hash-set! eq-abbreviations (syntax-e (first abbv+string))
                         (second abbv+string)))
            (list (list #'quote "'")
                  (list #'quasiquote "`")
                  (list #'unquote ",")
                  (list #'unquote-splicing ",@")
                  (list #'syntax "#'")
                  (list #'quasisyntax "#`")
                  (list #'unsyntax "#,")
                  (list #'unsyntax-splicing "#,@")))
  
  ;; abbreviated-quote?: syntax -> boolean
  (define (abbreviated-quote? stx)
    (let/ec return
      (let ([s
             (and (identifier? stx)
                  (or (module-identifier-mapping-get syntax-abbreviations stx (lambda () #f))
                      (hash-ref eq-abbreviations (syntax-e stx) #f)))])
        (and s (= (syntax-span stx) (string-length s))))))
  
  
  ;; abbreviated-quote-stx->string: syntax -> string
  (define (abbreviated-quote-stx->string stx)
    (or (module-identifier-mapping-get syntax-abbreviations stx (lambda () #f))
        (hash-ref eq-abbreviations (syntax-e stx))))
  
  ;; reprint: syntax pos -> pos
  ;; prints out datum, returns last position.
  (define (reprint stx last-pos)
    (cond
      [(< (pos-line last-pos) (syntax-line stx))
       (newline)
       (reprint stx (pos-newline last-pos))]
      [(< (pos-column last-pos) (syntax-column stx))
       (display " ")
       (reprint stx (pos-forward-column last-pos))]
      [else
       (main-case-analysis stx last-pos)]))
  
  
  ;; main-case-analysis: syntax number number -> number
  ;; Does the main case analysis on the datum.
  ;; Returns the last syntax object printed.
  (define (main-case-analysis stx last-pos)
    (syntax-case stx ()
      [(abbreviated-quote datum)
       (abbreviated-quote? (syntax abbreviated-quote))
       (handle-abbreviated-quote stx last-pos)]
      [(_0 . _1)
       (handle-pair/empty stx last-pos)]
      [()
       (handle-pair/empty stx last-pos)]
      [#(_ ...)
       (handle-vector stx last-pos)]
      [else
       (handle-datum stx last-pos)]))
  
  
  ;; handle-abbreviated-quote: syntax pos -> pos
  (define (handle-abbreviated-quote stx last-pos)
    (syntax-case stx ()
      [(abbrv-quote datum)
       (abbreviated-quote? (syntax abbrv-quote))
       (let ([quote-string (abbreviated-quote-stx->string (syntax abbrv-quote))])
         (display quote-string)
         (reprint (syntax datum)
                  (pos-forward-column last-pos (string-length quote-string))))]))
  
  
  ;; handle-pair/empty: syntax pos -> pos
  (define (handle-pair/empty stx last-pos)
    (display (open stx))
    (let ([new-last-pos
           (reprint-sequence-internals (syntax-e stx) (pos-forward-column last-pos))])
      (display (close stx))
      ;; unfortunately, syntax objects do not capture enough
      ;; for us to know if there's some newline between the
      ;; open and close parens. We just assume that we've just gone forward.
      (pos-forward-column new-last-pos)))
  
  
  ;; handle-vector: syntax pos -> pos
  (define (handle-vector stx last-pos)
    (display "#(")
    (let* ([vec (syntax-e stx)]
           [len (vector-length vec)])
      (let loop ([i 0]
                 [last-pos (pos-forward-column last-pos 2)])
        (cond [(< i len)
               (loop (add1 i)
                     (reprint (vector-ref vec i) last-pos))]
              [else
               (display ")")
               (pos-forward-column last-pos)]))))
  
  
  ;; handle-datum: syntax pos -> pos
  (define (handle-datum stx last-pos)
    (write (syntax->datum stx))
    (pos-stx-printed last-pos stx))
  
  
  ;; reprint-sequence-internals: syntax (union syntax-pair empty syntax-object) -> syntax
  ;; Handles the printing of the internal elements.
  ;; Returns the syntax of the last printed element.
  (define (reprint-sequence-internals stx-pair last-pos)
    (let loop ([stx-pair stx-pair]
               [last-pos last-pos])
      (cond
        [(empty? stx-pair) last-pos]
        
        [(pair? stx-pair)
         (let ([new-last-pos
                (reprint (first stx-pair) last-pos)])
           (loop (rest stx-pair) new-last-pos))]
        
        [else
         (display " . ")
         (reprint stx-pair (pos-forward-column last-pos 3))])))
  
  
  
  ;; open: syntax -> character
  ;; Depending on the shape of the stx, returns the appropriate opening paren.
  (define (open stx)
    (case (syntax-property stx 'paren-shape)
      [(#\[) "["]
      [(#\{) "{"]
      [else "("]))
  
  
  ;; close: syntax -> character
  ;; Depending on the shape of the stx, returns the appropriate closing paren.
  (define (close stx)
    (case (syntax-property stx 'paren-shape)
      [(#\[) "]"]
      [(#\{) "}"]
      [else ")"]))
  
  
  ;; syntax-reprint: stx output-port -> void
  (define (syntax-reprint stx [outp (current-output-port)])
    (parameterize ([current-output-port outp])
      (reprint stx (make-pos (syntax-line stx) 0)))
    (void))
  
  (define (syntax-reprint-many stxs [outp (current-output-port)])
    (parameterize ([current-output-port outp])
      (reprint-sequence-internals stxs (make-pos (syntax-line (first stxs)) 0)))))

