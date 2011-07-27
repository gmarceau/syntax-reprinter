(module test-syntax-reprinter mzscheme
  (require "syntax-reprinter.ss"
           (planet "test.ss" ("schematics" "schemeunit.plt" 2 4))
           (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2 4)))
  
  (define (reprint stx)
    (define outp (open-output-string))
    (syntax-reprint stx outp)
    (get-output-string outp))
  
  (define (reprint-many stx)
    (define outp (open-output-string))
    (syntax-reprint-many stx outp)
    (get-output-string outp))
  
  (define reprinter-tests
    (test-suite
     "Reprinter tests"
     (test-equal? "simple test"
                  (reprint (syntax (hello world)))
                  "                                   (hello world)")
     
     (test-equal? "dotted pair"
                  (reprint (syntax (hello . nurse)))
                  "                                   (hello . nurse)")
     
     (test-equal? "boxy empty"
                  (reprint (syntax []))
                  "                                   []")
     
     (test-equal? "quote using quote"
                  (reprint (syntax (quote hello)))
                  
                  "                                   (quote hello)")
     
     (test-equal? "quote in quote in quote"
                  (reprint (syntax '(quote '(quote hello))))
                  "                                   '(quote '(quote hello))")
     
     (test-equal? "list containing lists"
                  (reprint (syntax (hello (world) testing)))
                  "                                   (hello (world) testing)")
     
     (test-equal? "vector"
                  (reprint (syntax #(1 2
                                       3
                                       4)))
                  "                                   #(1 2\n                                       3\n                                       4)")
     
     (test-equal? "quoted list"
                  (reprint (syntax '(hello)))
                  "                                   '(hello)")
     
     
     (test-equal? "syntax spanning lines"
                  (reprint (syntax (hiya
                                    world)))
                  "                                   (hiya\n                                    world)")
     
     
     (test-equal? "syntax spanning multiple lines"
                  (reprint (syntax (hiya
                                    
                                    
                                    world)))
                  "                                   (hiya\n\n\n                                    world)")
     
     
     (let ()
       (define str "(1 2 3    4)      (1 2 3 4)")
       (define port (begin (port-count-lines-enabled #t) (open-input-string str)))
       (test-equal? "syntax-reprint-many"
                    (reprint-many (list (read-syntax 'str port) (read-syntax 'str port)))
                    str))))
  
  (test/text-ui reprinter-tests))