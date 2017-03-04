#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(define ab-lexer
  (lexer
   [#\a (display "You matched a.\n")]
   [#\b (display "You matched b.\n")]))

(define ab-test-in (open-input-string "ababc"))

(define (string->char s)
  (car (string->list s)))

(define lex
  (lexer
   [#\space (lex input-port)]
   [#\newline (lex input-port)]
   [any-char (list 'STRING lexeme)]))

(define in (open-input-string "foo

 bar baz"))

(define basic-printing-lexer
  (lexer
   [(repetition 1 +inf.0 (char-range #\a #\z))
    (begin
      (display "found an id: ")
      (display lexeme)
      (newline))]
   [(union #\space #\newline) (void)]))

(define (run-basic-printing-lexer port)
  (when (not (eq? 'eof (basic-printing-lexer port)))
    (run-basic-printing-lexer port)))

(define basic-lexer
  (lexer
   [(repetition 1 +inf.0 (char-range #\a #\z))
    (cons `(ID, lexeme) (basic-lexer input-port))]
   [(union #\space #\newline)
    (basic-lexer input-port)]
   [(eof) '()]))


(define calc-lexer
  (lexer
   [(:+ (:or (char-range #\a #\z) (char-range #\A #\Z)))
    (cons `(ID, (string->symbol lexeme))
          (calc-lexer input-port))]
   [#\(
    (cons '(LPAR)
          (calc-lexer input-port))]
   [#\)
    (cons '(RPAR)
          (calc-lexer input-port))]
   [(:: (:? #\-) (:+ (char-range #\0 #\9)))
    (cons `(INT, (string->number lexeme))
          (calc-lexer input-port))]
   [(:or #\* #\+)
    (cons `(OP, (string->symbol lexeme))
          (calc-lexer input-port))]
   [whitespace (calc-lexer input-port)]
   [(eof) '()]))
                                 

;; c style comment lexer

(define calc+-lexer
  (lexer
   [(:+ (:or (char-range #\a #\z) (char-range #\A #\Z)))
    (cons `(ID, (string->symbol lexeme))
          (calc+-lexer input-port))]
   [#\(
    (cons '(LPAR)
          (calc+-lexer input-port))]
   [#\)
    (cons '(RPAR)
          (calc+-lexer input-port))]
   [(:: (:? #\-) (:+ (char-range #\0 #\9)))
    (cons `(INT, (string->number lexeme))
          (calc+-lexer input-port))]
   [(:or #\* #\+)
    (cons `(OP, (string->symbol lexeme))
          (calc+-lexer input-port))]
   [whitespace (calc+-lexer input-port)]
   [(eof) '()]
   [(:: #\/ #\*) (comment-lexer input-port)]))


(define comment-lexer
  (lexer
   [(:: #\* #\/) (calc+-lexer input-port)]
   [any-char (comment-lexer input-port)]))
        

(calc+-lexer (open-input-string "-3 * /* this is comment */(foo

+ 12)"))
















    