#lang racket

;; (provide evm-assemble)

(struct cnst (val) #:transparent) ;; represents a constant
(struct labl (val) #:transparent) ;; represents a label
(struct sequ (val [size #:mutable]) #:transparent) ;; represents a sequence of opcodes

(struct dataVal (seq) #:transparent) ;; represents a dataVal token
;; (struct dataLoc (seq) #:transparent) ;; represents a dataLoc token
(struct dataSize (seq) #:transparent) ;; represents a dataSize token

;; seq is a symbol corresponding to a seq

(struct atok (op a1 a2) #:transparent) ;; represents an arithmetic token

;; op is a (add, sub, mul, div)
;; ai are (number, symbol, data, op)


(define (evm-assemble prog)
  (if (list? prog)
      (evmasm (evmasm-func->asm `((seq bytecode ,prog))))
      (evm-assemble `((push ,prog)))))


(define (evmasm prog)
  (reset)
  (parseSEQ prog))

(define (reset)
  (hash-clear! hsh)
  (hash-clear! hsh-seq))
(define hsh (make-hash))
(define hsh-seq (make-hash))

;; HELPERS

(define (parseSEQ prog)
  (if (empty? prog) empty
      (append
       (match (car prog)
         [`(seq ,var ,p)
          (let ([prg (parseSEQ p)])
            (hash-set! hsh-seq var (sequ prg (length prg)))
            `((label ,var)
              ,(dataVal var)))]
         [x `(,x)])
       (parseSEQ (cdr prog)))))

(define (evmasm-func->asm prog)
  (if (empty? prog) empty
      (append
       (match (car prog)
         [`(dataVal ,seq) `(,(dataVal (sanVAR seq)))]
         ;; [`(dataLoc ,seq) `(,(dataLoc (sanVAR seq)))]
         [`(dataSize ,seq) `(,(dataSize (sanVAR seq)))]

         [`(def ,var ,val) `((cnst ,(sanVAR var) ,val))]
         [`(label ,var) `((labl ,(sanVAR var)))]
         [`(seq ,var ,p) `((seq ,(sanVAR var) ,(evmasm-func->asm p)))]

         [`(,op ,_ ,_)
          #:when (aop? op)
          (evmasm-func->asm `(,(aexp-op (car prog))))]

         [(list op args ...)
          (append* (reverse (cons `((,op))
                                  (map evmasm-func->asm
                                       (map list args)))))]

         [(? symbol? var) `(,(sanVAR var))]
         [x `(,x)])
       (evmasm-func->asm (cdr prog)))))

;; Optimize arithmetic expressions
(define (aexp-op prog)
  (match prog
    [`(,op ,ae1 ,ae2)
     #:when (aop? op)
     (let ([a1 (aexp-op ae1)]
           [a2 (aexp-op ae2)])
       (cond
         [(or (list? a1)
              (list? a2))
          `(,(opTrans op) ,ae1 ,ae2)]
         [(and (number? a1)
               (number? a2))
          ((aopTrans op) a1 a2)]
         [else (atok op a1 a2)]))]

    ;; [`(dataLoc ,seq) (dataLoc (sanVAR seq))]
    [`(dataSize ,seq) (dataSize (sanVAR seq))]
    [(? symbol? var) (sanVAR var)]

    [_ prog]))

;; True if op is an arithmetic operator
(define (aop? op)
   (ormap (lambda (x) (equal? x op))
                        '(+ - * / %)))

(define opTransHash
  (make-immutable-hash
   '((+ . add)
     (- . sub)
     (* . mul)
     (/ . div)
     (% . mod))
   ))
(define aopTransHash
  (make-immutable-hash
   `((+ . ,+)
     (- . ,-)
     (* . ,*)
     (/ . ,quotient)
     (% . ,modulo))
   ))

(define (opTrans op)
  (hash-ref opTransHash op))
(define (aopTrans op)
  (hash-ref aopTransHash op))

;; Sanitize a variable name
(define (sanVAR var)
  (symb-append '_var: var))

;; Appends to symbol
(define (symb-append symb1 symb2)
  (string->symbol (string-append (symbol->string symb1) (symbol->string symb2))))
