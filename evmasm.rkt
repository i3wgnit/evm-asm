#lang racket

(provide evm-assemble)

(define (evm-assemble prog)
  (reset)
  (if (list? prog)
      (evmasm (evmasm-func->asm `((seq bytecode ,prog))))
      (evm-assemble `((push ,prog)))))


;; Structures

(struct cnst (val) #:transparent) ;; represents a constant
(struct labl (val) #:transparent) ;; represents a label
;; (struct dest (val) #:transparent) ;; represents a jump destination
(struct sequ (val size) #:mutable #:transparent) ;; represents a sequence of opcodes

(struct dataVal (sequ) #:transparent) ;; represents a dataVal token
;; (struct dataLoc (sequ) #:transparent) ;; represents a dataLoc token
(struct dataSize (sequ) #:transparent) ;; represents a dataSize token

;; sequ is a symbol corresponding to a sequence

(struct atok (op a1 a2) #:transparent) ;; represents an arithmetic token

;; op is a (add, sub, mul, div)
;; ai are (number, symbol, data, op)


;; Main Logic

(define (evmasm prog)
  (parsePROG prog)
  (parseLABL (hash-ref hsh-sequ '__bytecode)))


;; HELPERS

(define (reset)
  (hash-clear! hsh-labl)
  (hash-clear! hsh-cnst)
  (hash-clear! hsh-sequ))
(define hsh-labl (make-hash))
(define hsh-cnst (make-hash))
(define hsh-sequ (make-hash))

(define (parsePROG prog)
  (append* (map pPROG-h prog)))
(define (pPROG-h p)
  (match p
    [`(def ,var ,val)
     (if (hash-has-key? hsh-cnst var)
         (error 'parsePROG "~a defined more than once" var)
         (hash-set! hsh-cnst var (append* (pPROG-h val))))
     `()]
    [`(seq ,var ,prog)
     (let ([prg (parsePROG prog)])
       (hash-set! hsh-sequ var (sequ prg (length prg)))
       `((label ,var) ,(dataVal var)))]
    [x `(,x)]))

;;       [(atok op ae1 ae2)
;;        (let ([a1 (append* (pPROG-h ae1))]
;;              [a2 (append* (pPROG-h ae2))])
;;          (if (and (number? a1)
;;                   (number? a2))
;;              `(,((aopTrans op) a1 a2))
;;              (atok op a1 a2)))]
;;       [(? symbol? x)
;;        #:when (hash-has-key? hsh-cnst x)
;;        `(,(hash-ref hsh-cnst x))]

(define (parseSEQU sequ)
  (let* ([nProg (pSEQU-h (sequ-val sequ) (box 0) (box 0))]
         [len (length nProg)])
    (set-sequ-val! nProg)
    (set-sequ-size! len)))
(define (pSEQU-h prog rep acc)
  (match (car prog)
    [(? number? x)
     (let ([repe (unbox rep)]
           [accu (unbox acc)])
       (if (= x repe)
           (set-box! acc (add1 accu))
           (begin
             (set-box! rep x)
             (set-box! acc 1)
             (append (repet repe accu 0)
                     (pSEQU-h (cdr prog) rep acc)))))]
    [(? symbol? var)
     #:when (hash-has-key? hsh-cnst var)
     (pSEQU-h (cons (hash-ref hsh-cnst var) (cdr prog))
              rep acc)]
    [x
     (when (dataVal? x)
       (parseSEQU (hash-ref hsh-sequ x)))
     (let ([accu (unbox acc)])
       (set-box! acc 0)
       (append (repet (unbox rep) accu 0)
               `(,x)
               (pSEQU-h (cdr prog) rep acc)))]))
(define (repet rep left cnt)
  (let ([lim (min cnt 16)])
    (cond
      [(zero? left) empty]
      [(zero? cnt) (cons rep (repet rep (sub1 left) 1))]
      [(> left lim)
       (cons `(,(symb-append 'dup (string->symbol (number->string lim))))
             (repet rep (- left lim) (+ cnt lim)))]
      [else `((,(symb-append 'dup (string->symbol (number->string left)))))])))

(define (parseLABL sequ)
  (pLABL-h (sequ-val sequ) 0))
(define (pLABL-h prog acc)
  (match (car prog)
    [`(label ,var)
     (hash-set! hsh-labl var acc)
     (pLABL-h (cdr prog) acc)]
    [(dataVal var)
     (let* ([sequ (hash-ref hsh-sequ var)]
            [prg (parseLABL sequ)]
            [len (length prg)])
       (unless (= (sequ-size sequ) len)
         (hash-set! hsh-sequ-mod))
       (cons prg (pLABL-h (cdr prog) (+ len acc))))]
    ))


;; (define (parsePROG prog)
;;   (if (empty? prog) empty
;;       (append
;;        (match (car prog)
;;          [`(seq ,var ,p)
;;           (let ([prg (parsePROG p)])
;;             (hash-set! hsh-seq var (sequ prg (length prg)))
;;             (cons `(label ,var) prg))]
;;
;;          [`(def ,var ,val)
;;           (hash-set! hsh-cnst var val)
;;           `()]
;;          [(? symbol? x)
;;           #:when (hash-has-key? hsh-cnst x)
;;           `(,(hash-ref hsh-cnst x))]
;;
;;          [x `(,x)])
;;        (parsePROG (cdr prog)))))

(define (evmasm-func->asm prog)
  (match prog
    [(list `(dest ,var1) `(dest ,var2) x ...)
     (cons `(label ,(sanVAR var1))
           (evmasm-func->asm (cons `(dest ,var2) x)))]
    ['() '()]

    [_ (append (evmasm-fa-h (car prog)) (evmasm-func->asm (cdr prog)))]))
(define (evmasm-fa-h p)
  (match p
    [`(dataSize ,seq) `(,(dataSize (sanVAR seq)))]

    [`(def ,var ,val) `((def ,var ,(append* (evmasm-fa-h val))))]
    [`(label ,var) `((labl ,(sanVAR var)))]
    [`(dest ,var) `((labl ,(sanVAR var)) (jumpdest))]
    [`(seq ,var ,p) `((seq ,(sanVAR var) ,(evmasm-func->asm p)))]

    [`(,op ,_ ,_)
     #:when (aop? op)
     (evmasm-func->asm `(,(aexp-op (car prog))))]

    [(list op args ...)
     (append* (reverse (cons `((,op))
                             (map evmasm-func->asm
                                  (map list args)))))]

    [(? symbol? var) `(,(sanVAR var))]
    [x `(,x)]))

;; (define (evmasm-func->asm prog)
;;   (define (evmasm-fa-h p)
;;     (match p
;;       [`(dataSize ,seq) `(,(dataSize (sanVAR seq)))]
;;
;;       [`(def ,var ,val) `((def ,var ,(append* (evmasm-fa-h val))))]
;;       [`(label ,var) `((labl ,(sanVAR var)))]
;;       [`(dest ,var) `((labl ,(sanVAR var)) (jumpdest))]
;;       [`(seq ,var ,p) `((seq ,(sanVAR var) ,(evmasm-func->asm p)))]
;;
;;       [`(,op ,_ ,_)
;;        #:when (aop? op)
;;        (evmasm-func->asm `(,(aexp-op (car prog))))]
;;
;;       [(list op args ...)
;;        (append* (reverse (cons `((,op))
;;                                (map evmasm-func->asm
;;                                     (map list args)))))]
;;
;;       [(? symbol? var) `(,(sanVAR var))]
;;       [x `(,x)]))
;;   (append* (map evmasm-fa-h prog)))

;; (define (evmasm-func->asm prog)
;;   (if (empty? prog) empty
;;       (append
;;        (match (car prog)
;;          ;; [`(dataVal ,seq) `(,(dataVal (sanVAR seq)))]
;;          ;; [`(dataLoc ,seq) `(,(dataLoc (sanVAR seq)))]
;;          [`(dataSize ,seq) `(,(dataSize (sanVAR seq)))]
;;
;;          [`(def ,var ,val) `((def ,(sanVAR var) (aexp-op val)))]
;;          [`(label ,var) `((labl ,(sanVAR var)))]
;;          [`(seq ,var ,p) `((seq ,(sanVAR var) ,(evmasm-func->asm p)))]
;;
;;          [`(,op ,_ ,_)
;;           #:when (aop? op)
;;           (evmasm-func->asm `(,(aexp-op (car prog))))]
;;
;;          [(list op args ...)
;;           (append* (reverse (cons `((,op))
;;                                   (map evmasm-func->asm
;;                                        (map list args)))))]
;;
;;          [(? symbol? var) `(,(sanVAR var))]
;;          [x `(,x)])
;;        (evmasm-func->asm (cdr prog)))))

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
  (symb-append '__ var))

;; Appends to symbol
(define (symb-append symb1 symb2)
  (string->symbol (string-append (symbol->string symb1) (symbol->string symb2))))
