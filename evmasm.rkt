#lang racket

(provide evm-assemble)

(define (evm-assemble prog)
  (reset)
  (if (list? prog)
      (evmasm (evmasm-func->asm `((seq bytecode ,prog))))
      (evm-assemble `((push ,prog)))))


;; Structures

;; (struct cnst (val) #:transparent) ;; represents a constant
;; (struct labl (val) #:transparent) ;; represents a label
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
  (parseCNST '__bytecode)
  (parseREP '__bytecode)
  (parseLABL '__bytecode)
  (let ([pre-asm (asm-pre '__bytecode)])
    (printf "Asm::")
    (for-each ((curry printf) "~%~a") pre-asm)
    (printf "~%Hex::~%")
    (for-each display
              (map (lambda (x)
                     (let ([size (add1 (dLog x 256))])
                       (~r x #:base 16 #:min-width (* 2 size) #:pad-string "0")))
                   (asm pre-asm)))
    (printf "~%Bytesize::")
    (hash-for-each hsh-sequ (lambda (key val) (printf "~%~a: ~a" key (sequ-size val))))))



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
         (error 'pPROG "~a defined more than once" var)
         (hash-set! hsh-cnst var (parsePROG val)))
     empty]
    [`(seq ,var ,prog)
     (let ([prg (parsePROG prog)])
       (hash-set! hsh-sequ var (sequ prg 0))
       (parsePROG `((label ,var) ,(dataVal var))))]
    [`(label ,var)
     (hash-set! hsh-labl var 0)
     `(,p)]
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

(define (parseCNST var)
  (let* ([sequ (hash-ref hsh-sequ var)]
         [nProg (pCNST-h (sequ-val sequ) (hash))])
    (set-sequ-val! sequ nProg)))
(define (pCNST-h prog keys)
  (append* (map (lambda (x) (pCNST-h-h x keys)) prog)))
(define (pCNST-h-h p keys)
  (match p
    [(? symbol? var)
     #:when (hash-has-key? hsh-cnst var)
     (if (hash-has-key? keys var)
         (error 'pCNST "~a is circular" var)
         (let* ([val (hash-ref hsh-cnst var)]
                [nVal (pCNST-h val (hash-set keys var 1))])
           (hash-set! hsh-cnst var nVal)
           nVal))]
    [(atok op ae1 ae2)
     (let* ([a1 (append* (pCNST-h-h ae1 keys))]
            [a2 (append* (pCNST-h-h ae2 keys))]
            [aexps `(,a1 ,a2)])
       (match aexps
            [`(,(? number?) ,(? number?))
             `(,((aopTrans op) a1 a2))]
            [_ (evmasm-fa-single `(,op ,a1 ,a2))]))]
    [_
     (when (dataVal? p)
       (let ([var (dataVal-sequ p)])
         (parseCNST var)))
     `(,p)]))

(define (parseREP var)
  (let* ([sequ (hash-ref hsh-sequ var)]
         [nProg (pREP-h (sequ-val sequ) 0 0)])
    (set-sequ-val! sequ nProg)))
(define (pREP-h prog rep acc)
  (if (empty? prog) (repet rep acc)
      (match (car prog)
        [(? (or-compose `(,number? ,symbol? ,atok?)) x)
         (if (equal? x rep)
             (pREP-h (cdr prog) rep (add1 acc))
             (append (repet rep acc)
                     (pREP-h (cdr prog) x 1)))]
        [x
         (when (dataVal? x)
           (let ([var (dataVal-sequ x)])
             (parseREP var)))
         (append (repet rep acc)
                 `(,x)
                 (pREP-h (cdr prog) 0 0))])))

;; (define (parseSEQU var)
;;   (let* ([sequ (hash-ref hsh-sequ var)]
;;          [nProg (pSEQU-h (sequ-val sequ) 0 0)]
;;          [len (length nProg)])
;;     (set-sequ-val! sequ nProg)
;;     (set-sequ-size! sequ len)))
;; (define (pSEQU-h prog rep acc)
;;   (if (empty? prog) (repet rep acc)
;;       (match (car prog)
;;         [(? symbol? var)
;;          #:when (hash-has-key? hsh-cnst var)
;;          (pSEQU-h (append (hash-ref hsh-cnst var) (cdr prog))
;;                   rep acc)]
;;         [(? num-or-symb? x)
;;          (if (equal? x rep)
;;              (pSEQU-h (cdr prog) rep (add1 acc))
;;              (append (repet rep acc)
;;                      (pSEQU-h (cdr prog) x 1)))]
;;         [x
;;          (when (dataVal? x)
;;            (parseSEQU (dataVal-sequ x)))
;;          (append (repet rep acc)
;;                  `(,(match x
;;                       [(atok op ae1 ae2)]))
;;                  (pSEQU-h (cdr prog) 0 0))])))
(define (repet rep cnt)
  (if (zero? cnt) empty
      (cons rep (build-list (sub1 cnt) (const '(dup1))))))

;; (define (pSEQU-atok aexp)
;;   (match aexp
;;     [(? symbol? x)
;;      #:when (hash-has-key? hsh-cnst x)
;;      (let ([nVal (hash-ref hsh-cnst x)])
;;        (hash-set! hsh-cnst x nVal)
;;        (if ()))]
;;     []))
;; (define (pSEQU-atok-h ae)
;;   (match ae
;;     [(? symbol? x)
;;      #:when (hash-has-key? hsh-cnst x)
;;
;;      ]
;;    ))


;; (if (hash-has-key? keys x)
;;     (error 'pSEQU-atok "~a is circular" x)
;;     (begin
;;       (hash-set! keys x #t)
;;       (let ([nVal (pSEQU-atok-h (hash-ref hsh-cnst x) keys)])
;;         (hash-set! hsh-cnst x nVal))))

(define ((or-compose lst) elem)
  (ormap (lambda (x) (x elem)) lst))
(define num-or-symb? (or-compose `(,number? ,symbol?)))

;; (define (repet rep left cnt)
;;   (let ([lim (min cnt 16)])
;;     (cond
;;       [(zero? left) empty]
;;       [(zero? cnt) (cons rep (repet rep (sub1 left) 1))]
;;       [(> left lim)
;;        (cons `(,(symb-append 'dup (string->symbol (number->string lim))))
;;              (repet rep (- left lim) (+ cnt lim)))]
;;       [else `((,(symb-append 'dup (string->symbol (number->string left)))))])))

(define (parseLABL var)
  (let ([hsh (make-hash)])
    (pLABL-h var hsh)
    (unless (empty? (hash-keys hsh))
      (hash-for-each hsh (lambda (key val)
                           (set-sequ-size! (hash-ref hsh-sequ key) val)))
      (parseLABL var))))
(define (pLABL-h var hsh)
  (let* ([sequ (hash-ref hsh-sequ var)]
         [prog (sequ-val sequ)]
         [set-hsh! (((curry hash-set!) hsh) var)]
         [size (pLABL-h-h prog set-hsh! hsh 0)])
    (when (or (not (= size (sequ-size sequ)))
              (hash-has-key? hsh var))
      (set-hsh! size))))
(define (pLABL-h-h prog set-hsh! hsh acc)
  (if (empty? prog) acc
      (pLABL-h-h (cdr prog) set-hsh! hsh
                 (+ acc (pLABL-single (car prog) set-hsh! hsh acc)))))
(define (pLABL-single p set-hsh! hsh acc)
  (match p
    [`(label ,var)
     (let ([oVal (hash-ref hsh-labl var)])
       (unless (= oVal acc)
         (hash-set! hsh-labl var acc)
         (set-hsh! acc)))
     0]

    [`(,op) 1]
    [(? number?) (push-size p)]

    [(dataVal var)
     (pLABL-h var hsh)
     (let ([sequ (hash-ref hsh-sequ var)])
       (sequ-size sequ))]
    [_ (let ([val (pLABL-s-h p)])
         (pLABL-single val set-hsh! hsh acc))]))
(define (pLABL-s-h p)
  (match p
    [(? number?) p]
    [(? symbol?) (hsh-labl-ref p)]
    [(atok op ae1 ae2)
     (let ([a1 (pLABL-s-h ae1)]
           [a2 (pLABL-s-h ae2)])
       (if (and (number? a1)
                (number? a2))
           ((aopTrans op) a1 a2)
           (error 'pLABL "~a is invalid")))]
    [(dataSize var)
     (let ([sequ (hash-ref hsh-sequ var)])
       (sequ-size sequ))]))
(define (hsh-labl-ref key)
  (hash-ref hsh-labl key (lambda () (error 'label "~a is not defined" key))))

(define (push-size num)
  (+ 2 (dLog num 256)))

(define (dLog num base)
  (dLog-h num base 0))
(define (dLog-h num base acc)
  (if (< num base) acc
      (dLog-h (quotient num base) base (add1 acc))))


(define (asm-pre var)
  (let* ([sequ (hash-ref hsh-sequ var)]
         [prog (sequ-val sequ)])
    (asm-p-h prog)))
(define (asm-p-h prog)
  (append* (map asm-p-single prog)))
(define (asm-p-single p)
  (match p
    [(dataVal var)
     (let* ([sequ (hash-ref hsh-sequ var)]
            [prog (sequ-val sequ)])
       (asm-p-h prog))]
    [_ `(,p)]))

(define (asm prog)
  (append* (map asm-single prog)))
(define (asm-single p)
  (match p
    [(? number?)
     (let* ([lg (dLog p 256)]
            [ps (symb-append 'push (number->symbol (add1 lg)))])
       `(,(opCode ps) ,p))]

    [`(label ,_) empty]
    [`(,op) `(,(opCode op))]

    [_ (asm-single (pLABL-s-h p))]))


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
     (evmasm-func->asm `((label ,var1) (dest ,var2) . ,x))]
    [(? empty?) empty]

    [_ (append (evmasm-fa-single (car prog)) (evmasm-func->asm (cdr prog)))]))
(define (evmasm-fa-single p)
  (match p
    [`(dataSize ,seq) `(,(dataSize (sanVAR seq)))]

    [`(def ,var ,val) `((def ,(sanVAR var) ,(evmasm-fa-single val)))]
    [`(label ,var) `((label ,(sanVAR var)))]
    [`(dest ,var) `((label ,(sanVAR var)) (jumpdest))]
    [`(seq ,var ,prg) `((seq ,(sanVAR var) ,(evmasm-func->asm prg)))]

    [`(,op ,_ ,_)
     #:when (aop? op)
     (evmasm-fa-single (aexp-op p))]

    [(list op args ...)
     (append (append* (reverse (map evmasm-fa-single args)))
             `((,op)))]

    [(? symbol?) `(,(sanVAR p))]
    [_ `(,p)]))

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

;; True if op is an arithmetic operator
(define aop? ((curry hash-has-key?) opTransHash))

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


(define (opCode op)
  (hash-ref opCodes op))
;; (define opCodes
;;   (map (lambda (y x) (cons x y))
;;        (build-list #x100 identity)
;;        (append
;;         '(stop add mul sub div sdiv mod smod addmod mulmod expt signextend)
;;         '(lt gt slt sgt eq iszero and or xor not byte)
;;         '(sha3)
;;         '(address balance origin caller callvalue calldataload calldatasize calldatacopy codesize codecopy gasprice extcodesize extcodecopy returndatasize returndatacopy)
;;         '(blockhash coinbase timestamp number difficulty gaslimit)
;;         '(pop mload mstore mstores sload sstore jump jumpi pc msize gas jumpdest)
;;         (map (compose ((curry symb-append) 'push)
;;                       string->symbol number->string)
;;              (build-list 32 add1))
;;         (map (compose ((curry symb-append) 'dup)
;;                       string->symbol number->string)
;;              (build-list 16 add1))
;;         (map (compose ((curry symb-append) 'swap)
;;                       string->symbol number->string)
;;              (build-list 16 add1))
;;         (map (compose ((curry symb-append) 'log)
;;                       string->symbol number->string)
;;              (build-list 4 add1))
;;         '(create call callcode return)
;;         '(delegatecall staticcall revert invalid selfdestruct))))
(define number->symbol (compose string->symbol number->string))

(define (opCodify lst start)
  (map (lambda (x y) (cons x y)) lst
       (build-list (length lst) ((curry +) start))))

(define opCodes
  (make-immutable-hash
   (append* (map (lambda (x) (opCodify (cadr x) (car x)))
                 `([#x0 (stop add mul sub div sdiv mod smod addmod mulmod exp signextend)]
                   [#x10 (lt gt slt sgt eq iszero and or xor not byte)]
                   [#x20 (sha3)] [#x20 (keccak256)]
                   [#x30 (address balance origin caller callvalue calldataload calldatasize calldatacopy codesize codecopy gasprice extcodesize extcodecopy returndatasize returndatacopy)]
                   [#x40 (blockhash coinbase timestamp number difficulty gaslimit)]
                   [#x50 (pop mload mstore mstores sload sstore jump jumpi pc msize gas jumpdest)]
                   [#x60 ,(map (compose ((curry symb-append) 'push) number->symbol)
                               (build-list 32 add1))]
                   [#x80 ,(map (compose ((curry symb-append) 'dup) number->symbol)
                               (build-list 16 add1))]
                   [#x90 ,(map (compose ((curry symb-append) 'swap) number->symbol)
                               (build-list 16 add1))]
                   [#xa0 ,(map (compose ((curry symb-append) 'log) number->symbol)
                               (build-list 4 add1))]
                   [#xf0 (create call callcode return delegatecall)]
                   [#xfa (staticcall)]
                   [#xfd (revert invalid selfdestruct)])))))
