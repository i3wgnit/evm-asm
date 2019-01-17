#lang racket

(provide evm-assemble)

(define (evm-assemble prog [return (void)]
                      [printOrder (if (void? return) '("Asm:~%" asm "~%Bytesize:~%" size "~%Hex:~%" hex) '())])
  (reset)
  (evmasm-h prog)
  (let* ([pre-asm (asm-pre '__bytecode)]
         [hex (map (lambda (x)
                     (let ([size (add1 (dLog x 256))])
                       (~r x #:base 16 #:min-width (* 2 size) #:pad-string "0")))
                   (asm pre-asm))])

    ;; print
    (for-each
     (lambda (x)
       (for-each
        ((curry apply) printf)
        (match x
          ['asm (map (compose ((curry cons) "~a~%") list) pre-asm)]
          ['size (hash-map hsh-sequ (lambda (key val) `("~a: ~a~%" ,key ,(sequ-size val))))]
          ['hex (map list (append hex `("~%")))]
          [(? list?) `(,x)]
          [_ `((,x))])))
     printOrder)

    ;; return
    (match return
      ['asm pre-asm]
      ['hex (string-append* "" hex)]
      [_ (void)])))

(define (evmasm-h prog)
  (if (list? prog)
      (evmasm `((seq bytecode ,prog)))
      (evmasm-h `(,prog))))


;; --------------
;; | Structures |
;; --------------

;; sequ is a structure that contains snippets of code and their respective bytesize
(struct sequ (val size) #:mutable #:transparent) ;; represents a sequence of opcodes

;; (dataVal sequ) represents the snippet that `sequ` stores
(struct dataVal (sequ) #:transparent) ;; represents a dataVal token

;; (dataSize sequ) represents the bytesize of the snippet that `sequ` stores
(struct dataSize (sequ) #:transparent) ;; represents a dataSize token

;; sequ is a symbol corresponding to a sequence

;; These are present to allow for possible optimisation on arithmetic operations.
(struct atok (op a1 a2) #:transparent) ;; represents an arithmetic token

;; op is a (add, sub, mul, div)
;; ai are (number, symbol, dataSize, op)


;; --------------
;; | Main Logic |
;; --------------

(define (evmasm prog)
  (parsePROG (evmasm-func->asm prog))
  (parseCNST '__bytecode)
  (parseREP '__bytecode)
  (parseLABL '__bytecode))


;; -----------
;; | HELPERS |
;; -----------


;; Functional to assembly
;; ----------------------

;; This transforms functional-like syntax to a list of assembly stack operations.

(define (evmasm-func->asm prog)
  (match prog
    ;; Basic optimisations::

    ;; Optimises jump destinations
    [`((dest ,var1) (dest ,var2) . ,x)
     (evmasm-func->asm `((label ,var1) (dest ,var2) . ,x))]

    ;; Check if empty
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

    [`(,op . ,args)
     (append (append* (reverse (map evmasm-fa-single args)))
             `((,op)))]

    [(? symbol?) `(,(sanVAR p))]
    [_ `(,p)]))

;; Optimize arithmetic expressions
(define (aexp-op prog)
  (match prog
    [`(,op ,ae1 ,ae2)
     #:when (aop? op)
     (let ([a1 (append* (evmasm-fa-single ae1))]
           [a2 (append* (evmasm-fa-single ae2))])
       (cond
         [(or (list? a1)
              (list? a2))
          `(,(opTrans op) ,a1 ,a2)]
         [(and (number? a1)
               (number? a2))
          ((aopTrans op) a1 a2)]
         [else (atok op a1 a2)]))]

    [_ (error 'aexp-op "~a is not a valid aexp" prog)]))


;; Parse Program
;; -------------

;; This is the second pass of the assembler.
;; It populates hsh-labl and hsh-cnst.
;; It also converts (seq ...) statements to (dataVal)

(define (parsePROG prog)
  (append* (map pPROG-h prog)))
(define (pPROG-h p)
  (match p
    [`(label ,var)
     (hash-set! hsh-labl var 0)
     `(,p)]
    [`(def ,var ,val)
     (if (hash-has-key? hsh-cnst var)
         (error 'pPROG "~a defined more than once" var)
         (hash-set! hsh-cnst var (parsePROG val)))
     empty]
    [`(seq ,var ,prog)
     (let ([prg (parsePROG prog)])
       (hash-set! hsh-sequ var (sequ prg 0))
       (parsePROG `((label ,var) ,(dataVal var))))]
    [x `(,x)]))


;; Parse Constants
;; ---------------

;; This is the third pass of the assembler.
;; It replaces every symbol:cnst to its native value.

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
     (let* ([a1 (pCNST-h-h ae1 keys)]
            [a2 (pCNST-h-h ae2 keys)]
            [aexps `(,a1 ,a2)])
       (match aexps
         [`((,(? number?)) (,(? number?)))
          `(,((aopTrans op) a1 a2))]
         [(list-no-order (? (or-compose `(,(compose not len-0?) ,(compose list? car)))) _)
          (append a2 a1 `(,(opTrans op)))]
         [_ `(,(atok op a1 a2))]))]
    [_
     (when (dataVal? p)
       (let ([var (dataVal-sequ p)])
         (parseCNST var)))
     `(,p)]))
(define (len-0? lst)
  (empty? (cdr lst)))


;; Parse Repetition
;; ----------------

;; This is the fourth pass of the assembler.
;; It replaces repetitions with the appropriate number of (dup1).
;; e.g. '(1 1 1 1) -> '(1 (dup1) (dup1) (dup1) (dup1))

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
(define (repet rep cnt)
  (if (zero? cnt) empty
      (cons rep (build-list (sub1 cnt) (const '(dup1))))))


;; Parse Label
;; -----------

;; This is the fifth and upward passes of the assembler.
;; To the contrary of what it's name implies,
;; This step does not only parses labels.
;; It also updates the bytesize value of all `sequ` snippets until done.

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
    ;; Label, add 0
    [`(label ,var)
     (let ([oVal (hash-ref hsh-labl var)])
       (unless (= oVal acc)
         (hash-set! hsh-labl var acc)
         (set-hsh! acc)))
     0]

    ;; Op code, add 1
    [`(,op) 1]

    ;; Number, add push size
    [(? number?) (push-size p)]

    ;; dataVal, add bytesize
    [(dataVal var)
     (pLABL-h var hsh)
     (let ([sequ (hash-ref hsh-sequ var)])
       (sequ-size sequ))]

    ;; Parse value, then recheck
    [_ (let ([val (pLABL-s-h (if (list? p) p `(,p)))])
         (pLABL-single val set-hsh! hsh acc))]))
(define (pLABL-s-h p)
  (match p
    [`(,(? number? n)) n]
    [`(,(? symbol? s)) (hsh-labl-ref s)]
    [`(,(atok op ae1 ae2))
     (let ([a1 (pLABL-s-h ae1)]
           [a2 (pLABL-s-h ae2)])
       (if (and (number? a1)
                (number? a2))
           ((aopTrans op) a1 a2)
           (error 'pLABL "~a is invalid" p)))]
    [`(,(dataSize var))
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


;; Pre-assembler
;; -------------

;; Despite already 10^6 steps into assembly, I am calling this one the 'pre-assembler'.
;; This step replaces all (dataVal sequ) by the sequence `sequ`.
;; This allows for a visualisation of the applied optimisations.

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


;; Assembler
;; ---------

;; This is the actual assembler.
;; It turns asm-opcodes to actual numbers.

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

    [_ (asm-single (pLABL-s-h `(,p)))]))


;; -----------------------
;; | Helpers for helpers |
;; -----------------------

;; Allows for a quick reset of all global hash-tables
(define (reset)
  (hash-clear! hsh-labl)
  (hash-clear! hsh-cnst)
  (hash-clear! hsh-sequ))
(define hsh-labl (make-hash))
(define hsh-cnst (make-hash))
(define hsh-sequ (make-hash))


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

(define ((or-compose lst) elem)
  (ormap (lambda (x) (x elem)) lst))

(define num-or-symb? (or-compose `(,number? ,symbol?)))
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

;; Number to symbol
(define number->symbol (compose string->symbol number->string))

;; Transform a symbol to a hex opcode
(define (opCode op)
  (hash-ref opCodes op))

(define (opCodify lst start)
  (map (lambda (x y) (cons x y)) lst
       (build-list (length lst) ((curry +) start))))

;; Defining all opcodes
(define opCodes
  (make-immutable-hash
   (append* (map (lambda (x) (opCodify (cadr x) (car x)))
                 `([#x0 (stop add mul sub div sdiv mod smod addmod mulmod exp signextend)]
                   [#x10 (lt gt slt sgt eq iszero and or xor not byte)]
                   [#x20 (sha3)] [#x20 (keccak256)]
                   [#x30 (address balance origin caller callvalue calldataload calldatasize calldatacopy codesize codecopy gasprice extcodesize extcodecopy returndatasize returndatacopy)]
                   [#x40 (blockhash coinbase timestamp number difficulty gaslimit)]
                   [#x50 (pop mload mstore mstore8 sload sstore jump jumpi pc msize gas jumpdest)]
                   [#x60 ,(map (compose ((curry symb-append) 'push) number->symbol)
                               (build-list 32 add1))]
                   [#x80 ,(map (compose ((curry symb-append) 'dup) number->symbol)
                               (build-list 16 add1))]
                   [#x90 ,(map (compose ((curry symb-append) 'swap) number->symbol)
                               (build-list 16 add1))]
                   [#xa0 ,(map (compose ((curry symb-append) 'log) number->symbol)
                               (build-list 4 identity))]
                   [#xf0 (create call callcode return delegatecall)]
                   [#xfa (staticcall)]
                   [#xfd (revert invalid selfdestruct)]
                   [#xff (suicide)])))))
