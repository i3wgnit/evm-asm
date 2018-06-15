#lang racket

(require "evmasm.rkt")

;; This is a manually optimised version of the swap

(define constants
  '(;; Constants
    ;; ---------

    ;; Currently, there is no way to import files.
    ;; Actually I lied, this is how you do it


    (def scratch 0)

    (def s_keyHash 0)
    (def s_expiration 1)
    (def s_recipient 2)
    (def s_deployer 3)

    (def invalid-location 2)

    (def claim #xbd66528a)
    (def expire #x79599f96)))

(define prog
  (append constants
  '(;; Constructor
    ;; -----------

    ;; No different from LLL

    (codecopy scratch (dataSize bytecode) #x20)
    (sstore s_keyHash (mload scratch))

    (codecopy scratch (+ (dataSize bytecode) #x20) #x20)
    (sstore s_expiration (mload scratch))

    (codecopy scratch (+ (dataSize bytecode) #x40) #x20)
    (sstore s_recipient (mload scratch))

    (sstore s_deployer (caller))

    ;; 'sub_0 is the location of sub_0
    ;; (dataSize sub_0) is the bytesize of sub_0
    (codecopy sub_0 0 (dup1 (dataSize sub_0)))
    (return 0)
    (stop)


    ;; Actual contract with functions
    ;; ------------------------------

    ;; This is sub_0
    (seq sub_0
         (;; Not payable functions.
          (jumpi loc:invalid (gt (callvalue) 0))

          ;; Two copies of hash on the stack
          (dup1 (div (calldataload 0) (exp 2 #xe0)))

          (jumpi fun:expire (eq expire))

          ;; If not calling expire, then use the second copy of hash
          (jumpi fun:claim (eq claim))

          ;; >
          ;; Labels don't add (jumpdest)
          (label loc:invalid)
          (invalid)

          (dest fun:expire)
          ;; continue, if timestand >= (sload 1)
          (jumpi loc:invalid (lt (timestamp) (sload 1)))
          (sload 3)
          (jump call:end)

          (dest fun:claim)

          ;; +2 (sload 2) on the stack
          (dup1 (sload 2))

          ;; -1 item
          (jumpi loc:invalid (iszero (eq (caller))))
          (mstore 0 (calldataload 4))

          ;; +2 (keccak256)
          (dup1 (keccak256 0 #x20))

          ;; -1 item
          (jumpi loc:invalid (iszero (eq (sload 0))))

          ;; mstore keccak
          (mstore 0)

          (dest call:end)
          ;; remaining (sload #x?) on top of stack
          0 0 0 0
          (balance (address))

          ;; dup (sload #x?)
          (dup6)
          (- (gas) #x15)
          (call)
          (stop))))))


(evm-assemble prog)
