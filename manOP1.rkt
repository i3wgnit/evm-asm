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
    (codecopy 0 sub_0 (dup1 (dataSize sub_0)))

    ;; The assembler does not actually cares how many inputs an element uses
    (return 0)
    (stop)


    ;; Actual contract with functions
    ;; ------------------------------

    ;; This is sub_0
    (seq sub_0
         (;; Functions declarations
          ;; Two copies of hash on the stack
          (dup1 (div (calldataload 0) (exp 2 #xe0)))

          ;; Payable functions
          ;; --

          ;; Not payable functions.
          (jumpi loc:invalid (gt (callvalue) 0))

          (jumpi fun:expire (eq expire))
          ;; If not calling expire, then use the second copy of hash
          (jumpi fun:claim (eq claim))

          ;; >
          ;; Labels don't add (jumpdest)
          (label loc:invalid)
          (invalid)

          (dest fun:expire)
          ;; continue, if timestamp >= (sload 1)
          (jumpi loc:invalid (lt (timestamp) (sload s_expiration)))
          (selfdestruct (sload s_deployer))
          (stop)

          (dest fun:claim)

          ;; +2 (sload 2) on the stack
          (dup1 (sload s_recipient))

          ;; -1 item
          ;; not(recipient == caller)
          (jumpi loc:invalid (iszero (eq (caller))))
          (mstore 0 (calldataload 4))

          ;; +1 (keccak256)
          (keccak256 0 #x20)

          ;; -1 item
          (jumpi loc:invalid (iszero (eq (sload s_keyHash))))

          (selfdestruct)
          (stop))))))


(evm-assemble prog)
