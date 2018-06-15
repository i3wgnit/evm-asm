#lang racket

(require "evmasm.rkt")

;; @harsh, this is a 1-to-1 replica of the non-optimised LLL assembly.

(define prog '(
;; Constants
;; =========
(def scratch 0)

(def s_keyHash 0)
(def s_expiration 1)
(def s_recipient 2)
(def s_deployer 3)

(def invalid-location 2)

(def claim #xbd66528a)
(def expire #x79599f96)

;; Constructor
;; ===========

(codecopy scratch (dataSize bytecode) #x20)
(sstore s_keyHash (mload scratch))

(codecopy scratch (+ (dataSize bytecode) #x20) #x20)
(sstore s_expiration (mload scratch))

(codecopy scratch (+ (dataSize bytecode) #x40) #x20)
(sstore s_recipient (mload scratch))

(sstore s_deployer (caller))

(codecopy sub_0 0 (dup1 (dataSize sub_0)))
(return 0)
(stop)

;; Actual contract with functions
;; ==============================

(seq sub_0
     ((jumpi loc:invalid (gt (callvalue) 0))

      (div (calldataload 0) (exp 2 #xe0))
      (dup1)
      (jumpi fun:expire (eq expire))
      (jumpi fun:claim (eq claim))

      (label loc:invalid)
      (invalid)

      (dest fun:expire)
      (jumpi loc:invalid (lt (timestamp) (sload #x1)))
      (pop (call (- (gas) #x15) (sload #x3) (balance (address)) 0 0 0 0))
      (stop)

      (dest fun:claim)
      (dup1 (sload #x2))

      ;; -1 item on stack
      (jumpi loc:invalid (iszero (eq (caller))))
      (mstore 0 (calldataload #x4))
      (dup1 (keccak256 0 #x32))

      ;; -1 item on stack
      (jumpi loc:invalid (iszero (eq (sload 0))))

      ;; mstore keccak
      (mstore 0)

      ;; (sload #x2) on top of stack
      0 0 0 0
      (balance (address))
      (swap5)
      (- (gas) #x15)
      (pop)
      (stop)))))


(evm-assemble prog)
