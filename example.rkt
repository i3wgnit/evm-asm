#lang racket

(require "evmasm.rkt")

;; This is a 1-to-1 replica of the non-optimised LLL assembly output.

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

(codecopy 0 sub_0 (dup1 (dataSize sub_0)))
(return 0)
(stop)

;; Actual contract with functions
;; ==============================

(seq sub_0
     ((jumpi tag_16 (iszero (eq (/ (calldataload 0) (exp 2 #xe0)) expire)))
      (jumpi fun:expire (iszero (gt (callvalue) 0)))
      (invalid)

      (dest fun:expire)
      (jumpi tag_34 (gt (timestamp) (sload #x1)))
      (jump #x2)
      (jump tag_38)

      (dest tag_34)
      (pop (call (- (gas) #x15) (sload #x3) (balance (address)) 0 0 0 0))
      (stop)

      (dest tag_38)
      (dest tag_16)
      (jumpi tag_72 (iszero (eq (/ (calldataload 0) (exp 2 #xe0)) claim)))
      (jumpi fun:claim (iszero (gt (callvalue) 0)))
      (invalid)

      (dest fun:claim)
      (jumpi tag_91 (iszero (iszero (eq (caller) (sload #x2)))))
      (invalid)

      (dest tag_91)
      (mstore 0 (calldataload 4))
      (mstore 0 (keccak256 0 32))
      (jumpi tag_114 (eq (sload 0) (mload 0)))
      (jump #x2)
      (jump tag_118)

      (dest tag_114)
      (pop (call (- (gas) #x15) (sload #x2) (balance (address)) 0 0 0 0))
      (stop)

      (dest tag_118)
      (dest tag_72)
      (invalid)))
    )
  )


(evm-assemble prog)
