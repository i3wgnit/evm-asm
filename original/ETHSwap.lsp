(seq

  (include "./lib/constants.lsp")
  (include "./lib/utilities.lsp")

  (codecopy scratch (bytecodesize) 0x20) ;; first 32bytes for bytes32 keyHash
  (sstore s_keyHash @scratch)

  (codecopy scratch (+ (bytecodesize) 0x20) 0x20) ;; next 32bytes for uint256 expiration
  (sstore s_expiration @scratch)

  (codecopy scratch (+ (bytecodesize) 0x40) 0x20) ;; next 32bytes for padded address recipient (address: 20bytes)
  (sstore s_recipient @scratch)

  (sstore s_deployer (caller))

  (returnlll
    (seq
      (function expire
        (seq not-payable
          (if (> (timestamp) @@s_expiration)
            (seq
              (send @@s_deployer (balance (address)))
              (stop))
            (jump invalid-location))))

      (function claim
        (seq not-payable only-recipient
          (mstore 0x0 (calldataload 0x04))
          (mstore 0x0 (sha3 0x0 0x20))

          (if (= @@s_keyHash @0x0)
            (seq
              (send @@s_recipient (balance (address)))
              (stop))
            (jump invalid-location))))

      (panic))))
