  codecopy(0x0, bytecodeSize, 0x20)
  sstore(0x0, mload(0x0))
  codecopy(0x0, add(bytecodeSize, 0x20), 0x20)
  sstore(0x1, mload(0x0))
  codecopy(0x0, add(bytecodeSize, 0x40), 0x20)
  sstore(0x2, mload(0x0))
  sstore(0x3, caller)
  dataSize(sub_0)
  dup1
  dataOffset(sub_0)
  0x0
  codecopy
  0x0
  return
stop

sub_0: assembly {
      jumpi(tag_16, iszero(eq(div(calldataload(0x0), exp(0x2, 0xe0)), 0x79599f96)))
      jumpi(tag_24, iszero(gt(callvalue, 0x0)))
      invalid
    tag_24:
      jumpi(tag_34, gt(timestamp, sload(0x1)))
      jump(0x2)
      jump(tag_38)
    tag_34:
      pop(call(sub(gas, 0x15), sload(0x3), balance(address), 0x0, 0x0, 0x0, 0x0))
      stop
    tag_38:
    tag_16:
      jumpi(tag_72, iszero(eq(div(calldataload(0x0), exp(0x2, 0xe0)), 0xbd66528a)))
      jumpi(tag_80, iszero(gt(callvalue, 0x0)))
      invalid
    tag_80:
      jumpi(tag_91, iszero(iszero(eq(caller, sload(0x2)))))
      invalid
    tag_91:
      mstore(0x0, calldataload(0x4))
      mstore(0x0, keccak256(0x0, 0x20))
      jumpi(tag_114, eq(sload(0x0), mload(0x0)))
      jump(0x2)
      jump(tag_118)
    tag_114:
      pop(call(sub(gas, 0x15), sload(0x2), balance(address), 0x0, 0x0, 0x0, 0x0))
      stop
    tag_118:
    tag_72:
      invalid
}

