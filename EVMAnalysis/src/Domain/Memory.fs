module EVMAnalysis.Domain.Memory

open EVMAnalysis.Domain.AbsVal

// Key of memory.
type MemoryAddr = bigint

type MemoryAddrModule () =
  inherit Elem<MemoryAddr>()
  override __.toString (addr: bigint) =
    "0x" + addr.ToString("X")

let MemoryAddr = MemoryAddrModule () // Use 'MemoryAddr' like a module.

type Memory = Map<MemoryAddr,AbsVal>

type MemoryModule () =
  inherit FunDomain<MemoryAddr,AbsVal>(MemoryAddr, AbsVal)

let Memory = MemoryModule () // Use 'Memory' like a module.
