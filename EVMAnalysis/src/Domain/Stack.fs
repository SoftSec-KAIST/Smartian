module EVMAnalysis.Domain.Stack

open EVMAnalysis.Domain.AbsVal

// Key of stack.
type StackAddr = bigint

type StackAddrModule () =
  inherit Elem<StackAddr>()
  override __.toString (addr: bigint) =
    "0x" + addr.ToString("X")

let StackAddr = StackAddrModule () // Use 'StackAddr' like a module.

type Stack = Map<StackAddr,AbsVal>

type StackModule () =
  inherit FunDomain<StackAddr,AbsVal>(StackAddr, AbsVal)

  member __.containsVal x (stack: Stack) =
    Map.exists (fun k v -> v = x) stack

let Stack = StackModule () // Use 'Stack' like a module.
