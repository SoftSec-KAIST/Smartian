module EVMAnalysis.Domain.FlatInt

open System.Numerics
open EVMAnalysis
open EVMAnalysis.Const

type FlatInt = Bot | Top | Fixed of bigint

type FlatIntModule () =

  inherit AbstractDomain<FlatInt>()

  // Abstract domain functions.

  override __.bot = Bot

  override __.isBot x =
    match x with
    | Bot -> true
    | Top | Fixed _ -> false

  override __.leq x y =
    match x, y with
    | Bot, _ | _, Top -> true
    | _, Bot | Top, _ -> false
    | Fixed i1, Fixed i2 -> i1 = i2

  override __.join x y =
    match x, y with
    | Bot, _ | _, Top -> y
    | _, Bot | Top, _ -> x
    | Fixed a, Fixed b -> if a = b then Fixed a else Top

  override __.toString x =
    match x with
    | Bot -> "_"
    | Top -> "T"
    | Fixed i -> sprintf "0x%s" (i.ToString("X"))

  member __.isTop x =
    match x with
    | Top -> true
    | Bot | Fixed _ -> false

  // Constants.

  member __.top = Top

  member __.zero = Fixed 0I

  // Construct functions.

  member __.ofAddr (addr: Addr) = Fixed (bigint addr)

  member __.ofBigInt i = Fixed i

  // Checker functions.

  member __.isZero = function
    | Bot | Top -> false
    | Fixed i -> i = 0I

  member __.isConst = function
    | Bot | Top -> false
    | Fixed _ -> true

  // Getter function.

  member __.tryGetConst = function
    | Bot | Top -> None
    | Fixed i -> Some i

  // Arithmetic and logical operations.
  member __.notOp x =
    match x with
    | Bot | Top -> x
    | Fixed i -> Fixed(i ^^^ MAX_UINT256)

  member __.add x y =
    match x, y with
    | Bot, _  | _, Bot -> Bot
    | Top, _  | _, Top -> Top
    | Fixed ix, Fixed iy -> Fixed ((ix + iy) &&& MAX_UINT256)

  member __.sub x y =
    match x, y with
    | Bot, _  | _, Bot -> Bot
    | Top, _  | _, Top -> Top
    | Fixed ix, Fixed iy -> Fixed ((ix - iy) &&& MAX_UINT256)

  member __.mul x y =
    match x, y with
    | Bot, _  | _, Bot -> Bot
    | Top, _  | _, Top -> Top
    | Fixed ix, Fixed iy -> Fixed ((ix * iy) &&& MAX_UINT256)

  member __.div x y =
    match x, y with
    | Bot, _  | _, Bot -> Bot
    | Top, _  | _, Top -> Top
    | Fixed ix, Fixed iy -> if iy = 0I then Top else Fixed (ix / iy)

  member __.orOp x y =
    match x, y with
    | Bot, _  | _, Bot -> Bot
    | Top, _  | _, Top -> Top
    | Fixed ix, Fixed iy -> Fixed (ix ||| iy)

  member __.andOp x y =
    match x, y with
    | Bot, _  | _, Bot -> Bot
    | Top, _  | _, Top -> Top
    | Fixed ix, Fixed iy -> Fixed (ix &&& iy)

  member __.xor x y =
    match x, y with
    | Bot, _  | _, Bot -> Bot
    | Top, _  | _, Top -> Top
    | Fixed ix, Fixed iy -> Fixed (ix ^^^ iy)

  member __.extractLSBit x =
    match x with
    | Bot | Top -> x
    | Fixed i -> Fixed (i &&& 1I)

  member __.extractLSByte x =
    match x with
    | Bot | Top -> x
    | Fixed i -> Fixed (i &&& 255I)

  member __.exp x y =
    match x, y with
    | Bot, _  | _, Bot -> Bot
    | Top, _  | _, Top -> Top
    | Fixed ix, Fixed iy -> Fixed (BigInt.exp ix iy &&& MAX_UINT256)

let FlatInt = FlatIntModule() // Use 'FlatInt' like a module.
