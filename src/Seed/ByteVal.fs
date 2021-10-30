namespace Smartian

/// Represents the concrete value of a single byte, and its corresponding
/// 'approximate path condition'. Conceptually, each byte is constrained with a
/// upper bound and a lower bound (i.e. as an interval). However, we have
/// constructors for some special cases. Handling these cases specially helps in
/// several points, for example in printing readable log messages.
type ByteVal =
  /// A byte whose lower bound and upper bound have the same value.
  | Fixed of byte
  /// A byte that is constrained to be between a lower and upper bound.
  | Interval of (byte * byte)
  /// A byte that is not constrained at all.
  | Undecided of byte
  /// A byte that is not constrained at all, and moreover not mutated at all
  /// since the initialization.
  | Untouched of byte
  | Sampled of byte

module ByteVal =
  /// Initialize a new ByteVal with provided value
  let newByteVal byte = Untouched byte

  let getConcreteByte = function
    | Untouched b | Undecided b | Fixed b | Sampled b -> b
    | Interval (low, high)  -> byte ((uint32 low + uint32 high) / 2u)

  let isFixed = function
    | Fixed _ -> true
    | Untouched _ | Undecided _ | Interval _ | Sampled _ -> false

  let isUnfixed = function
    | Fixed _ -> false
    | Untouched _ | Undecided _ | Interval _ | Sampled _ -> true

  let isSampledByte = function
    | Sampled _ -> true
    | Fixed _ | Untouched _ | Undecided _ | Interval _ -> false

  let isConstrained = function
    | Fixed _ | Interval _ -> true
    | Untouched _ | Undecided _ | Sampled _ -> false

  let isNullByte byteVal =
    getConcreteByte byteVal = 0uy

  let toString = function
    | Untouched b -> sprintf "%02x" b
    | Undecided b -> sprintf "%02x?" b
    | Fixed b -> sprintf "%02x!" b
    // | Interval (low, upper) -> sprintf "%02x@(%02x-%02x)" low low upper
    | Interval (low, upper) ->
      sprintf "%02x@(%02x-%02x)" ((low + upper) / 2uy) low upper
    | Sampled b -> sprintf "%02x*" b

  let getMinMax byteVal =
    match byteVal with
    | Untouched _ | Undecided _ | Sampled _ -> (0uy, 255uy) (* All bytes *)
    | Fixed x -> (x, x)
    | Interval (low, upper) -> (low, upper)
