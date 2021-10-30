module Smartian.Linear

open Utils

type Fraction =
  {
    Numerator : bigint
    Denominator : bigint
  }
  static member (==) (f1 : Fraction, f2 : Fraction) =
    f1.Numerator * f2.Denominator = f1.Denominator * f2.Numerator

/// Represents (y-y0) = a * (x-x0)
type Linearity = {
  (* The size of comparison operation (determined by cmpb, cmpw, cmpl..) may
   * not always match with the size of input field.
   *)
  Slope      : Fraction // a
  X0         : bigint
  Y0         : bigint
  Target     : bigint // 'y' value we want to achieve
}

let generate slope x0 y0 targetY =
  { Slope = slope; X0 = x0; Y0 = y0; Target = targetY }

let private calcSlope x1 x2 y1 y2 =
  { Numerator = (y2 - y1); Denominator = (x2 - x1) }

let findCommonSlope cmpSize x1 x2 x3 y1 y2 y3 =
  if x1 >= x2 || x2 >= x3 then failwith "BranchInfo out of order"
  let wrapper = getUnsignedMax cmpSize + 1I
  let slope12 = calcSlope x1 x2 y1 y2
  let slope23 = calcSlope x2 x3 y2 y3
  if slope12 = slope23 then
    slope12
  elif y1 < y2 && y3 < y1 && calcSlope x2 x3 y2 (y3 + wrapper) == slope12 then
    slope12 // y3 may have been overflowed
  elif y2 > y3 && y1 < y3 && calcSlope x1 x2 (y1 + wrapper) y2 == slope23 then
    slope23 // y1 may have been overflowed
  elif y1 > y2 && y3 > y1 && calcSlope x2 x3 y2 (y3 - wrapper) == slope12 then
    slope12 // y3 may have been underflowed
  elif y2 < y3 && y1 > y3 && calcSlope x1 x2 (y1 - wrapper) y2 == slope23 then
    slope23 // y1 may have been underflowed
  else { Numerator = 0I; Denominator = 0I }

let toString { Slope = s; Target = targ; X0 = x0; Y0 = y0 } =
  let slopeFloat = float s.Numerator / float s.Denominator
  Printf.sprintf "%A - %A = %f (x - %A)" targ y0 slopeFloat x0