structure CastReal : CAST_REAL where type t = Real.t = struct
   open Real
   structure Bits = Word
   val isoBits = NONE
end

structure CastLargeReal : CAST_REAL where type t = LargeReal.t = CastReal
