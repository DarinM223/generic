(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure FunctionalRecordUpdate =
struct
   structure Fold =
   struct
     fun fold (a, f) g = g (a, f)
     fun post (w, g) s =
       w (fn (a, h) => s (a, g o h))

     fun step0 h (a, f) =
       fold (h a, f)
     fun step1 h (a, f) b =
       fold (h (b, a), f)
     fun step2 h (a, f) b c =
       fold (h (b, c, a), f)
   end

  local
    fun next g (f, z) x =
      g (f x, z)
    fun f1 (f, z) x =
      f (z x)
    fun f2 z = next f1 z
    fun f3 z = next f2 z
    fun f4 z = next f3 z
    fun f5 z = next f4 z
    fun f6 z = next f5 z
    fun f7 z = next f6 z
    fun f8 z = next f7 z
    fun f9 z = next f8 z
    fun f10 z = next f9 z
    fun f11 z = next f10 z
    fun f12 z = next f11 z
    fun f13 z = next f12 z
    fun f14 z = next f13 z
    fun f15 z = next f14 z
    fun f16 z = next f15 z
    fun f17 z = next f16 z
    fun f18 z = next f17 z
    fun f19 z = next f18 z
    fun f20 z = next f19 z
    fun f21 z = next f20 z
    fun f22 z = next f21 z
    fun f23 z = next f22 z
    fun f24 z = next f23 z
    fun f25 z = next f24 z
    fun f26 z = next f25 z
    fun f27 z = next f26 z
    fun f28 z = next f27 z
    fun f29 z = next f28 z
    fun f30 z = next f29 z

    fun c0 from = from
    fun c1 from = c0 from f1
    fun c2 from = c1 from f2
    fun c3 from = c2 from f3
    fun c4 from = c3 from f4
    fun c5 from = c4 from f5
    fun c6 from = c5 from f6
    fun c7 from = c6 from f7
    fun c8 from = c7 from f8
    fun c9 from = c8 from f9
    fun c10 from = c9 from f10
    fun c11 from = c10 from f11
    fun c12 from = c11 from f12
    fun c13 from = c12 from f13
    fun c14 from = c13 from f14
    fun c15 from = c14 from f15
    fun c16 from = c15 from f16
    fun c17 from = c16 from f17
    fun c18 from = c17 from f18
    fun c19 from = c18 from f19
    fun c20 from = c19 from f20
    fun c21 from = c20 from f21
    fun c22 from = c21 from f22
    fun c23 from = c22 from f23
    fun c24 from = c23 from f24
    fun c25 from = c24 from f25
    fun c26 from = c25 from f26
    fun c27 from = c26 from f27
    fun c28 from = c27 from f28
    fun c29 from = c28 from f29
    fun c30 from = c29 from f30

    fun makeUpdate cX (from, from', to) record =
      let
        fun ops () = cX from'
        fun vars f = to f record
      in
        Fold.fold ((vars, ops), fn (vars, _) => vars from)
      end
  in
    fun makeUpdate0 z = makeUpdate c0 z
    fun makeUpdate1 z = makeUpdate c1 z
    fun makeUpdate2 z = makeUpdate c2 z
    fun makeUpdate3 z = makeUpdate c3 z
    fun makeUpdate4 z = makeUpdate c4 z
    fun makeUpdate5 z = makeUpdate c5 z
    fun makeUpdate6 z = makeUpdate c6 z
    fun makeUpdate7 z = makeUpdate c7 z
    fun makeUpdate8 z = makeUpdate c8 z
    fun makeUpdate9 z = makeUpdate c9 z
    fun makeUpdate10 z = makeUpdate c10 z
    fun makeUpdate11 z = makeUpdate c11 z
    fun makeUpdate12 z = makeUpdate c12 z
    fun makeUpdate13 z = makeUpdate c13 z
    fun makeUpdate14 z = makeUpdate c14 z
    fun makeUpdate15 z = makeUpdate c15 z
    fun makeUpdate16 z = makeUpdate c16 z
    fun makeUpdate17 z = makeUpdate c17 z
    fun makeUpdate18 z = makeUpdate c18 z
    fun makeUpdate19 z = makeUpdate c19 z
    fun makeUpdate20 z = makeUpdate c20 z
    fun makeUpdate21 z = makeUpdate c21 z
    fun makeUpdate22 z = makeUpdate c22 z
    fun makeUpdate23 z = makeUpdate c23 z
    fun makeUpdate24 z = makeUpdate c24 z
    fun makeUpdate25 z = makeUpdate c25 z
    fun makeUpdate26 z = makeUpdate c26 z
    fun makeUpdate27 z = makeUpdate c27 z
    fun makeUpdate28 z = makeUpdate c28 z
    fun makeUpdate29 z = makeUpdate c29 z
    fun makeUpdate30 z = makeUpdate c30 z

    fun upd z =
      Fold.step2
        (fn (s, f, (vars, ops)) => (fn out => vars (s (ops ()) (out, f)), ops))
        z
    fun set z =
      Fold.step2
        (fn (s, v, (vars, ops)) =>
           (fn out => vars (s (ops ()) (out, fn _ => v)), ops)) z
    fun $ (a, f) = f a
  end
end

functor WithPretty (Arg : WITH_PRETTY_DOM) = let
   structure Result = struct
      (* <-- SML/NJ workaround *)
      open TopLevel
      infix  7 *`
      infix  6 +`
      infixr 6 <^> <+>
      infixr 5 <$> <$$> </> <//>
      infix  4 <\ \>
      infixr 4 </ />
      infix  2 >|
      infixr 2 |<
      infix  1 >>=
      infix  0 &
      infixr 0 -->
      (* SML/NJ workaround --> *)

      structure Fixity = struct
         datatype t = ATOMIC | NONFIX | INFIXL of Int.t | INFIXR of Int.t
      end

      open Fixity

      fun mark f doc = (f, doc)

      open Prettier

      val parens       = (1, (lparen,   rparen))
      val hashParens   = (2, (txt "#(", rparen))
      val braces       = (1, (lbrace,   rbrace))
      val brackets     = (1, (lbracket, rbracket))
      val hashBrackets = (2, (txt "#[", rbracket))

      fun surround (n, p) = nest n o enclose p
      fun atomize (a, d) = if ATOMIC = a then d else surround parens d

      structure Fmt = struct
         type r = {conNest : Int.t Option.t,
                   contString : Bool.t,
                   fieldNest : Int.t Option.t,
                   intRadix : StringCvt.radix,
                   maxDepth : Int.t Option.t,
                   maxLength : Int.t Option.t,
                   maxString : Int.t Option.t,
                   realFmt : StringCvt.realfmt,
                   wordRadix : StringCvt.radix}
         datatype t = T of r

         local
            open FunctionalRecordUpdate
            fun from a b c d e f g h i = { conNest = a, contString = b, fieldNest = c, intRadix = d,
              maxDepth = e, maxLength = f, maxString = g, realFmt = h, wordRadix = i}
            fun to f {conNest, contString, fieldNest, intRadix, maxDepth, maxLength, maxString, realFmt, wordRadix} =
               f conNest contString fieldNest intRadix maxDepth maxLength maxString realFmt wordRadix
            fun updateR r = FunctionalRecordUpdate.makeUpdate9 (from, from, to) r
         in
            fun u f v r = updateR r set f v $
         end

         val default =
             T {conNest = SOME 1,
                contString = true,
                fieldNest = SOME 1,
                intRadix = StringCvt.DEC,
                maxDepth = NONE,
                maxLength = NONE,
                maxString = NONE,
                realFmt = StringCvt.GEN NONE,
                wordRadix = StringCvt.HEX}

         datatype 'a opt =
            O of {get : r -> 'a,
                  set : 'a -> r UnOp.t,
                  chk : 'a Effect.t}

         val notNeg = fn i => if i < 0 then raise Size else ()
         val notNegOpt = Option.app notNeg
         fun chkRealFmt fmt =
             if case fmt
                 of StringCvt.SCI (SOME i) => i < 0
                  | StringCvt.FIX (SOME i) => i < 0
                  | StringCvt.GEN (SOME i) => i < 1
                  | _                      => false
             then raise Size
             else ()

         val conNest = O {get = #conNest, set = u#conNest, chk = notNegOpt}
         val contString = O {get = #contString, set = u#contString, chk = ignore}
         val fieldNest = O {get = #fieldNest, set = u#fieldNest, chk = notNegOpt}
         val intRadix = O {get = #intRadix, set = u#intRadix, chk = ignore}
         val maxDepth = O {get = #maxDepth, set = u#maxDepth, chk = notNegOpt}
         val maxLength = O {get = #maxLength, set = u#maxLength, chk = notNegOpt}
         val maxString = O {get = #maxString, set = u#maxString, chk = notNegOpt}
         val realFmt = O {get = #realFmt, set = u#realFmt, chk = chkRealFmt}
         val wordRadix = O {get = #wordRadix, set = u#wordRadix, chk = ignore}

         fun op & (T opts, (O {set, chk, ...}, v)) = (chk v ; T (set v opts))
         fun op := x = x
         fun ! (O {get, ...}) (T opts) = get opts
      end

      type c = {map : (HashUniv.t, Prettier.t Option.t) HashMap.t,
                cnt : Int.t Ref.t,
                fmt : Fmt.t}
      type v = {maxDepth : OptInt.t}
      datatype e = E of c * v
      type 'a t = e * 'a -> Fixity.t * Prettier.t
      type 'a p = e * 'a -> Prettier.t

      fun inj b a2b = b o Pair.map (id, a2b)

      val txt0b = txt "0b"
      val txt0o = txt "0o"
      val txt0w = txt "0w"
      val txt0wb = txt "0wb"
      val txt0wo = txt "0wo"
      val txt0wx = txt "0wx"
      val txt0x = txt "0x"
      val txtDots = txt "..."
      val txtFalse = txt "false"
      val txtFn = txt "#fn"
      val txtHash = txt "#"
      val txtHashDQuote = txt "#\""
      val txtNlBs = txt "\\n\\"
      val txtBsDots = txt "\\..."
      val txtTrue = txt "true"
      val txtUnit = txt "()"

      val ctorRef = Generics.C "ref"

      fun cyclic aT aP =
          case HashUniv.new {eq = op =, hash = Word32.toWord o Arg.hash aT}
           of (to, _) =>
              fn (e as E ({map, cnt, ...}, _), v) =>
                 case to v
                  of vD =>
                     case HashMap.find map vD
                      of SOME (SOME u) => (ATOMIC, u)
                       | SOME NONE => let
                            val u = txtHash <^>
                                    txt (Int.toString (cnt := !cnt + 1 ; !cnt))
                         in
                            HashMap.insert map (vD, SOME u)
                          ; (ATOMIC, u)
                         end
                       | NONE =>
                         (HashMap.insert map (vD, NONE)
                        ; case aP (e, v)
                           of (f, d) =>
                              (f,
                               lazy (fn () =>
                                        case HashMap.find map vD
                                         of SOME (SOME u) => u <^> equals
                                          | _             => empty) <^> d))

      fun sequ style (Ops.S {toSlice, getItem, ...}) aP
               (e as E ({fmt = Fmt.T r, ...}, _), a) = let
         fun lp (n, d, s) =
             case getItem s
              of NONE        => surround style d
               | SOME (a, s) => let
                    val d = d <^> comma
                 in
                    if SOME 0 = n
                    then surround style (d <$> txtDots)
                    else lp (OptInt.- (n, SOME 1), d <$> group (aP (e, a)), s)
                 end
         open Fmt
      in
         (ATOMIC,
          if SOME 0 = #maxLength r
          then surround style txtDots
          else case getItem (toSlice a)
                of NONE        => op <^> (#2 style)
                 | SOME (a, s) =>
                   lp (OptInt.- (#maxLength r, SOME 1), group (aP (e, a)), s))
      end

      val intPrefix =
       fn StringCvt.BIN => txt0b (* XXX HaMLet-S *)
        | StringCvt.OCT => txt0o (* XXX non-standard *)
        | StringCvt.DEC => empty
        | StringCvt.HEX => txt0x

      fun mkInt (Ops.I {fmt, compare, isoInt = (_, fromInt), ...})
                (E ({fmt = Fmt.T {intRadix, ...}, ...}, _), i) =
          (ATOMIC,
           if LESS = compare (i, fromInt 0)
           then txt "~" <^> intPrefix intRadix <^>
                txt (String.extract (fmt intRadix i, 1, NONE))
           else intPrefix intRadix <^> txt (fmt intRadix i))

      val wordPrefix =
       fn StringCvt.BIN => txt0wb (* XXX HaMLet-S *)
        | StringCvt.OCT => txt0wo (* XXX non-standard *)
        | StringCvt.DEC => txt0w
        | StringCvt.HEX => txt0wx

      fun mkWord fmt (E ({fmt = Fmt.T {wordRadix, ...}, ...}, _), w) =
          (ATOMIC, wordPrefix wordRadix <^> txt (fmt wordRadix w))

      fun mkReal fmt (E ({fmt = Fmt.T {realFmt, ...}, ...}, _), r) =
          (ATOMIC, txt (fmt realFmt r))

      fun depth aP (E (c, {maxDepth}), v) =
          if SOME 0 = maxDepth
          then (ATOMIC, txtDots)
          else aP (E (c, {maxDepth = OptInt.- (maxDepth, SOME 1)}), v)

      fun nested (m, lhs, frhs, s)
                 (ex as (E ({fmt = Fmt.T r, ...}, _), _)) = let
         val rhs = frhs ex
         fun next n = nest n (lhs <$> rhs)
         fun same () = nest m (lhs <+> rhs)
      in
         group (case s r
                 of SOME n => if m <= n then same () else next n
                  | NONE   => same ())
      end

      val exnHandler : Exn.t t Ref.t =
          ref (mark ATOMIC o txtHash <\ op <^> o txt o General.exnName o #2)
      fun regExn aP e2a =
          Ref.modify (fn exnHandler => fn (env, e) =>
                                          case e2a e
                                           of NONE   => exnHandler (env, e)
                                            | SOME a => aP (env, a))
                     exnHandler

      fun iso' bP = inj bP o Iso.to

      structure PrettyRep = LayerRep
        (open Arg
         type 'a t = 'a t and 'a s = 'a t and ('a, 'k) p = 'a p)

      open PrettyRep.This

      structure Pretty = struct
         type 'a monad = e -> 'a * e
         fun return a e = (a, e)
         fun (aM >>= a2bM) e = uncurry a2bM (aM e)

         fun getFmt (e as E ({fmt, ...}, _)) = (fmt, e)
         fun setFmt fmt (E ({cnt, map, ...}, v)) =
             ((), E ({cnt = cnt, fmt = fmt, map = map}, v))

         fun getRemDepth (e as E (_, {maxDepth})) = (maxDepth, e)
         fun setRemDepth remDepth =
             (Fmt.notNegOpt remDepth
            ; fn (E (c, _)) => ((), E (c, {maxDepth = remDepth})))

         structure Fixity = Fixity

         type 'a t = 'a -> (Fixity.t * Prettier.t) monad

         fun getPrinter aT =
             case getT aT
              of aP => fn a => fn e => (aP (e, a), e)
         fun setPrinter aP = mapT (const (Pair.fst o uncurry aP o Pair.swap))
         fun mapPrinter f t = setPrinter (f (getPrinter t)) t

         local
            fun mk con n cmpL cmpR =
                if n < 0 orelse 9 < n then raise Domain else
                   fn c => case txt (Generics.Con.toString c) of c =>
                      fn (aT, bT) => case getT aT & getT bT of aP & bP =>
                         (mapS o const)
                            (fn (e, (a, b)) => let
                                   val (aF, aS) = aP (e, a)
                                   val (bF, bS) = bP (e, b)
                                   val aS = if cmpL aF
                                            then surround parens aS
                                            else aS
                                   val bS = if cmpR bF
                                            then surround parens bS
                                            else bS
                                in
                                   (con n, aS <$> c </> bS)
                                end)
         in
            fun infixL n =
                mk INFIXL n
                   (fn INFIXL l => l <  n | INFIXR r => r <= n | _ => false)
                   (fn INFIXL l => l <= n | INFIXR r => r <= n | _ => false)
            fun infixR n =
                mk INFIXR n
                   (fn INFIXL l => l <= n | INFIXR r => r <= n | _ => false)
                   (fn INFIXL l => l <= n | INFIXR r => r <  n | _ => false)
         end
      end

      fun fmt t =
          case getT t
           of p => fn fmt => fn x =>
              group (#2 (p (E ({map = HashMap.new {eq = HashUniv.eq,
                                                   hash = HashUniv.hash},
                                cnt = ref ~1,
                                fmt = fmt},
                               {maxDepth = Fmt.! Fmt.maxDepth fmt}),
                            x)))
      fun pretty t = fmt t Fmt.default
      fun show t = Prettier.render NONE o pretty t

      local
         open Pretty
      in
         fun withShow toString =
             setPrinter (fn v => return (ATOMIC, txt (toString v)))

         fun withFmt fmt =
             mapPrinter (fn p => fn v => setFmt fmt >>= (fn () => p v))
      end

      structure Open = LayerDepCases
        (fun iso        aT = iso' (getT aT)
         fun isoProduct aP = iso' (getP aP)
         fun isoSum     aS = iso' (getS aS)

         fun aP *` bP = let
            val aP = getP aP
            val bP = getP bP
         in
            fn (e, a & b) => aP (e, a) <^> comma <$> bP (e, b)
         end
         fun T t = group o #2 o getT t
         fun R l = let
            val s = Generics.Label.toString l
            val t = txt s <+> equals
            val m = String.length s + 2
         in
            fn aT => nested (m, t, T aT, #fieldNest)
         end
         fun tuple aP = mark ATOMIC o surround parens o getP aP
         fun record aP = mark ATOMIC o surround braces o getP aP

         fun aS +` bS = let
            val aP = getS aS
            val bP = getS bS
         in
            fn (e, INL a) => aP (e, a)
             | (e, INR b) => bP (e, b)
         end
         fun unit _ = (ATOMIC, txtUnit)
         fun C0 c = const (ATOMIC, txt (Generics.Con.toString c))
         fun C1 c = let
            val s = Generics.Con.toString c
            val t = txt s
            val m = String.length s + 1
         in
            fn aT => mark NONFIX o nested (m, t, atomize o getT aT, #conNest)
         end
         fun data aS = depth (getS aS)

         val Y = Tie.function

         fun exn ? = depth (!exnHandler) ?
         fun regExn0 c = case C0 c of uP => regExn uP o Pair.snd
         fun regExn1 c aT = case C1 c aT of aP => regExn aP o Pair.snd

         fun refc aT =
             cyclic (Arg.Open.refc ignore aT) o flip inj ! |< C1 ctorRef aT
         fun array aT =
             cyclic (Arg.Open.array ignore aT) |<
                    sequ hashParens ArrayOps.ops (T aT)
         fun vector aT = sequ hashBrackets VectorOps.ops (T aT)
         fun list aT = sequ brackets ListOps.ops (T aT)

         fun op --> _ = const (ATOMIC, txtFn)

         local
            val toLit = txt o Substring.translate Char.toString
         in
            fun string (E ({fmt = Fmt.T r, ...}, _), s) = let
               val l = size s
               val n = Int.min (getOpt (#maxString r, l), l)
               val suf = if n < l then txtBsDots else empty
               val s = Substring.substring (s, 0, n)
               fun wide () = toLit s
               fun narrow () =
                   List.foldl1
                      (fn (x, s) => s <^> txtNlBs <$> backslash <^> x)
                      (List.map toLit (Substring.fields (#"\n" <\ op =) s))
            in
               (ATOMIC,
                dquotes ((if #contString r
                          then choice {wide = wide (), narrow = lazy narrow}
                          else wide ())
                         <^> suf))
            end
         end

         fun bool (_, b) = (ATOMIC, if b then txtTrue else txtFalse)
         fun char (_, x) =
             (ATOMIC, txtHashDQuote <^> txt (Char.toString x) <^> dquote)
         val int  = mkInt IntOps.ops
         val real = mkReal Real.fmt
         val word = mkWord Word.fmt

         val fixedInt = mkInt FixedIntOps.ops
         val largeInt = mkInt LargeIntOps.ops

         val largeReal = mkReal LargeReal.fmt
         val largeWord = mkWord LargeWord.fmt

         val word8  = mkWord Word8.fmt
         val word32 = mkWord Word32.fmt
(*
         val word64 = mkWord Word64.fmt
*)

         fun hole () = undefined

         open Arg PrettyRep)
   end
in
   Result :> PRETTY_CASES
      where type ('a,     'x) Open.Rep.t = ('a,     'x) Result.Open.Rep.t
      where type ('a,     'x) Open.Rep.s = ('a,     'x) Result.Open.Rep.s
      where type ('a, 'k, 'x) Open.Rep.p = ('a, 'k, 'x) Result.Open.Rep.p
end
