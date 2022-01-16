{ The code is translated from Bochs by Florian Klaempfl and contains 
  the copyright notice below. The original filename is fsincos.cc

   All changes to the code are copyrighted by the Free Pascal development team and
   released under the same license as the orginal code, see below.
}

{============================================================================
This source file is an extension to the SoftFloat IEC/IEEE Floating-point
Arithmetic Package, Release 2b, written for Bochs (x86 achitecture simulator)
floating point emulation.

THIS SOFTWARE IS DISTRIBUTED AS IS, FOR FREE.  Although reasonable effort has
been made to avoid it, THIS SOFTWARE MAY CONTAIN FAULTS THAT WILL AT TIMES
RESULT IN INCORRECT BEHAVIOR.  USE OF THIS SOFTWARE IS RESTRICTED TO PERSONS
AND ORGANIZATIONS WHO CAN AND WILL TAKE FULL RESPONSIBILITY FOR ALL LOSSES,
COSTS, OR OTHER PROBLEMS THEY INCUR DUE TO THE SOFTWARE, AND WHO FURTHERMORE
EFFECTIVELY INDEMNIFY JOHN HAUSER AND THE INTERNATIONAL COMPUTER SCIENCE
INSTITUTE (possibly via similar legal warning) AGAINST ALL LOSSES, COSTS, OR
OTHER PROBLEMS INCURRED BY THEIR CUSTOMERS AND CLIENTS DUE TO THE SOFTWARE.

Derivative works are acceptable, even for commercial purposes, so long as
(1) the source code for the derivative work includes prominent notice that
the work is derivative, and (2) the source code includes prominent notice with
these four paragraphs for those parts of this code that are retained.
=============================================================================}

{============================================================================
 * Written for Bochs (x86 achitecture simulator) by
 *            Stanislav Shwartsman [sshwarts at sourceforge net]
 * ==========================================================================}

{ the pascal translation is based on https://github.com/lubomyr/bochs/blob/8e0b9abcd81cd24d4d9c68f7fdef2f53bc180d33/cpu/fpu/fsincos.cc

  if you update it, please make a note here

}

{$define FLOAT128}

{$define USE_estimateDiv128To64}

{$I f80constant.inc}

const
  floatx80_one : floatx80 = 
     (
         low: QWord($8000000000000000);
         high: $3fff;
     );

{ reduce trigonometric function argument using 128-bit precision
   M_PI approximation }
function argument_reduction_kernel(aSig0: Bit64u; Exp: Integer; var zSig0, zSig1: Bit64u) : Bit64u;
var
    aSig1, term0, term1, term2, q : Bit64u;
begin
    aSig1 := 0;

    shortShift128Left(aSig1, aSig0, Exp, aSig1, aSig0);
    q := estimateDiv128To64(aSig1, aSig0, FLOAT_PI_HI);
    mul128By64To192(FLOAT_PI_HI, FLOAT_PI_LO, q, term0, term1, term2);
    sub128(aSig1, aSig0, term0, term1, zSig1, zSig0);
    while (Bit64s(zSig1) < 0) do
    begin
        dec(q);
        add192(zSig1, zSig0, term2, 0, FLOAT_PI_HI, FLOAT_PI_LO, zSig1, zSig0, term2);
    end;
    zSig1 := term2;
    Result := q;
end;

function reduce_trig_arg(expDiff: Integer; var zSign: Integer; var aSig0, aSig1: Bit64u) : Integer;
var
  term0, term1, q : Bit64u;
  eq, lt : Integer;
begin
    q := 0;

    if (expDiff < 0) then
    begin
        shift128Right(aSig0, 0, 1, aSig0, aSig1);
        expDiff := 0;
    end;
    if (expDiff > 0) then
    begin
        q := argument_reduction_kernel(aSig0, expDiff, aSig0, aSig1);
    end
    else 
    begin
        if (FLOAT_PI_HI <= aSig0) then
        begin
            dec(aSig0, FLOAT_PI_HI);
            q := 1;
        end;
    end;

    shift128Right(FLOAT_PI_HI, FLOAT_PI_LO, 1, &term0, &term1);
    if (not(lt128(aSig0, aSig1, term0, term1))<>0) then
    begin
        lt := lt128(term0, term1, aSig0, aSig1);
        eq := eq128(aSig0, aSig1, term0, term1);

        if (((eq<>0) and ((q and 1)<>0)) or (lt<>0)) then
        begin
            zSign := not(zSign);
            inc(q);
        end;
        if (lt<>0) then
          sub128(FLOAT_PI_HI, FLOAT_PI_LO, aSig0, aSig1, &aSig0, &aSig1);
    end;

    Result:= Integer(q and 3);
end;


const
  sin_arr: array[0..10] of float128 = (    
    (low: $3fff000000000000; high: $0000000000000000), {  1 }
    (low: $bffc555555555555; high: $5555555555555555), {  3 }
    (low: $3ff8111111111111; high: $1111111111111111), {  5 }
    (low: $bff2a01a01a01a01; high: $a01a01a01a01a01a), {  7 }
    (low: $3fec71de3a556c73; high: $38faac1c88e50017), {  9 }
    (low: $bfe5ae64567f544e; high: $38fe747e4b837dc7), { 11 }
    (low: $3fde6124613a86d0; high: $97ca38331d23af68), { 13 }
    (low: $bfd6ae7f3e733b81; high: $f11d8656b0ee8cb0), { 15 }
    (low: $3fce952c77030ad4; high: $a6b2605197771b00), { 17 }
    (low: $bfc62f49b4681415; high: $724ca1ec3b7b9675), { 19 }
    (low: $3fbd71b8ef6dcf57; high: $18bef146fcee6e45)  { 21 }
  );


  cos_arr: array[0..10] of float128 = (
    (low: $3fff000000000000; high: $0000000000000000), {  0 }
    (low: $bffe000000000000; high: $0000000000000000), {  2 }
    (low: $3ffa555555555555; high: $5555555555555555), {  4 }
    (low: $bff56c16c16c16c1; high: $6c16c16c16c16c17), {  6 }
    (low: $3fefa01a01a01a01; high: $a01a01a01a01a01a), {  8 }
    (low: $bfe927e4fb7789f5; high: $c72ef016d3ea6679), { 10 }
    (low: $3fe21eed8eff8d89; high: $7b544da987acfe85), { 12 }
    (low: $bfda93974a8c07c9; high: $d20badf145dfa3e5), { 14 }
    (low: $3fd2ae7f3e733b81; high: $f11d8656b0ee8cb0), { 16 }
    (low: $bfca6827863b97d9; high: $77bb004886a2c2ab), { 18 }
    (low: $3fc1e542ba402022; high: $507a9cad2bf8f0bb)  { 20 }
  );


{$I f80poly.inc}

{ 0 <= x <= pi/4 }
function poly_sin(x: float128): float128;inline;
begin
    //                 3     5     7     9     11     13     15
    //                x     x     x     x     x      x      x
    // sin (x) ~ x - --- + --- - --- + --- - ---- + ---- - ---- =
    //                3!    5!    7!    9!    11!    13!    15!
    //
    //                 2     4     6     8     10     12     14
    //                x     x     x     x     x      x      x
    //   := x * [ 1 - --- + --- - --- + --- - ---- + ---- - ---- ] =
    //                3!    5!    7!    9!    11!    13!    15!
    //
    //           3                          3
    //          --       4k                --        4k+2
    //   p(x) := >  C  * x   > 0     q(x) := >  C   * x     < 0
    //          --  2k                     --  2k+1
    //          k=0                        k=0
    //
    //                          2
    //   sin(x) ~ x * [ p(x) + x * q(x) ]
    //

    Result := OddPoly(x, sin_arr, length(sin_arr));
end;


{ 0 <= x <= pi/4 }
function poly_cos(x: float128): float128;inline;
begin
    //                 2     4     6     8     10     12     14
    //                x     x     x     x     x      x      x
    // cos (x) ~ 1 - --- + --- - --- + --- - ---- + ---- - ----
    //                2!    4!    6!    8!    10!    12!    14!
    //
    //           3                          3
    //          --       4k                --        4k+2
    //   p(x) := >  C  * x   > 0     q(x) := >  C   * x     < 0
    //          --  2k                     --  2k+1
    //          k=0                        k=0
    //
    //                      2
    //   cos(x) ~ [ p(x) + x * q(x) ]
    //

    Result := EvenPoly(x, cos_arr, length(cos_arr));
end;


procedure sincos_invalid(sin_a: pfloatx80; cos_a: pfloatx80; a: floatx80);
begin
    if assigned(sin_a) then
      sin_a^ := a;
    if assigned(cos_a) then
      cos_a^ := a;
end;


procedure sincos_tiny_argument(sin_a, cos_a: pfloatx80; a: floatx80);
begin
    if assigned(sin_a) then
      sin_a^ := a;
    if assigned(cos_a) then
      cos_a^ := floatx80_one;
end;


function floatx80_chs(const a: floatx80): floatx80;
begin
    Result := a;
    Result.high := Result.high xor $8000;
end;


function sincos_approximation(neg: Integer; r: float128; quotient: Bit64u): floatx80;
begin
    if (quotient and $1)<>0 then
    begin
        r := poly_cos(r);
        neg := 0;
    end
    else  
    begin
        r := poly_sin(r);
    end;

    result := float128_to_floatx80(r);
    if (quotient and $2)<>0 then
        neg := neg xor 1;

    if (neg<>0) then
        floatx80_chs(result);
end;

// =================================================
// FSINCOS               Compute sin(x) and cos(x)
// =================================================

//
// Uses the following identities:
// ----------------------------------------------------------
//
//  sin(-x) := -sin(x)
//  cos(-x) :=  cos(x)
//
//  sin(x+y) := sin(x)*cos(y)+cos(x)*sin(y)
//  cos(x+y) := sin(x)*sin(y)+cos(x)*cos(y)
//
//  sin(x+ pi/2)  :=  cos(x)
//  sin(x+ pi)    := -sin(x)
//  sin(x+3pi/2)  := -cos(x)
//  sin(x+2pi)    :=  sin(x)
//

function fsincos(a: floatx80;sin_a, cos_a: pfloatx80): Integer;
var
    aSig0, aSig1: Bit64u;
    aExp, zExp, expDiff: Bit32s;
    aSign, zSign, q: Integer;
    r: float128;
label
    invalid;    
begin
    aSig1 := 0;
    q := 0;

    // handle unsupported extended double-precision floating encodings
    if (floatx80_is_unsupported(a)<>0) then
    begin
        goto invalid;
    end;

    aSig0 := extractFloatx80Frac(a);
    aExp := extractFloatx80Exp(a);
    aSign := extractFloatx80Sign(a);

    { invalid argument }
    if (aExp = $7FFF) then
    begin
        if (Bit64u(aSig0 shl 1)<>0) then
        begin
            sincos_invalid(sin_a, cos_a, propagateFloatx80NaN(a, a)); // not sure if this is correctly translated (FK)
            Result := 0;
            Exit;
        end;

    invalid:
        float_raise(float_flag_invalid);
        sincos_invalid(sin_a, cos_a, floatx80_default_nan);
        Result := 0;
        Exit;
    end;

    if (aExp = 0) then
    begin
        if (aSig0 = 0) then
        begin
            sincos_tiny_argument(sin_a, cos_a, a);
            Result := 0;
            Exit;
        end;

        float_raise(float_flag_denormal);

        { handle pseudo denormals }
        if (not(aSig0 and QWord($8000000000000000))<>0) then
        begin
            float_raise(float_flag_inexact);
            if assigned(sin_a) then
                float_raise(float_flag_underflow);
            sincos_tiny_argument(sin_a, cos_a, a);
            Result := 0;
            Exit;
        end;

        normalizeFloatx80Subnormal(aSig0, aExp, aSig0);
    end;

    zSign := aSign;
    zExp := FLOATX80_EXP_BIAS;
    expDiff := aExp - zExp;

    { argument is out-of-range }
    if (expDiff >= 63) then    
        Exit(-1);

    float_raise(float_flag_inexact);

    if (expDiff < -1) then
    begin    // doesn't require reduction
        if (expDiff <= -68) then
        begin
            a := packFloatx80(aSign, aExp, aSig0);
            sincos_tiny_argument(sin_a, cos_a, a);
            Result := 0;
            Exit;
        end;
        zExp := aExp;
    end
    else begin
        q := reduce_trig_arg(expDiff, zSign, aSig0, aSig1);
    end;

    { **************************** }
    { argument reduction completed }
    { **************************** }

    { using float128 for approximation }
    r := normalizeRoundAndPackFloat128(0, zExp-$10, aSig0, aSig1);

    if (aSign<>0) then
      q := -q;
    if assigned(sin_a) then
      sin_a^ := sincos_approximation(zSign, r,   q);
    if assigned(cos_a) then
      cos_a^ := sincos_approximation(zSign, r, q+1);

    Result := 0;
end;

function fsin(var a: floatx80): Integer;
begin
    Result := fsincos(a, @a, nil);
end;

function fcos(var a: floatx80): Integer;
begin
    Result := fsincos(a, nil, @a);
end;


// =================================================
// FPTAN                 Compute tan(x)
// =================================================

//
// Uses the following identities:
//
// 1. ----------------------------------------------------------
//
//  sin(-x) := -sin(x)
//  cos(-x) :=  cos(x)
//
//  sin(x+y) := sin(x)*cos(y)+cos(x)*sin(y)
//  cos(x+y) := sin(x)*sin(y)+cos(x)*cos(y)
//
//  sin(x+ pi/2)  :=  cos(x)
//  sin(x+ pi)    := -sin(x)
//  sin(x+3pi/2)  := -cos(x)
//  sin(x+2pi)    :=  sin(x)
//
// 2. ----------------------------------------------------------
//
//           sin(x)
//  tan(x) := ------
//           cos(x)
//

function ftan(var a: floatx80): Integer;
var
    aSig0, aSig1: Bit64u;
    aExp, zExp, expDiff: Bit32s;
    aSign, zSign, q: Integer;    
    r, cos_r, sin_r: float128;
label
    invalid;
begin
    aSig1 := 0;
    q := 0;

    // handle unsupported extended double-precision floating encodings
    if (floatx80_is_unsupported(a)<>0) then
    begin
        goto invalid;
    end;

    aSig0 := extractFloatx80Frac(a);
    aExp := extractFloatx80Exp(a);
    aSign := extractFloatx80Sign(a);

    { invalid argument }
    if (aExp = $7FFF) then
    begin
        if (Bit64u(aSig0 shl 1)<>0) then
        begin
            a := propagateFloatx80NaN(a, a); // not sure if this is correctly translated (FK)
            Result := 0;
            Exit;
        end;

    invalid:
        float_raise(float_flag_invalid);
        a := floatx80_default_nan;
        Result := 0;
        Exit;
    end;

    if (aExp = 0) then
    begin
        if (aSig0 = 0) then
            Exit(0);
        float_raise(float_flag_denormal);
        { handle pseudo denormals }
        if (not(aSig0 and QWord($8000000000000000))<>0) then
        begin
            float_raise([float_flag_inexact,float_flag_underflow]);
            Result := 0;
            Exit;
        end;
        normalizeFloatx80Subnormal(aSig0, aExp, aSig0);
    end;

    zSign := aSign;
    zExp := FLOATX80_EXP_BIAS;
    expDiff := aExp - zExp;

    { argument is out-of-range }
    if (expDiff >= 63) then
        Exit(-1);

    float_raise(float_flag_inexact);

    if (expDiff < -1) then
    begin    // doesn't require reduction
        if (expDiff <= -68) then
        begin
            a := packFloatx80(aSign, aExp, aSig0);
            Result := 0;
            exit;
        end;
        zExp := aExp;
    end
    else 
    begin
        q := reduce_trig_arg(expDiff, zSign, aSig0, aSig1);
    end;

    { **************************** }
    { argument reduction completed }
    { **************************** }

    { using float128 for approximation }
    r := normalizeRoundAndPackFloat128(0, zExp-$10, aSig0, aSig1);

    sin_r := poly_sin(r);
    cos_r := poly_cos(r);

    if (q and $1)<>0 then
    begin
        r := float128_div(cos_r, sin_r);
        zSign :=  zSign xor 1;
    end
    else
    begin
        r := float128_div(sin_r, cos_r);
    end;

    a := float128_to_floatx80(r);
    if (zSign<>0) then
        floatx80_chs(a);

    Result := 0;
end;
