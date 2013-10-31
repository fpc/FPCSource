{
    Copyright (C) 2013 by Max Nazhalov

    This file, in conjunction with FLT_CORE.INC, implements 2-way conversion
    among the binary floating-point value and its ASCII representation.

    This library is free software; you can redistribute it and/or modify it
    under the terms of the GNU Library General Public License as published by
    the Free Software Foundation; either version 2 of the License, or (at your
    option) any later version with the following modification:

    As a special exception, the copyright holders of this library give you
    permission to link this library with independent modules to produce an
    executable, regardless of the license terms of these independent modules,
    and to copy and distribute the resulting executable under terms of your
    choice, provided that you also meet, for each linked independent module,
    the terms and conditions of the license of that module. An independent
    module is a module which is not derived from or based on this library.
    If you modify this library, you may extend this exception to your version
    of the library, but you are not obligated to do so. If you do not wish to
    do so, delete this exception statement from your version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    See the GNU Library General Public License for more details.

    You should have received a copy of the GNU Library General Public License
    along with this library; if not, write to the Free Software Foundation,
    Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

 ****************************************************************************
}
{
 Algorithm for converting floating-point values to decimal representation
 implemented here has the formal name "Grisu" (basically, "Grisu1").
 It was developed by Florian Loitsch and was presented on the PLDI forum
 in the mid-2010. Detailed algorithm description and rationale can be found
 in his article, referenced below. [1]

 This implementation is purely based on [1], extrapolating the original
 algorithm on to floating point types other than the "double": namely
 "single", "extended" and "float128".

 For self-sufficiency, inverse conversion is also implemented.
 It uses strightforward approach, with the help of high-precision integer
 routines and tables used by Grisu. The main goal was to confirm the "internal
 identity", i.e. conversion path " float -> ASCII -> float " should recover
 the original floating-point representation so long as there are enough
 significant digits provided.

 Generally, the algorithm used has two major drawbacks:
  1. Although "Grisu1" is seems precise in the sense of internal identity, as
     defined above, it is often produces suboptimal output, i.e., in
     exponential representation, output could end up with "...00001",
     "...99998" et al., despite of the fact that in those cases rounding-off
     the last 1 (or, less often, 2) digits would not break internal identity.
     Refer to [1] for further explanations.
  2. Although "Grisu1" is REALLY fast on, e.g. i386, there can be significant
     performance impact on platforms which have long floating point, but
     rather "short" integer ALU (e.g., i8086 with i8087). Despite of the fact
     that an attempt was made to use only "uint32" when converting the
     "single" back and forth, all other floating-point types unavoidably
     require and heavily use "uint64" arithmetics.

 ---

 Implementation was intentionally split into 3 include files to simplify
 maintenance and to avoid complex multiple self-includings.

 This file is the root one. It, depending on the selected mode, defines
 proper conditionals, and then includes FLT_CORE.INC, which implements actual
 conversions. Basically, there are 2 possible compilation modes:
  1. If condition "fpc_softfpu_implementation" is defined before including
     this file, it is assumed to be a part of SoftFPU. In this case, file
     FLT_CORE.INC is included once per every supported floating point type.
     Naming conflicts resolved with macros.
     Supported types are selected with the following pre-defined conditionals:
         SOFTFLOAT_ASCII_FLOAT32
         SOFTFLOAT_ASCII_FLOAT64
         SOFTFLOAT_ASCII_FLOATX80
         SOFTFLOAT_ASCII_FLOAT128
     This mode is basically intended to allow testing and debugging various
     conversion scenarios without having appropriate floating point hardware,
     using only sandbox provided by the SoftFPU. An implementation example of
     this mode is shown in the accompanying file SFPU_FORMAT.PP.
  2. If condition "fpc_softfpu_implementation" is not defined, it is assumed
     to be a part of the system unit, or something else. In this case, file
     FLT_CORE.INC is included only once, and the actual floating point type
     must be defined as "ValReal" before including this file. Also, kind of
     the "ValReal" must be further specified with the following defines (in
     order of priority):
         SUPPORT_FLOAT128
         SUPPORT_EXTENDED
         SUPPORT_DOUBLE
         SUPPORT_SINGLE
     Overall behavior and definitions mimic the REAL2STR.INC file from the
     FPC source tree.

 BEWARE: there are two minor differences between the code generated for mode 1
 and mode 2, which can potentially lead to different results:
   <i> floating-point value packing and unpacking routines are implemented
       differently among these modes (see FLT_PACK.INC).
  <ii> also one internal subroutine involved into conversion to ASCII, namely
       "k_comp", has separate implementations for mode 1 and mode 2.
 Certainly, they should behave the same, and they have never been caught yet
 during testing, but who knows..

 ---

 References:

  [1] Florian Loitsch. Printing Floating-Point Numbers Quickly and Accurately
      with Integers. PLDI'10, June 5-10, 2010, Toronto, Ontario, Canada.
      http://florian.loitsch.com/publications/dtoa-pldi2010.pdf?attredirects=0

  [2] IEEE 754-2008, Standard for Floating-Point Arithmetic. IEEE, New York,
      Aug. 29 2008.

 ****************************************************************************
}
{$push}
{$Q-,R-,B-}

{$ifdef DEBUG}
    {$define grisu1_debug}
    {$C+}
{$else}
    {$undef grisu1_debug}
{$endif}

(*-------------------------------------------------------
 | Compatibility settings
 *-------------------------------------------------------*)
    
    // FPC defaults to "real indefinite" QNaN value, which is negative.
    // Undefine to respect the sign provided during ASCII->float conversion.
    {$define GRISU1_A2F_QNAN_REAL_INDEFINITE}

    // Controls printing of NaN-sign.
    // Undefine to print NaN sign during float->ASCII conversion.
    {$define GRISU1_F2A_NAN_SIGNLESS} // IEEE does not interpret the sign of a NaN, so leave it defined.

    // Controls rounding of generated digits when formatting with narrowed
    // width (either fixed or exponential notation).
    // Traditionally, FPC and BP7/Delphi use "roundTiesToAway" mode.
    // Undefine to use "roundTiesToEven" approach.
    {$define GRISU1_F2A_HALF_ROUNDUP}

    // This one is a hack against Grusu sub-optimality.
    // It may be used only strictly together with GRISU1_F2A_HALF_ROUNDUP.
    // It does not violate most general rules due to the fact that it is
    // applicable only when formatting with narrowed width, where the fine
    // view is more desirable, and the precision is already lost, so it can
    // be used in general-purpose applications.
    // Refer to its implementation.
    {$define GRISU1_F2A_AGRESSIVE_ROUNDUP} // Defining this fixes several tests.

    // Controls rounding of over-required digits during ASCII->float.
    // Undefine to use "roundTiesToEven" approach. [Should have no much sense, though]
    {$undef GRISU1_A2F_HALF_ROUNDUP}

    // Controls which result is returned in case of error during ASCII->float.
    // FPC initializes result to "0.0".
    // Undefine to return (s)NAN.
    {$define GRISU1_A2F_ERROR_RET0} // Leave it defined, otherwise several irrelevant tests will break.

    // Undefine to enable SNaN support.
    // Note: IEEE [754-2008, page 31] requires (1) to recognize "SNaN" during
    // ASCII->float, and (2) to generate the "invalid FP operation" exception
    // either when SNaN is printed as "NaN", or "SNaN" is evaluated to QNaN,
    // so it would be preferable to undefine these settings,
    // but the FPC RTL is not ready for this right now..
    {$define GRISU1_F2A_NO_SNAN}
    {$define GRISU1_A2F_NO_SNAN}

    // Controls how many digits is printed for "single".
    // IEEE claims 9 digits is always enough, and it is confirmed by
    // comprehensive testing.
    // Define to print 10 digits.
    // Note: there is no much sense to print 10 digits, except that it fixes
    // the test "./tests/test/units/sysutils/tfloattostr.pp".
    // Alternate way would be to print 9 digits, and change the test to
    // compare against "6e-9" instead of "6e-10".
    {$define GRISU1_F2A_SINGLE_10DIGITS}

(*-------------------------------------------------------
 | These conditional defines are heavily used internally,
 | so make sure they are not defined around
 *-------------------------------------------------------*)

{$ifdef VALREAL_32}
    {$fatal VALREAL_32 should not be defined here!}
{$endif}
{$ifdef VALREAL_64}
    {$fatal VALREAL_64 should not be defined here!}
{$endif}
{$ifdef VALREAL_80}
    {$fatal VALREAL_80 should not be defined here!}
{$endif}
{$ifdef VALREAL_128}
    {$fatal VALREAL_128 should not be defined here!}
{$endif}
{$ifdef VALREAL_PACK}
    {$fatal VALREAL_PACK should not be defined here!}
{$endif}

(*-------------------------------------------------------
 | Floating point types formatting profile
 *-------------------------------------------------------*)

type
    TReal_Type = (
        RT_S32REAL,  // single
        RT_S64REAL,  // double
        RT_S80REAL,  // extended [80-bit]
        RT_SC80REAL, // extended ["C-extended"; functionally the same as RT_S80REAL, but may be different in alignment and padding]
        RT_C64BIT,   // comp [legacy; just an int64 passed via float]
        RT_CURRENCY, // currency [seems never passed to str_real since it has its own dedicated converters after r5866]
        RT_S128REAL  // float128
    );

const
    float_format: array [ TReal_Type ] of record
        nDig_mantissa, nDig_exp10: integer;
    end = (
{
    Number of mantissa digits is dictated by [2] "IEEE 754-2008", page 32.
    N = 1 + ceiling( p * log10(2) ), where p is the number of significant
    bits, i.e. 24/53/64/113 accordingly (including the implicit one if any)
}
        // RT_S32REAL
        ( nDig_mantissa: {$ifdef GRISU1_F2A_SINGLE_10DIGITS} 10 {$else} 9 {$endif}; 
          nDig_exp10: 2;
        ),
        // RT_S64REAL
        ( nDig_mantissa: 17;
          nDig_exp10: 3;
        ),
        // RT_S80REAL
        ( nDig_mantissa: 21;
          nDig_exp10: 4;
        ),
        // RT_SC80REAL
        ( nDig_mantissa: 21;
          nDig_exp10: 4;
        ),
        // RT_C64BIT
        ( nDig_mantissa: 19; // int64 has 19 digits
          nDig_exp10: 4; // Delphi prints exponent with 4 digits
        ),
        // RT_CURRENCY [seems not used after r5866]
        ( nDig_mantissa: 19;
          nDig_exp10: 2;
        ),
        // RT_S128REAL
        ( nDig_mantissa: 36;
          nDig_exp10: 4;
        )
    );

    C_STR_INF  : string[3] = 'Inf';
    C_STR_QNAN : string[3] = 'Nan';
{$if not ( defined(GRISU1_F2A_NO_SNAN) and defined(GRISU1_A2F_NO_SNAN) )}
    C_STR_SNAN : string[4] = 'SNan';
{$endif GRISU1_*_NO_SNAN}

{$ifdef fpc_softfpu_implementation}
(****************************************************************************
 * 
 * SoftPFU unit: Multiple instances for all supported floating point types
 *
 ****************************************************************************)
{$inline on}
{$macro on}
{$define grisu1_inline}

    {$define ValSInt:=integer}

{$ifdef SOFTFLOAT_ASCII_FLOAT32}
    {$define VALREAL_32}
    {$undef  VALREAL_64}
    {$undef  VALREAL_80}
    {$undef  VALREAL_128}

 // Name remapping
    {$define val_real:=ascii_to_float32}
    {$define ValReal:=float32rec}
    {$define TDIY_FP:=TDIY_FP32}
    {$define TDIY_FP_Power_of_10:=TDIY_FP32_Power_of_10}
    
 // Implementation
    {$info === float32<->ASCII ===}
    {$i flt_core.inc}

{$endif SOFTFLOAT_ASCII_FLOAT32}

{$ifdef SOFTFLOAT_ASCII_FLOAT64}
    {$undef  VALREAL_32}
    {$define VALREAL_64}
    {$undef  VALREAL_80}
    {$undef  VALREAL_128}

 // Name remapping
    {$define val_real:=ascii_to_float64}
    {$define ValReal:=float64}
    {$define TDIY_FP:=TDIY_FP64}
    {$define TDIY_FP_Power_of_10:=TDIY_FP64_Power_of_10}

 // Implementation
    {$info === float64<->ASCII ===}
    {$i flt_core.inc}

{$endif SOFTFLOAT_ASCII_FLOAT64}

{$ifdef SOFTFLOAT_ASCII_FLOATX80}
    {$undef  VALREAL_32}
    {$undef  VALREAL_64}
    {$define VALREAL_80}
    {$undef  VALREAL_128}

 // Name remapping
    {$define val_real:=ascii_to_floatx80}
    {$define ValReal:=floatx80}
    {$define TDIY_FP:=TDIY_FP96}
    {$define TDIY_FP_Power_of_10:=TDIY_FP96_Power_of_10}

 // Implementation
    {$info === floatx80<->ASCII ===}
    {$i flt_core.inc}

{$endif SOFTFLOAT_ASCII_FLOATX80}

{$ifdef SOFTFLOAT_ASCII_FLOAT128}
    {$undef  VALREAL_32}
    {$undef  VALREAL_64}
    {$undef  VALREAL_80}
    {$define VALREAL_128}

 // Name remapping
    {$define val_real:=ascii_to_float128}
    {$define ValReal:=float128}
    {$define TDIY_FP:=TDIY_FP128}
    {$define TDIY_FP_Power_of_10:=TDIY_FP128_Power_of_10}

 // Implementation
    {$info === float128<->ASCII ===}
    {$i flt_core.inc}

{$endif SOFTFLOAT_ASCII_FLOAT128}

 // Clean-up
    {$undef VALREAL_32}
    {$undef VALREAL_64}
    {$undef VALREAL_80}
    {$undef VALREAL_128}
    {$undef VALREAL_PACK}
    {$undef val_real}
    {$undef ValReal}
    {$undef ValSInt}
    {$undef TDIY_FP}
    {$undef TDIY_FP_Power_of_10}
    {$undef grisu1_inline}
    {$info ========================}

{$else not fpc_softfpu_implementation}
(****************************************************************************
 * 
 * System unit: only one native floating point type
 *
 ****************************************************************************)
{$ifdef SYSTEMINLINE}
    {$define grisu1_inline}
{$else}
    {$undef grisu1_inline}
{$endif}

{$if defined(SUPPORT_FLOAT128)}
    {$undef  VALREAL_32}
    {$undef  VALREAL_64}
    {$undef  VALREAL_80}
    {$define VALREAL_128}

{$elseif defined(SUPPORT_EXTENDED)}
    {$undef  VALREAL_32}
    {$undef  VALREAL_64}
    {$define VALREAL_80}
    {$undef  VALREAL_128}

{$elseif defined(SUPPORT_DOUBLE)}
    {$undef  VALREAL_32}
    {$define VALREAL_64}
    {$undef  VALREAL_80}
    {$undef  VALREAL_128}

{$elseif defined(SUPPORT_SINGLE)}
    {$define VALREAL_32}
    {$undef  VALREAL_64}
    {$undef  VALREAL_80}
    {$undef  VALREAL_128}

{$else}
    {$error Unrecognized ValReal type}
{$endif SUPPORT_*}

 // Implementation
    {$i flt_core.inc}

 // Clean-up
    { undef VALREAL_32}
    { undef VALREAL_64}
    { undef VALREAL_80}
    { undef VALREAL_128}
    {$undef VALREAL_PACK}
    {$undef grisu1_inline}

{$endif fpc_softfpu_implementation}

{$pop}
