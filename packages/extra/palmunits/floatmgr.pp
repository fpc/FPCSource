{$MACRO ON}

(******************************************************************************
 *
 * Copyright (c) 1996-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: FloatMgr.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *    New Floating point routines, provided by new IEEE arithmetic
 *    68K software floating point emulator (sfpe) code.
 *
 * History:
 *    9/23/96 - Created by SCL
 *   11/15/96 - First build of NewFloatMgr.lib
 *   11/26/96 - Added FlpCorrectedAdd and FlpCorrectedSub routines
 *   12/30/96 - Added FlpVersion routine
 *    2/ 4/97 - Fixed FlpDoubleBits definition - sign & exp now Int32s
 *                so total size of FlpCompDouble is 64 bits, not 96.
 *    2/ 5/97 - Added note about FlpBase10Info reporting "negative" zero.
 *    7/21/99 - Renamed NewFloatMgr.h to FloatMgr.h.
 *
 *****************************************************************************)

unit floatmgr;

interface

uses palmos, coretraps, errorbase;

(************************************************************************
 * Differences between FloatMgr (PalmOS v1.0) and (this) NewFloatMgr
 ***********************************************************************)
//
// FloatMgr (PalmOS v1.0)       NewFloatMgr
// ----------------------       ---------------------------------------------
// FloatType (64-bits)          use FlpFloat (32-bits) or FlpDouble (64-bits)
//
// fplErrOutOfRange             use _fp_get_fpscr() to retrieve errors
//
// FplInit()                        not necessary
// FplFree()                        not necessary
//
// FplFToA()                        use FlpFToA()
// FplAToF()                        use FlpAToF()
// FplBase10Info()              use FlpBase10Info() [*signP returns sign BIT: 1 if negative]
//
// FplLongToFloat()             use _f_itof() or _d_itod()
// FplFloatToLong()             use _f_ftoi() or _d_dtoi()
// FplFloatToULong()                use _f_ftou() or _d_dtou()
//
// FplMul()                         use _f_mul() or _d_mul()
// FplAdd()                         use _f_add() or _d_add()
// FplSub()                         use _f_sub() or _d_sub()
// FplDiv()                         use _f_div() or _d_div()

(************************************************************************
 * New Floating point manager constants
 ***********************************************************************)

const
  flpVersion_ = $02008000; // first version of NewFloatMgr (PalmOS 2.0)

(*
 * These constants are passed to and received from the _fp_round routine.
 *)

  flpToNearest  = 0;
  flpTowardZero = 1;
  flpUpward     = 3;
  flpDownward   = 2;
  flpModeMask   = $00000030;
  flpModeShift  = 4;

(*
 * These masks define the fpscr bits supported by the sfpe (software floating point emulator).
 * These constants are used with the _fp_get_fpscr and _fp_set_fpscr routines.
 *)

  flpInvalid   = $00008000;
  flpOverflow  = $00004000;
  flpUnderflow = $00002000;
  flpDivByZero = $00001000;
  flpInexact   = $00000800;

(*
 * These constants are returned by _d_cmp, _d_cmpe, _f_cmp, and _f_cmpe:
 *)

  flpEqual     = 0;
  flpLess      = 1;
  flpGreater   = 2;
  flpUnordered = 3;

(************************************************************************
 * New Floating point manager types (private)
 ***********************************************************************)

type
  _sfpe_64_bits = record // for internal use only
    high: Int32;
    low: Int32;
  end;

  sfpe_long_long = _sfpe_64_bits;          // for internal use only
  sfpe_unsigned_long_long = _sfpe_64_bits; // for internal use only

(************************************************************************
 * New Floating point manager types (public)
 ***********************************************************************)

  FlpFloat = Single; //Int32;
  FlpDouble = Double; //_sfpe_64_bits;
  FlpLongDouble = _sfpe_64_bits;

(*
* A double value comprises the fields:
*       0x80000000 0x00000000 -- sign bit (1 for negative)
*       0x7ff00000 0x00000000 -- exponent, biased by 0x3ff == 1023
*       0x000fffff 0xffffffff -- significand == the fraction after an implicit "1."
* So a double has the mathematical form:
*       (-1)^sign_bit * 2^(exponent - bias) * 1.significand
* What follows are some structures (and macros) useful for decomposing numbers.
*)

  FlpDoubleBits = record // for accessing specific fields
    Bits1: UInt32;
{
    UInt32  sign: 1;
    Int32   exp : 11;
    UInt32  manH: 20;
}
    ManL: UInt32;
  end;

(*!!!
typedef union {
        double              d;          // for easy assignment of values
        FlpDouble           fd;     // for calling New Floating point manager routines
        UInt32              ul[2];  // for accessing upper and lower longs
        FlpDoubleBits   fdb;        // for accessing specific fields
} FlpCompDouble;

typedef union {
        float               f;          // for easy assignment of values
        FlpFloat            ff;     // for calling New Floating point manager routines
        UInt32              ul;     // for accessing bits of the float
} FlpCompFloat;
!!!*)

(************************************************************************
 * Useful macros...
 ***********************************************************************)

{
#define BIG_ENDIAN 1
#define __FIRST32(x) *((UInt32 *) &x)
#define __SECOND32(x) *((UInt32 *) &x + 1)
#define __ALL32(x) *((UInt32 *) &x)

#ifdef LITTLE_ENDIAN
#define __LO32(x) *((UInt32 *) &x)
#define __HI32(x) *((UInt32 *) &x + 1)
#define __HIX 1
#define __LOX 0
#else
#define __HI32(x) *((UInt32 *) &x)
#define __LO32(x) *((UInt32 *) &x + 1)
#define __HIX 0
#define __LOX 1
#endif

#define FlpGetSign(x)           ((__HI32(x) & 0x80000000) != 0)
#define FlpIsZero(x)                ( ((__HI32(x) & 0x7fffffff) | (__LO32(x))) == 0)

#define FlpGetExponent(x)       (((__HI32(x) & 0x7ff00000) >> 20) - 1023)


#define FlpNegate(x)                (((FlpCompDouble *)&x)->ul[__HIX] ^= 0x80000000)
#define FlpSetNegative(x)       (((FlpCompDouble *)&x)->ul[__HIX] |= 0x80000000)
#define FlpSetPositive(x)       (((FlpCompDouble *)&x)->ul[__HIX] &= ~0x80000000)
}

(*******************************************************************
 * New Floating point manager errors
 * The constant fplErrorClass is defined in SystemMgr.h
 *******************************************************************)

const
  flpErrOutOfRange = flpErrorClass or 1;

(************************************************************
 * New Floating point manager trap macros
 *************************************************************)

(************************************************************
 * New Floating point manager selectors
 *************************************************************)

type
  sysFloatSelector = Enum; // The order of this enum *MUST* match the
                           // corresponding table in NewFloatDispatch.c

const
  sysFloatBase10Info = 0;                            // 0
  sysFloatFToA = Succ(sysFloatBase10Info);           // 1
  sysFloatAToF = Succ(sysFloatFToA);                 // 2
  sysFloatCorrectedAdd = Succ(sysFloatAToF);         // 3
  sysFloatCorrectedSub = Succ(sysFloatCorrectedAdd); // 4
  sysFloatVersion = Succ(sysFloatCorrectedSub);      // 5

  flpMaxFloatSelector = sysFloatVersion;             // used by NewFloatDispatch.c

type
  sysFloatEmSelector = Enum; // The order of this enum *MUST* match the
                             // sysFloatSelector table in NewFloatDispatch.c
const
  sysFloatEm_fp_round = 0;                                 // 0
  sysFloatEm_fp_get_fpscr = Succ(sysFloatEm_fp_round);     // 1
  sysFloatEm_fp_set_fpscr = Succ(sysFloatEm_fp_get_fpscr); // 2

  sysFloatEm_f_utof = Succ(sysFloatEm_fp_set_fpscr);       // 3
  sysFloatEm_f_itof = Succ(sysFloatEm_f_utof);             // 4
  sysFloatEm_f_ulltof = Succ(sysFloatEm_f_itof);           // 5
  sysFloatEm_f_lltof = Succ(sysFloatEm_f_ulltof);          // 6

  sysFloatEm_d_utod = Succ(sysFloatEm_f_lltof);            // 7
  sysFloatEm_d_itod = Succ(sysFloatEm_d_utod);             // 8
  sysFloatEm_d_ulltod = Succ(sysFloatEm_d_itod);           // 9
  sysFloatEm_d_lltod = Succ(sysFloatEm_d_ulltod);          // 10

  sysFloatEm_f_ftod = Succ(sysFloatEm_d_lltod);            // 11
  sysFloatEm_d_dtof = Succ(sysFloatEm_f_ftod);             // 12
  sysFloatEm_f_ftoq = Succ(sysFloatEm_d_dtof);             // 13
  sysFloatEm_f_qtof = Succ(sysFloatEm_f_ftoq);             // 14
  sysFloatEm_d_dtoq = Succ(sysFloatEm_f_qtof);             // 15
  sysFloatEm_d_qtod = Succ(sysFloatEm_d_dtoq);             // 16

  sysFloatEm_f_ftou = Succ(sysFloatEm_d_qtod);             // 17
  sysFloatEm_f_ftoi = Succ(sysFloatEm_f_ftou);             // 18
  sysFloatEm_f_ftoull = Succ(sysFloatEm_f_ftoi);           // 19
  sysFloatEm_f_ftoll = Succ(sysFloatEm_f_ftoull);          // 20

  sysFloatEm_d_dtou = Succ(sysFloatEm_f_ftoll);            // 21
  sysFloatEm_d_dtoi = Succ(sysFloatEm_d_dtou);             // 22
  sysFloatEm_d_dtoull = Succ(sysFloatEm_d_dtoi);           // 23
  sysFloatEm_d_dtoll = Succ(sysFloatEm_d_dtoull);          // 24

  sysFloatEm_f_cmp = Succ(sysFloatEm_d_dtoll);             // 25
  sysFloatEm_f_cmpe = Succ(sysFloatEm_f_cmp);              // 26
  sysFloatEm_f_feq = Succ(sysFloatEm_f_cmpe);              // 27
  sysFloatEm_f_fne = Succ(sysFloatEm_f_feq);               // 28
  sysFloatEm_f_flt = Succ(sysFloatEm_f_fne);               // 29
  sysFloatEm_f_fle = Succ(sysFloatEm_f_flt);               // 30
  sysFloatEm_f_fgt = Succ(sysFloatEm_f_fle);               // 31
  sysFloatEm_f_fge = Succ(sysFloatEm_f_fgt);               // 32
  sysFloatEm_f_fun = Succ(sysFloatEm_f_fge);               // 33
  sysFloatEm_f_for = Succ(sysFloatEm_f_fun);               // 34

  sysFloatEm_d_cmp = Succ(sysFloatEm_f_for);               // 35
  sysFloatEm_d_cmpe = Succ(sysFloatEm_d_cmp);              // 36
  sysFloatEm_d_feq = Succ(sysFloatEm_d_cmpe);              // 37
  sysFloatEm_d_fne = Succ(sysFloatEm_d_feq);               // 38
  sysFloatEm_d_flt = Succ(sysFloatEm_d_fne);               // 39
  sysFloatEm_d_fle = Succ(sysFloatEm_d_flt);               // 40
  sysFloatEm_d_fgt = Succ(sysFloatEm_d_fle);               // 41
  sysFloatEm_d_fge = Succ(sysFloatEm_d_fgt);               // 42
  sysFloatEm_d_fun = Succ(sysFloatEm_d_fge);               // 43
  sysFloatEm_d_for = Succ(sysFloatEm_d_fun);               // 44

  sysFloatEm_f_neg = Succ(sysFloatEm_d_for);               // 45
  sysFloatEm_f_add = Succ(sysFloatEm_f_neg);               // 46
  sysFloatEm_f_mul = Succ(sysFloatEm_f_add);               // 47
  sysFloatEm_f_sub = Succ(sysFloatEm_f_mul);               // 48
  sysFloatEm_f_div = Succ(sysFloatEm_f_sub);               // 49

  sysFloatEm_d_neg = Succ(sysFloatEm_f_div);               // 50
  sysFloatEm_d_add = Succ(sysFloatEm_d_neg);               // 51
  sysFloatEm_d_mul = Succ(sysFloatEm_d_add);               // 52
  sysFloatEm_d_sub = Succ(sysFloatEm_d_mul);               // 53
  sysFloatEm_d_div = Succ(sysFloatEm_d_sub);               // 54

(************************************************************
 * New Floating point manager routines
 *************************************************************)

                // Note: FlpBase10Info returns the actual sign bit in *signP (1 if negative)
                // Note: FlpBase10Info reports that zero is "negative".
                //          A workaround is to check (*signP && *mantissaP) instead of just *signP.
function FlpBase10Info(a: FlpDouble; var mantissaP: UInt32; var exponentP, signP: Int16): Err;

function FlpFToA(a: FlpDouble; s: PChar): Err;

function FlpAToF(const s: PChar): FlpDouble;

function FlpCorrectedAdd(firstOperand, secondOperand: FlpDouble; howAccurate: Int16): FlpDouble;

function FlpCorrectedSub(firstOperand, secondOperand: FlpDouble;  howAccurate: Int16): FlpDouble;

// These next three functions correspond to the previous three above.
// The signatures are different, but in fact with CodeWarrior for Palm OS
// the structure return values above are implemented via a hidden pointer
// parameter, so corresponding functions are binary compatible.  Programs
// using CodeWarrior to target m68k Palm OS can use either function
// interchangeably.
//
// However, a description of the handling of structure return values is
// missing from the defined Palm OS ABI, and m68k-palmos-gcc does it
// differently.  So programs compiled with GCC using the standard functions
// above are likely to crash: GCC users must use the FlpBuffer* forms of
// these functions.
//
// The FlpBuffer* functions are not available on the Simulator, so you need
// to use the standard versions above if you want Simulator compatibility.
//
// Many of the _d_* functions further below suffer from the same problem.
// This is not an issue, because programs targeting Palm OS devices can use
// operators (+ - * / etc) instead of calling these functions directly.
// (GCC users may wish to use -lnfm -- see the documentation for details.)
//
// See the SDK's SampleCalc example for further discussion.

procedure FlpBufferAToF(var result: FlpDouble; const s: PChar);

procedure FlpBufferCorrectedAdd(var result: FlpDouble; firstOperand, secondOperand: FlpDouble; howAccurate: Int16);

procedure FlpBufferCorrectedSub(var result: FlpDouble; firstOperand, secondOperand: FlpDouble; howAccurate: Int16);

function FlpVersion: UInt32;

//procedure FlpSelectorErrPrv(flpSelector: UInt16); // used only by NewFloatDispatch.c

// The following macros could be useful but are left undefined due to the
// confusion they might cause.  What was called a "float" in PalmOS v1.0 was
// really a 64-bit; in v2.0 "float" is only 32-bits and "double" is 64-bits.
// However, if a v1.0 program is converted to use the NewFloatMgr, these
// macros could be re-defined, or the native _d_ routines could be called.

//#define FlpLongToFloat(x)  _d_itod(x) // similar to 1.0 call, but returns double
//#define FlpFloatToLong(f)  _d_dtoi(f) // similar to 1.0 call, but takes a double
//#define FlpFloatToULong(f) _d_dtou(f) // similar to 1.0 call, but takes a double

(************************************************************
 * New Floating point emulator functions
 *************************************************************)

(*
 * These three functions define the interface to the (software) fpscr
 * of the sfpe. _fp_round not only sets the rounding mode according
 * the low two bits of its argument, but it also returns those masked
 * two bits. This provides some hope of compatibility with less capable
 * emulators, which support only rounding to nearest. A programmer
 * concerned about getting the rounding mode requested can test the
 * return value from _fp_round; it will indicate what the current mode is.
 *
 * Constants passed to and received from _fp_round are:
 *      flpToNearest, flpTowardZero, flpUpward, or flpDownward
 *)

function _fp_round(Value: Int32): Int32;

(*
 * Constants passed to _fp_set_fpscr and received from _fp_get_fpscr are:
 *      flpInvalid, flpOverflow, flpUnderflow, flpDivByZero, or flpInexact
 *)

function _fp_get_fpscr: Int32;
procedure _fp_set_fpscr(Value: Int32);

(*
 * The shorthand here can be determined from the context:
 *      i   --> long (Int32)
 *      u   --> UInt32 (UInt32)
 *      ll  --> long long int
 *      ull --> UInt32 long int
 *      f   --> float
 *      d   --> double
 *      q   --> long double (defaults to double in this implementaton)
 *      XtoY--> map of type X to a value of type Y
 *)

function _f_utof(Value: UInt32): FlpFloat;
function _f_itof(Value: Int32): FlpFloat;
//!!!function _f_ulltof(Value: sfpe_unsigned_long_long): FlpFloat; syscall sysTrapFlpEmDispatch, sysFloatEm_f_ulltof;
//!!!function _f_lltof(Value: sfpe_long_long): FlpFloat; syscall sysTrapFlpEmDispatch, sysFloatEm_f_lltof;

function _d_utod(Value: UInt32): FlpDouble;
function _d_itod(Value: Int32): FlpDouble;
//!!!function _d_ulltod(Value: sfpe_unsigned_long_long): FlpDouble; syscall sysTrapFlpEmDispatch, sysFloatEm_d_ulltod;
//!!!function _d_lltod(Value: sfpe_long_long): FlpDouble; syscall sysTrapFlpEmDispatch, sysFloatEm_d_lltod;

function _f_ftod(Value: FlpFloat): FlpDouble;
function _d_dtof(Value: FlpDouble): FlpFloat;

//!!!function _f_ftoq(Value: FlpFloat): FlpLongDouble; syscall sysTrapFlpEmDispatch, sysFloatEm_f_ftoq;
function _f_qtof(var Value: FlpLongDouble): FlpFloat;

//!!!function _d_dtoq(Value: FlpDouble): FlpLongDouble; syscall sysTrapFlpEmDispatch, sysFloatEm_d_dtoq;
//!!!function _d_qtod(var Value: FlpLongDouble): FlpDouble; syscall sysTrapFlpEmDispatch, sysFloatEm_d_qtod;

function _f_ftou(Value: FlpFloat): UInt32;
function _f_ftoi(Value: FlpFloat): Int32;

//!!!function _f_ftoull(Value: FlpFloat): sfpe_unsigned_long_long; syscall sysTrapFlpEmDispatch, sysFloatEm_f_ftoull;
//!!!function _f_ftoll(Value: FlpFloat): sfpe_long_long; syscall sysTrapFlpEmDispatch, sysFloatEm_f_ftoll;

function _d_dtou(Value: FlpDouble): UInt32;
function _d_dtoi(Value: FlpDouble): Int32;

//!!!function _d_dtoull(Value: FlpDouble): sfpe_unsigned_long_long; syscall sysTrapFlpEmDispatch, sysFloatEm_d_dtoull;
//!!!function _d_dtoll(Value: FlpDouble): sfpe_long_long; syscall sysTrapFlpEmDispatch, sysFloatEm_d_dtoll;

(*
 * The comparison functions _T_Tcmp[e] compare their two arguments,
 * of type T, and return one of the four values defined below.
 * The functions _d_dcmpe and _f_fcmpe, in addition to returning
 * the comparison code, also set the invalid flag in the fpscr if
 * the operands are unordered. Two floating point values are unordered
 * when they enjoy no numerical relationship, as is the case when one
 * or both are NaNs.
 *
 * Return values for _d_cmp, _d_cmpe, _f_cmp, and _f_cmpe are:
 *      flpEqual, flpLess, flpGreater, or flpUnordered
 *
 * The function shorthand is:
 *      eq  --> equal
 *      ne  --> not equal
 *      lt  --> less than
 *      le  --> less than or equal to
 *      gt  --> greater than
 *      ge  --> greater than or equal to
 *      un  --> unordered with
 *      or  --> ordered with (i.e. less than, equal to, or greater than)
 *)

function _f_cmp(Left: FlpFloat; Right: FlpFloat): Int32;
function _f_cmpe(Left: FlpFloat; Right: FlpFloat): Int32;
function _f_feq(Left: FlpFloat; Right: FlpFloat): Int32;
function _f_fne(Left: FlpFloat; Right: FlpFloat): Int32;
function _f_flt(Left: FlpFloat; Right: FlpFloat): Int32;
function _f_fle(Left: FlpFloat; Right: FlpFloat): Int32;
function _f_fgt(Left: FlpFloat; Right: FlpFloat): Int32;
function _f_fge(Left: FlpFloat; Right: FlpFloat): Int32;
function _f_fun(Left: FlpFloat; Right: FlpFloat): Int32;
function _f_for(Left: FlpFloat; Right: FlpFloat): Int32;

function _d_cmp(Left: FlpDouble; Right: FlpDouble): Int32;
function _d_cmpe(Left: FlpDouble; Right: FlpDouble): Int32;
function _d_feq(Left: FlpDouble; Right: FlpDouble): Int32;
function _d_fne(Left: FlpDouble; Right: FlpDouble): Int32;
function _d_flt(Left: FlpDouble; Right: FlpDouble): Int32;
function _d_fle(Left: FlpDouble; Right: FlpDouble): Int32;
function _d_fgt(Left: FlpDouble; Right: FlpDouble): Int32;
function _d_fge(Left: FlpDouble; Right: FlpDouble): Int32;
function _d_fun(Left: FlpDouble; Right: FlpDouble): Int32;
function _d_for(Left: FlpDouble; Right: FlpDouble): Int32;

function _f_neg(Value: FlpFloat): FlpFloat;
function _f_add(Left: FlpFloat; Right: FlpFloat): FlpFloat;
function _f_mul(Left: FlpFloat; Right: FlpFloat): FlpFloat;
function _f_sub(Left: FlpFloat; Right: FlpFloat): FlpFloat;
function _f_div(Left: FlpFloat; Right: FlpFloat): FlpFloat;

function _d_neg(Value: FlpDouble): FlpDouble;
function _d_add(Left: FlpDouble; Right: FlpDouble): FlpDouble;
function _d_mul(Left: FlpDouble; Right: FlpDouble): FlpDouble;
function _d_sub(Left: FlpDouble; Right: FlpDouble): FlpDouble;
function _d_div(Left: FlpDouble; Right: FlpDouble): FlpDouble;

implementation

function __FlpBase10Info(a: FlpDouble; var mantissaP: UInt32; var exponentP, signP: Int16): Err; syscall sysTrapFlpDispatch;
function __FlpFToA(a: FlpDouble; s: PChar): Err; syscall sysTrapFlpDispatch;
function __FlpAToF(const s: PChar): FlpDouble; syscall sysTrapFlpDispatch;
function __FlpCorrectedAdd(firstOperand, secondOperand: FlpDouble; howAccurate: Int16): FlpDouble; syscall sysTrapFlpDispatch;
function __FlpCorrectedSub(firstOperand, secondOperand: FlpDouble;  howAccurate: Int16): FlpDouble; syscall sysTrapFlpDispatch;

procedure __FlpBufferAToF(var result: FlpDouble; const s: PChar); syscall sysTrapFlpDispatch;
procedure __FlpBufferCorrectedAdd(var result: FlpDouble; firstOperand, secondOperand: FlpDouble; howAccurate: Int16); syscall sysTrapFlpDispatch;
procedure __FlpBufferCorrectedSub(var result: FlpDouble; firstOperand, secondOperand: FlpDouble; howAccurate: Int16); syscall sysTrapFlpDispatch;
function __FlpVersion: UInt32; syscall sysTrapFlpDispatch;

function ___fp_round(Value: Int32): Int32; syscall sysTrapFlpEmDispatch;
function ___fp_get_fpscr: Int32; syscall sysTrapFlpEmDispatch;
procedure ___fp_set_fpscr(Value: Int32); syscall sysTrapFlpEmDispatch;
function ___f_utof(Value: UInt32): FlpFloat; syscall sysTrapFlpEmDispatch;
function ___f_itof(Value: Int32): FlpFloat; syscall sysTrapFlpEmDispatch;
function ___d_utod(Value: UInt32): FlpDouble; syscall sysTrapFlpEmDispatch;
function ___d_itod(Value: Int32): FlpDouble; syscall sysTrapFlpEmDispatch;
function ___f_ftod(Value: FlpFloat): FlpDouble; syscall sysTrapFlpEmDispatch;
function ___d_dtof(Value: FlpDouble): FlpFloat; syscall sysTrapFlpEmDispatch;
function ___f_qtof(var Value: FlpLongDouble): FlpFloat; syscall sysTrapFlpEmDispatch;

function ___f_ftou(Value: FlpFloat): UInt32; syscall sysTrapFlpEmDispatch;
function ___f_ftoi(Value: FlpFloat): Int32; syscall sysTrapFlpEmDispatch;

function ___d_dtou(Value: FlpDouble): UInt32; syscall sysTrapFlpEmDispatch;
function ___d_dtoi(Value: FlpDouble): Int32; syscall sysTrapFlpEmDispatch;


function FlpBase10Info(a: FlpDouble; var mantissaP: UInt32; var exponentP, signP: Int16): Err;
begin
 asm
  move.l #$sysFloatBase10Info, D2;
 end;
 FlpBase10Info := __FlpBase10Info(a, mantissaP, exponentP, signP);
end;

function FlpFToA(a: FlpDouble; s: PChar): Err;
begin
 asm
  move.l #$sysFloatFToA, D2;
 end;
 FlpFToA := __FlpFToA(a, s);
end;

function FlpAToF(const s: PChar): FlpDouble;
begin
 asm
  move.l #$sysFloatAToF, D2
 end;
 FlpAToF := __FlpAToF(s);
end;

function FlpCorrectedAdd(firstOperand, secondOperand: FlpDouble; howAccurate: Int16): FlpDouble;
begin
 asm
  move.l #$sysFloatCorrectedAdd, D2
 end;
 FlpCorrectedAdd := __FlpCorrectedAdd(firstOperand, secondOperand, howAccurate);
end;

function FlpCorrectedSub(firstOperand, secondOperand: FlpDouble;  howAccurate: Int16): FlpDouble;
begin
 asm
  move.l #$sysFloatCorrectedSub, D2;
 end;
 FlpCorrectedSub := __FlpCorrectedSub(firstOperand, secondOperand, howAccurate);
end;

procedure FlpBufferAToF(var result: FlpDouble; const s: PChar);
begin
 asm
  move.l #$sysFloatAToF, D2;
 end;
 __FlpBufferAToF(result, s);
end;

procedure FlpBufferCorrectedAdd(var result: FlpDouble; firstOperand, secondOperand: FlpDouble; howAccurate: Int16);
begin
 asm
  move.l #$sysFloatCorrectedAdd, D2;
 end;
 __FlpBufferCorrectedAdd(result, firstOperand, secondOperand, howAccurate);
end;

procedure FlpBufferCorrectedSub(var result: FlpDouble; firstOperand, secondOperand: FlpDouble; howAccurate: Int16);
begin
 asm
  move.l #$sysFloatCorrectedSub, D2
 end;
 __FlpBufferCorrectedSub(result, firstOperand, secondOperand, howAccurate);
end;

function FlpVersion: UInt32;
begin
 asm
  move.l #$sysFloatVersion, D2;
 end;
 FlpVersion := __FlpVersion;
end;

function _fp_round(Value: Int32): Int32;
begin
 asm
  move.l #$sysFloatEm_fp_round, D2;
 end;
 _fp_round := ___fp_round(Value);
end;

function _fp_get_fpscr: Int32;
begin
 asm
  move.l #$sysFloatEm_fp_get_fpscr, D2;
 end;
 _fp_get_fpscr := ___fp_get_fpscr;
end;

procedure _fp_set_fpscr(Value: Int32);
begin
 asm
  move.l #$sysFloatEm_fp_set_fpscr, D2;
 end;
 ___fp_set_fpscr(Value);
end;

function _f_utof(Value: UInt32): FlpFloat;
begin
 asm
  move.l #$sysFloatEm_f_utof, D2;
 end;
 _f_utof := ___f_utof(Value);
end;

function _f_itof(Value: Int32): FlpFloat;
begin
 asm
  move.l #$sysFloatEm_f_itof, D2;
 end;
 _f_itof := ___f_itof(Value);
end;

function _d_utod(Value: UInt32): FlpDouble;
begin
 asm
  move.l #$sysFloatEm_d_utod, D2;
 end;
 _d_utod := ___d_utod(Value);
end;

function _d_itod(Value: Int32): FlpDouble;
begin
 asm
  move.l #$sysFloatEm_d_itod, D2;
 end;
 _d_itod := ___d_itod(Value);
end;

function _f_ftod(Value: FlpFloat): FlpDouble;
begin
 asm
  move.l #$sysFloatEm_f_ftod, D2;
 end;
 _f_ftod := ___f_ftod(Value);
end;

function _d_dtof(Value: FlpDouble): FlpFloat;
begin
 asm
  move.l #$sysFloatEm_d_dtof, D2;
 end;
 _d_dtof := ___d_dtof(Value);
end;

function _f_qtof(var Value: FlpLongDouble): FlpFloat;
begin
 asm
  move.l #$sysFloatEm_f_qtof, D2;
 end;
 _f_qtof := ___f_qtof(Value);
end;

function _f_ftou(Value: FlpFloat): UInt32;
begin
 asm
  move.l #$sysFloatEm_f_ftou, D2;
 end;
 _f_ftou := ___f_ftou(Value);
end;

function _f_ftoi(Value: FlpFloat): Int32;
begin
 asm
  move.l #$sysFloatEm_f_ftoi, D2;
 end;
 _f_ftoi := ___f_ftoi(Value);
end;

function _d_dtou(Value: FlpDouble): UInt32;
begin
 asm
  move.l #$sysFloatEm_d_dtou, D2;
 end;
 _d_dtou := ___d_dtou(Value);
end;

function _d_dtoi(Value: FlpDouble): Int32;
begin
 asm
  move.l #$sysFloatEm_d_dtoi, D2;
 end;
 _d_dtoi := ___d_dtoi(Value);
end;

function ___f_cmp(Left: FlpFloat; Right: FlpFloat): Int32; syscall sysTrapFlpEmDispatch;
function ___f_cmpe(Left: FlpFloat; Right: FlpFloat): Int32; syscall sysTrapFlpEmDispatch;
function ___f_feq(Left: FlpFloat; Right: FlpFloat): Int32; syscall sysTrapFlpEmDispatch;
function ___f_fne(Left: FlpFloat; Right: FlpFloat): Int32; syscall sysTrapFlpEmDispatch;
function ___f_flt(Left: FlpFloat; Right: FlpFloat): Int32; syscall sysTrapFlpEmDispatch;
function ___f_fle(Left: FlpFloat; Right: FlpFloat): Int32; syscall sysTrapFlpEmDispatch;
function ___f_fgt(Left: FlpFloat; Right: FlpFloat): Int32; syscall sysTrapFlpEmDispatch;
function ___f_fge(Left: FlpFloat; Right: FlpFloat): Int32; syscall sysTrapFlpEmDispatch;
function ___f_fun(Left: FlpFloat; Right: FlpFloat): Int32; syscall sysTrapFlpEmDispatch;
function ___f_for(Left: FlpFloat; Right: FlpFloat): Int32; syscall sysTrapFlpEmDispatch;

function ___d_cmp(Left: FlpDouble; Right: FlpDouble): Int32; syscall sysTrapFlpEmDispatch;
function ___d_cmpe(Left: FlpDouble; Right: FlpDouble): Int32; syscall sysTrapFlpEmDispatch;
function ___d_feq(Left: FlpDouble; Right: FlpDouble): Int32; syscall sysTrapFlpEmDispatch;
function ___d_fne(Left: FlpDouble; Right: FlpDouble): Int32; syscall sysTrapFlpEmDispatch;
function ___d_flt(Left: FlpDouble; Right: FlpDouble): Int32; syscall sysTrapFlpEmDispatch;
function ___d_fle(Left: FlpDouble; Right: FlpDouble): Int32; syscall sysTrapFlpEmDispatch;
function ___d_fgt(Left: FlpDouble; Right: FlpDouble): Int32; syscall sysTrapFlpEmDispatch;
function ___d_fge(Left: FlpDouble; Right: FlpDouble): Int32; syscall sysTrapFlpEmDispatch;
function ___d_fun(Left: FlpDouble; Right: FlpDouble): Int32; syscall sysTrapFlpEmDispatch;
function ___d_for(Left: FlpDouble; Right: FlpDouble): Int32; syscall sysTrapFlpEmDispatch;

function ___f_neg(Value: FlpFloat): FlpFloat; syscall sysTrapFlpEmDispatch;
function ___f_add(Left: FlpFloat; Right: FlpFloat): FlpFloat; syscall sysTrapFlpEmDispatch;
function ___f_mul(Left: FlpFloat; Right: FlpFloat): FlpFloat; syscall sysTrapFlpEmDispatch;
function ___f_sub(Left: FlpFloat; Right: FlpFloat): FlpFloat; syscall sysTrapFlpEmDispatch;
function ___f_div(Left: FlpFloat; Right: FlpFloat): FlpFloat; syscall sysTrapFlpEmDispatch;

function ___d_neg(Value: FlpDouble): FlpDouble; syscall sysTrapFlpEmDispatch;
function ___d_add(Left: FlpDouble; Right: FlpDouble): FlpDouble; syscall sysTrapFlpEmDispatch;
function ___d_mul(Left: FlpDouble; Right: FlpDouble): FlpDouble; syscall sysTrapFlpEmDispatch;
function ___d_sub(Left: FlpDouble; Right: FlpDouble): FlpDouble; syscall sysTrapFlpEmDispatch;
function ___d_div(Left: FlpDouble; Right: FlpDouble): FlpDouble; syscall sysTrapFlpEmDispatch;


function _f_cmp(Left: FlpFloat; Right: FlpFloat): Int32;
begin
 asm
  move.l #$sysFloatEm_f_cmp , D2
 end;
 _f_cmp := ___f_cmp(Left, Right);
end;
function _f_cmpe(Left: FlpFloat; Right: FlpFloat): Int32;
begin
 asm
  move.l #$sysFloatEm_f_cmpe , D2
 end;
 _f_cmpe := ___f_cmpe(Left, Right);
end;
function _f_feq(Left: FlpFloat; Right: FlpFloat): Int32;
begin
 asm
  move.l #$sysFloatEm_f_feq , D2
 end;
 _f_feq := ___f_feq(Left, Right);
end;
function _f_fne(Left: FlpFloat; Right: FlpFloat): Int32;
begin
 asm
  move.l #$sysFloatEm_f_fne , D2
 end;
 _f_fne := ___f_fne(Left, Right);
end;
function _f_flt(Left: FlpFloat; Right: FlpFloat): Int32;
begin
 asm
  move.l #$sysFloatEm_f_flt, D2;
 end;
 _f_flt := ___f_flt(Left, Right);
end;
function _f_fle(Left: FlpFloat; Right: FlpFloat): Int32;
begin
 asm
  move.l #$sysFloatEm_f_fle, D2;
 end;
 _f_fle := ___f_fle(Left, Right);
end;
function _f_fgt(Left: FlpFloat; Right: FlpFloat): Int32;
begin
 asm
  move.l #$sysFloatEm_f_fgt, D2;
 end;
 _f_fgt := ___f_fgt(Left, Right);
end;
function _f_fge(Left: FlpFloat; Right: FlpFloat): Int32;
begin
 asm
  move.l #$sysFloatEm_f_fge, D2;
 end;
 _f_fge := ___f_fge(Left, Right);
end;
function _f_fun(Left: FlpFloat; Right: FlpFloat): Int32;
begin
 asm
  move.l #$sysFloatEm_f_fun, D2;
 end;
 _f_fun := ___f_fun(Left, Right);
end;
function _f_for(Left: FlpFloat; Right: FlpFloat): Int32;
begin
 asm
  move.l #$sysFloatEm_f_for, D2
 end;
 _f_for := ___f_for(Left, Right);
end;
function _d_cmp(Left: FlpDouble; Right: FlpDouble): Int32;
begin
 asm
  move.l #$sysFloatEm_d_cmp, D2
 end;
 _d_cmp := ___d_cmp(Left, Right);
end;
function _d_cmpe(Left: FlpDouble; Right: FlpDouble): Int32;
begin
 asm
  move.l #$sysFloatEm_d_cmpe, D2;
 end;
 _d_cmpe := ___d_cmpe(Left, Right);
end;
function _d_feq(Left: FlpDouble; Right: FlpDouble): Int32;
begin
 asm
  move.l #$sysFloatEm_d_feq, D2;
 end;
 _d_feq := ___d_feq(Left, Right);
end;
function _d_fne(Left: FlpDouble; Right: FlpDouble): Int32;
begin
 asm
  move.l #$sysFloatEm_d_fne, D2;
 end;
 _d_fne := ___d_fne(Left, Right);
end;
function _d_flt(Left: FlpDouble; Right: FlpDouble): Int32;
begin
 asm
  move.l #$sysFloatEm_d_flt, D2;
 end;
 _d_flt := ___d_flt(Left, Right);
end;
function _d_fle(Left: FlpDouble; Right: FlpDouble): Int32;
begin
 asm
  move.l #$sysFloatEm_d_fle, D2
 end;
 _d_fle := ___d_fle(Left, Right);
end;
function _d_fgt(Left: FlpDouble; Right: FlpDouble): Int32;
begin
 asm
  move.l #$sysFloatEm_d_fgt, D2;
 end;
 _d_fgt := ___d_fgt(Left, Right);
end;
function _d_fge(Left: FlpDouble; Right: FlpDouble): Int32;
begin
 asm
  move.l #$sysFloatEm_d_fge, D2;
 end;
 _d_fge := ___d_fge(Left, Right);
end;

function _d_fun(Left: FlpDouble; Right: FlpDouble): Int32;
begin
 asm
  move.l #$sysFloatEm_d_fun, D2
 end;
 _d_fun := ___d_fun(Left, Right);
end;

function _d_for(Left: FlpDouble; Right: FlpDouble): Int32;
begin
 asm
  move.l #$sysFloatEm_d_for, D2
 end;
 _d_for := ___d_for(Left, Right);
end;
function _f_neg(Value: FlpFloat): FlpFloat;
begin
 asm
  move.l #$sysFloatEm_f_neg, D2;
 end;
 _f_neg := ___f_neg(Value);
end;
function _f_add(Left: FlpFloat; Right: FlpFloat): FlpFloat;
begin
 asm
  move.l #$sysFloatEm_f_add, D2;
 end;
 _f_add := ___f_add(Left, Right);
end;
function _f_mul(Left: FlpFloat; Right: FlpFloat): FlpFloat;
begin
 asm
  move.l #$sysFloatEm_f_mul, D2;
 end;
 _f_mul := ___f_mul(Left, Right);
end;
function _f_sub(Left: FlpFloat; Right: FlpFloat): FlpFloat;
begin
 asm
  move.l #$sysFloatEm_f_sub, D2;
 end;
 _f_sub := ___f_sub(Left, Right);
end;
function _f_div(Left: FlpFloat; Right: FlpFloat): FlpFloat;
begin
 asm
  move.l #$sysFloatEm_f_div, D2
 end;
 _f_div := ___f_div(Left, Right);
end;
function _d_neg(Value: FlpDouble): FlpDouble;
begin
 asm
  move.l #$sysFloatEm_d_neg, D2;
 end;
 _d_neg := ___d_neg(Value);
end;

function _d_add(Left: FlpDouble; Right: FlpDouble): FlpDouble;
begin
 asm
  move.l #$sysFloatEm_d_add, D2;
 end;
 _d_add := ___d_add(Left, Right);
end;

function _d_mul(Left: FlpDouble; Right: FlpDouble): FlpDouble;
begin
 asm
  move.l #$sysFloatEm_d_mul, D2;
 end;
 _d_mul := ___d_mul(Left, Right);
end;

function _d_sub(Left: FlpDouble; Right: FlpDouble): FlpDouble;
begin
 asm
  move.l #$sysFloatEm_d_sub, D2;
 end;
 _d_sub := ___d_sub(Left, Right);
end;

function _d_div(Left: FlpDouble; Right: FlpDouble): FlpDouble;
begin
 asm
  move.l #$sysFloatEm_d_div, D2;
 end;
 _d_div := ___d_div(Left, Right);
end;

end.
