{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2004 by Florian Klaempfl
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

function FPUExceptionMaskToSoftFloatMask(const Mask: TFPUExceptionMask): byte;
begin
    result:=0;
    if exInvalidOp in Mask then
      result:=result or (1 shl ord(exInvalidOp));
    if exDenormalized in Mask then
      result:=result or (1 shl ord(exDenormalized));
    if exZeroDivide in Mask then
      result:=result or (1 shl ord(exZeroDivide));
    if exOverflow in Mask then
      result:=result or (1 shl ord(exOverflow));
    if exUnderflow in Mask then
      result:=result or (1 shl ord(exUnderflow));
    if exPrecision in Mask then
      result:=result or (1 shl ord(exPrecision));
end;

function SoftFloatMaskToFPUExceptionMask(const Mask: byte): TFPUExceptionMask;
begin
    result:=[];
    if (mask and (1 shl ord(exInvalidOp)) <> 0) then
      include(result,exInvalidOp);
    if (mask and (1 shl ord(exDenormalized)) <> 0) then
      include(result,exDenormalized);
    if (mask and (1 shl ord(exZeroDivide)) <> 0) then
      include(result,exZeroDivide);
    if (mask and (1 shl ord(exOverflow)) <> 0) then
      include(result,exOverflow);
    if (mask and (1 shl ord(exUnderflow)) <> 0) then
      include(result,exUnderflow);
    if (mask and (1 shl ord(exPrecision)) <> 0) then
      include(result,exPrecision);
end;

{$ifdef wince}

const
  _DN_SAVE  = $00000000;
  _DN_FLUSH = $01000000;

  _EM_INVALID    = $00000010;
  _EM_DENORMAL   = $00080000;
  _EM_ZERODIVIDE = $00000008;
  _EM_OVERFLOW   = $00000004;
  _EM_UNDERFLOW  = $00000002;
  _EM_INEXACT    = $00000001;

  _IC_AFFINE     = $00040000;
  _IC_PROJECTIVE = $00000000;

  _RC_CHOP       = $00000300;
  _RC_UP         = $00000200;
  _RC_DOWN       = $00000100;
  _RC_NEAR       = $00000000;

  _PC_24         = $00020000;
  _PC_53         = $00010000;
  _PC_64         = $00000000;

  _MCW_DN        = $03000000;
  _MCW_EM        = $0008001F;
  _MCW_IC        = $00040000;
  _MCW_RC        = $00000300;
  _MCW_PC        = $00030000;

function _controlfp(new: DWORD; mask: DWORD): DWORD; cdecl; external 'coredll';

function GetRoundMode: TFPURoundingMode;
var
  c: dword;
begin
  c:=_controlfp(0, 0);
  Result:=TFPURoundingMode((c shr 16) and 3);
end;

function SetRoundMode(const RoundMode: TFPURoundingMode): TFPURoundingMode;
var
  c: dword;
begin
  c:=Ord(RoundMode) shl 16;
  c:=_controlfp(c, _MCW_RC);
  Result:=TFPURoundingMode((c shr 16) and 3);
end;

function GetPrecisionMode: TFPUPrecisionMode;
var
  c: dword;
begin
  c:=_controlfp(0, 0);
  if c and _MCW_PC = _PC_64 then
    Result:=pmDouble
  else
    Result:=pmSingle;
end;

function SetPrecisionMode(const Precision: TFPUPrecisionMode): TFPUPrecisionMode;
var
  c: dword;
begin
  Result:=GetPrecisionMode;
  if Precision = pmSingle then
    c:=_PC_24
  else
    c:=_PC_64;
  _controlfp(c, _MCW_PC);
end;

function ConvertExceptionMask(em: dword): TFPUExceptionMask;
begin
  Result:=[];
  if em and _EM_INVALID <> 0 then
    Result:=Result + [exInvalidOp];
  if em and _EM_DENORMAL <> 0 then
    Result:=Result + [exDenormalized];
  if em and _EM_ZERODIVIDE <> 0 then
    Result:=Result + [exZeroDivide];
  if em and _EM_OVERFLOW <> 0 then
    Result:=Result + [exOverflow];
  if em and _EM_UNDERFLOW <> 0 then
    Result:=Result + [exUnderflow];
  if em and _EM_INEXACT <> 0 then
    Result:=Result + [exPrecision];
end;

function GetExceptionMask: TFPUExceptionMask;
begin
  Result:=ConvertExceptionMask(_controlfp(0, 0));
end;

function SetExceptionMask(const Mask: TFPUExceptionMask): TFPUExceptionMask;
var
  c: dword;
begin
  c:=0;
  if exInvalidOp in Mask then
    c:=c or _EM_INVALID;
  if exDenormalized in Mask then
    c:=c or _EM_DENORMAL;
  if exZeroDivide in Mask then
    c:=c or _EM_ZERODIVIDE;
  if exOverflow in Mask then
    c:=c or _EM_OVERFLOW;
  if exUnderflow in Mask then
    c:=c or _EM_UNDERFLOW;
  if exPrecision in Mask then
    c:=c or _EM_INEXACT;
  c:=_controlfp(c, _MCW_EM);
  Result:=ConvertExceptionMask(c);
  softfloat_exception_mask:=FPUExceptionMaskToSoftFloatMask(mask);
end;

procedure ClearExceptions(RaisePending: Boolean =true);
begin
end;

{$else wince}

{*****************************************************************************
                                   FPA code
 *****************************************************************************}
{
 Docs from uclib

 * We have a slight terminology confusion here.  On the ARM, the register
 * we're interested in is actually the FPU status word - the FPU control
 * word is something different (which is implementation-defined and only
 * accessible from supervisor mode.)
 *
 * The FPSR looks like this:
 *
 *     31-24        23-16          15-8              7-0
 * | system ID | trap enable | system control | exception flags |
 *
 * We ignore the system ID bits; for interest's sake they are:
 *
 *  0000	"old" FPE
 *  1000	FPPC hardware
 *  0001	FPE 400
 *  1001	FPA hardware
 *
 * The trap enable and exception flags are both structured like this:
 *
 *     7 - 5     4     3     2     1     0
 * | reserved | INX | UFL | OFL | DVZ | IVO |
 *
 * where a `1' bit in the enable byte means that the trap can occur, and
 * a `1' bit in the flags byte means the exception has occurred.
 *
 * The exceptions are:
 *
 *  IVO - invalid operation
 *  DVZ - divide by zero
 *  OFL - overflow
 *  UFL - underflow
 *  INX - inexact (do not use; implementations differ)
 *
 * The system control byte looks like this:
 *
 *     7-5      4    3    2    1    0
 * | reserved | AC | EP | SO | NE | ND |
 *
 * where the bits mean
 *
 *  ND - no denormalised numbers (force them all to zero)
 *  NE - enable NaN exceptions
 *  SO - synchronous operation
 *  EP - use expanded packed-decimal format
 *  AC - use alternate definition for C flag on compare operations
 */

/* masking of interrupts */
#define _FPU_MASK_IM	0x00010000	/* invalid operation */
#define _FPU_MASK_ZM	0x00020000	/* divide by zero */
#define _FPU_MASK_OM	0x00040000	/* overflow */
#define _FPU_MASK_UM	0x00080000	/* underflow */
#define _FPU_MASK_PM	0x00100000	/* inexact */
#define _FPU_MASK_DM	0x00000000	/* denormalized operation */

/* The system id bytes cannot be changed.
   Only the bottom 5 bits in the trap enable byte can be changed.
   Only the bottom 5 bits in the system control byte can be changed.
   Only the bottom 5 bits in the exception flags are used.
   The exception flags are set by the fpu, but can be zeroed by the user. */
#define _FPU_RESERVED	0xffe0e0e0	/* These bits are reserved.  */

/* The fdlibm code requires strict IEEE double precision arithmetic,
   no interrupts for exceptions, rounding to nearest.  Changing the
   rounding mode will break long double I/O.  Turn on the AC bit,
   the compiler generates code that assumes it is on.  */
#define _FPU_DEFAULT	0x00001000	/* Default value.  */
#define _FPU_IEEE	0x001f1000	/* Default + exceptions enabled. */
}


{$if not(defined(gba)) and not(defined(nds)) and not(defined(FPUSOFT)) and not(defined(FPULIBGCC))}
const
  _FPU_MASK_IM  =  $00010000;      { invalid operation      }
  _FPU_MASK_ZM  =  $00020000;      { divide by zero         }
  _FPU_MASK_OM  =  $00040000;      { overflow               }
  _FPU_MASK_UM  =  $00080000;      { underflow              }
  _FPU_MASK_PM  =  $00100000;      { inexact                }
  _FPU_MASK_DM  =  $00000000;      { denormalized operation }
  _FPU_MASK_ALL =  $001f0000;      { mask for all flags     }

function FPU_GetCW : dword; nostackframe; assembler;
  asm
    rfs r0
  end;


procedure FPU_SetCW(cw : dword); nostackframe; assembler;
  asm
    wfs r0
  end;
{$endif}


function GetRoundMode: TFPURoundingMode;
  begin
    { does not apply }
  end;


function SetRoundMode(const RoundMode: TFPURoundingMode): TFPURoundingMode;
  begin
    { does not apply }
  end;


function GetPrecisionMode: TFPUPrecisionMode;
  begin
    { does not apply }
  end;


function SetPrecisionMode(const Precision: TFPUPrecisionMode): TFPUPrecisionMode;
  begin
    { does not apply }
  end;


function GetExceptionMask: TFPUExceptionMask;
  var
    cw : dword;
  begin
{$if not(defined(gba)) and not(defined(nds)) and not(defined(FPUSOFT)) and not(defined(FPULIBGCC))}
    Result:=[];
    cw:=FPU_GetCW;

    if (cw and _FPU_MASK_IM)=0 then
      include(Result,exInvalidOp);

    if (cw and _FPU_MASK_DM)=0 then
      include(Result,exDenormalized);

    if (cw and _FPU_MASK_ZM)=0 then
      include(Result,exZeroDivide);

    if (cw and _FPU_MASK_OM)=0 then
      include(Result,exOverflow);

    if (cw and _FPU_MASK_UM)=0 then
      include(Result,exUnderflow);

    if (cw and _FPU_MASK_PM)=0 then
      include(Result,exPrecision);
{$else}
    Result:=SoftFloatMaskToFPUExceptionMask(softfloat_exception_mask);
{$endif}
  end;


function SetExceptionMask(const Mask: TFPUExceptionMask): TFPUExceptionMask;
  var
    cw : dword;
  begin
{$if not(defined(gba)) and not(defined(nds)) and not(defined(FPUSOFT)) and not(defined(FPULIBGCC))}
    cw:=FPU_GetCW or _FPU_MASK_ALL;

    if exInvalidOp in Mask then
      cw:=cw and not(_FPU_MASK_IM);

    if exDenormalized in Mask then
      cw:=cw and not(_FPU_MASK_DM);

    if exZeroDivide in Mask then
      cw:=cw and not(_FPU_MASK_ZM);

    if exOverflow in Mask then
      cw:=cw and not(_FPU_MASK_OM);

    if exUnderflow in Mask then
      cw:=cw and not(_FPU_MASK_UM);

    if exPrecision in Mask then
      cw:=cw and not(_FPU_MASK_PM);

    FPU_SetCW(cw);
{$endif}
    softfloat_exception_mask:=FPUExceptionMaskToSoftFloatMask(Mask);
  end;


procedure ClearExceptions(RaisePending: Boolean =true);
  begin
  end;

{$endif wince}
