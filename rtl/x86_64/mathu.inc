{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2004 by Florian Klaempfl
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

**********************************************************************}

{$ASMMODE GAS}

{$ifdef FPC_HAS_TYPE_EXTENDED}
{$define FPC_MATH_HAS_ARCTAN2}
function arctan2(y,x : float) : float;assembler;
  asm
     fldt y
     fldt x
     fpatan
     fwait
  end;

{$define FPC_MATH_HAS_TAN}
function tan(x : float) : float;assembler;
  asm
    fldt X
    fptan
    fstp %st
    fwait
  end;


{$define FPC_MATH_HAS_COTAN}
function cotan(x : float) : float;assembler;
  asm
    fldt X
    fptan
    fdivp %st,%st(1)
    fwait
  end;
{$endif FPC_HAS_TYPE_EXTENDED}


{$define FPC_MATH_HAS_SINCOS}
{$ifdef FPC_HAS_TYPE_EXTENDED}
procedure sincos(theta : extended;out sinus,cosinus : extended);assembler;
asm
    fldt theta
    fsincos
{$ifdef WIN64}
    fstpl (%r8)
    fstpl (%rdx)
{$else WIN64}
    fstpt (%rsi)
    fstpt (%rdi)
{$endif WIN64}
    fwait
  end;
{$endif FPC_HAS_TYPE_EXTENDED}


{$asmmode intel}
procedure sincos(theta : double;out sinus,cosinus : double);assembler;
  var
    t : double;
  asm
    movsd qword ptr t,xmm0
    fld qword ptr t
    fsincos
    fstp qword ptr [cosinus]
    fstp qword ptr [sinus]
    fwait
  end;


procedure sincos(theta : single;out sinus,cosinus : single);assembler;
  var
    t : single;
  asm
    movss dword ptr t,xmm0
    fld dword ptr t
    fsincos
    fstp dword ptr [cosinus]
    fstp dword ptr [sinus]
    fwait
  end;


{$asmmode gas}
function GetRoundMode: TFPURoundingMode;
begin
{$ifndef FPC_HAS_TYPE_EXTENDED}
  Result:=TFPURoundingMode((GetSSECSR shr 13) and $3);
{$else win64}
  Result:=TFPURoundingMode((Get8087CW shr 10) and $3);
{$endif win64}
end;

function SetRoundMode(const RoundMode: TFPURoundingMode): TFPURoundingMode;
var
  CtlWord: Word;
begin
  CtlWord:=Get8087CW;
  Set8087CW((CtlWord and $F3FF) or (Ord(RoundMode) shl 10));
  SetSSECSR((GetSSECSR and $ffff9fff) or (dword(RoundMode) shl 13));
  Result:=GetRoundMode;
end;

function GetPrecisionMode: TFPUPrecisionMode;
begin
  Result:=TFPUPrecisionMode((Get8087CW shr 8) and 3);
end;

function SetPrecisionMode(const Precision: TFPUPrecisionMode): TFPUPrecisionMode;
var
  CtlWord: Word;
begin
  CtlWord:=Get8087CW;
  Set8087CW((CtlWord and $FCFF) or (Ord(Precision) shl 8));
  Result:=TFPUPrecisionMode((CtlWord shr 8) and 3);
end;

function GetExceptionMask: TFPUExceptionMask;
begin
{$ifndef FPC_HAS_TYPE_EXTENDED}
  Result:=TFPUExceptionMask(dword((GetSSECSR shr 7) and $3f));
{$else win64}
  Result:=TFPUExceptionMask(dword(Get8087CW and $3F));
{$endif win64}
end;

function SetExceptionMask(const Mask: TFPUExceptionMask): TFPUExceptionMask;
var
  CtlWord: Word;
begin
  CtlWord:=Get8087CW;
  Set8087CW((CtlWord and $FFC0) or Byte(Longint(Mask)));
  SetSSECSR((GetSSECSR and $ffffe07f) or (dword(Mask) shl 7));
  softfloat_exception_mask:=dword(Mask);
  Result:=GetExceptionMask;
end;


procedure ClearExceptions(RaisePending: Boolean);assembler;
asm
  cmpb $0,RaisePending
  je .Lclear
  fwait
.Lclear:
  fnclex
end;
