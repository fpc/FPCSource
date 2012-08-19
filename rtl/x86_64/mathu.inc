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
{$endif FPC_HAS_TYPE_EXTENDED}


{$define FPC_MATH_HAS_SINCOS}
{$ifdef FPC_HAS_TYPE_EXTENDED}
procedure sincos(theta : extended;out sinus,cosinus : extended);assembler;
asm
    fldt theta
    fsincos
{$ifdef WIN64}
    fstpt (%r8)
    fstpt (%rdx)
{$else WIN64}
    fstpt (%rsi)
    fstpt (%rdi)
{$endif WIN64}
    fwait
  end;
{$endif FPC_HAS_TYPE_EXTENDED}


procedure sincos(theta : double;out sinus,cosinus : double);assembler;
  var
    t : double;
  asm
    movd %xmm0,t
    fldl t
    fsincos
{$ifdef WIN64}
    fstpl (%r8)
    fstpl (%rdx)
{$else WIN64}
    fstpl (%rsi)
    fstpl (%rdi)
{$endif WIN64}
    fwait
  end;


procedure sincos(theta : single;out sinus,cosinus : single);assembler;
  var
    t : single;
  asm
    movss %xmm0,t
    flds t
    fsincos
{$ifdef WIN64}
    fstps (%r8)
    fstps (%rdx)
{$else WIN64}
    fstps (%rsi)
    fstps (%rdi)
{$endif WIN64}
    fwait
  end;


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
  Result:=TFPUExceptionMask((GetSSECSR shr 7) and $3f);
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
