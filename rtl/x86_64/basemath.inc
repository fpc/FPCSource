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

{$asmmode gas}
function GetRoundMode: TFPURoundingMode;
begin
{$ifndef FPC_HAS_TYPE_EXTENDED}
  Result:=TFPURoundingMode((GetMXCSR shr 13) and $3);
{$else win64}
  Result:=TFPURoundingMode((Get8087CW shr 10) and $3);
{$endif win64}
end;

function SetRoundMode(const RoundMode: TFPURoundingMode): TFPURoundingMode;
var
  CtlWord: Word;
  SSECSR: dword;
begin
  CtlWord:=Get8087CW;
  SSECSR:=GetMXCSR;
  softfloat_rounding_mode:=RoundMode;
  Set8087CW((CtlWord and $F3FF) or (Ord(RoundMode) shl 10));
  SetMXCSR((SSECSR and $ffff9fff) or (dword(RoundMode) shl 13));
{$ifdef FPC_HAS_TYPE_EXTENDED}
  Result:=TFPURoundingMode((CtlWord shr 10) and 3);
{$else}
  Result:=TFPURoundingMode((SSECSR shr 13) and 3);
{$endif FPC_HAS_TYPE_EXTENDED}
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
  Result:=TFPUExceptionMask(dword((GetMXCSR shr 7) and $3f));
{$else win64}
  Result:=TFPUExceptionMask(dword(Get8087CW and $3F));
{$endif win64}
end;

function SetExceptionMask(const Mask: TFPUExceptionMask): TFPUExceptionMask;
var
  CtlWord: Word;
  SSECSR: dword;
begin
  CtlWord:=Get8087CW;
  SSECSR:=GetMXCSR;
  Set8087CW((CtlWord and $FFC0) or Byte(Longint(Mask)));
  SetMXCSR((SSECSR and $ffffe07f) or (dword(Mask) shl 7));
{$ifdef FPC_HAS_TYPE_EXTENDED}
  Result:=TFPUExceptionMask(dword(CtlWord and $3F));
{$else}
  Result:=TFPUExceptionMask((SSECSR shr 7) and $3F);
{$endif FPC_HAS_TYPE_EXTENDED}
end;


procedure ClearExceptions(RaisePending: Boolean);assembler;
asm
  cmpb $0,RaisePending
  je .Lclear
  fwait
.Lclear:
  fnclex
end;
