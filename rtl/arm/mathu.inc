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
  if c and _PC_64 <> 0 then
    Result:=pmDouble
  else
    Result:=pmSingle;
end;

function SetPrecisionMode(const Precision: TFPUPrecisionMode): TFPUPrecisionMode;
var
  c: dword;
begin
  if Precision = pmSingle then
    c:=_PC_53
  else
    c:=_PC_64;
  c:=_controlfp(c, _MCW_PC);
  if c and _PC_64 <> 0 then
    Result:=pmDouble
  else
    Result:=pmSingle;
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
end;

procedure ClearExceptions(RaisePending: Boolean =true);
begin
end;

{$endif wince}
