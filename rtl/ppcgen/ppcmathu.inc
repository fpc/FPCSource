{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2005 by Thomas Schatzl
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

const
  RoundModeMask        = %00000011;
  NonIEEEModeMask      = %00000100;

  InvalidOperationMask = %10000000;
  OverflowMask         = %01000000;
  UnderflowMask        = %00100000;
  ZeroDivideMask       = %00010000;
  InexactMask          = %00001000;
  AllExceptionsMask    = %11111000;
  ExceptionsPendingMask = %11111111111111100000011100000000;

  ExceptionMask        = InvalidOperationMask or OverflowMask or UnderflowMask or ZeroDivideMask or InexactMask;

  AllConfigBits        = ExceptionMask or NonIEEEModeMask or RoundModeMask;

{$ifdef aix}
const
  FP_RND_RZ = 0;
  FP_RND_RN = 1;
  FP_RND_RP = 2;
  FP_RND_RM = 3;

function feclearexcept(Mask: DWord):DWord;cdecl;external;

{$else aix}
const
  FP_RND_RZ = 1;
  FP_RND_RN = 0;
  FP_RND_RP = 2;
  FP_RND_RM = 3;
{$endif aix}

function GetRoundMode: TFPURoundingMode;
begin
{$ifndef aix}
  case GetNativeFPUControlWord and RoundModeMask of
{$else not aix}
  case GetNativeFPUControlWord.rndmode of
{$endif not aix}
    FP_RND_RN : result := rmNearest;
    FP_RND_RZ : result := rmTruncate;
    FP_RND_RP : result := rmUp;
    FP_RND_RM : result := rmDown;
  end;
end;

function SetRoundMode(const RoundMode: TFPURoundingMode): TFPURoundingMode;
var
  mode : DWord;
  currentcw: TNativeFPUControlWord;
begin
  softfloat_rounding_mode:=RoundMode;
  case (RoundMode) of
    rmNearest :
      begin
        mode := FP_RND_RN;
      end;
    rmTruncate :
      begin
        mode := FP_RND_RZ;
      end;
    rmUp :
      begin
        mode := FP_RND_RP;
      end;
    rmDown :
      begin
        mode := FP_RND_RM;
      end;
  end;
  result := GetRoundMode;
  currentcw:=GetNativeFPUControlWord;
{$ifndef aix}
  SetNativeFPUControlWord((currentcw and (not RoundModeMask)) or mode);
{$else not aix}
  currentcw.rndmode:=mode;
  SetNativeFPUControlWord(currentcw);
{$endif not aix}
end;


function GetPrecisionMode: TFPUPrecisionMode;
begin
  result := pmDouble;
end;

function SetPrecisionMode(const Precision: TFPUPrecisionMode): TFPUPrecisionMode;
begin
  { nothing to do, not supported }
  result := pmDouble;
end;


function GetExceptionMask: TFPUExceptionMask;
var
  currentExceptionMask: cardinal;
begin
  result := [];
{$ifndef aix}
  currentExceptionMask:=GetNativeFPUControlWord;
{$else}
  currentExceptionMask:=GetNativeFPUControlWord.exceptionmask;
{$endif}
  if ((currentExceptionMask and InvalidOperationMask) = 0) then
    result := result + [exInvalidOp];
  if ((currentExceptionMask and OverflowMask) = 0) then
    result := result + [exOverflow];
  if ((currentExceptionMask and UnderflowMask) = 0) then
    result := result + [exUnderflow];
  if ((currentExceptionMask and ZeroDivideMask) = 0) then
    result := result + [exZeroDivide];
  if ((currentExceptionMask and InexactMask) = 0) then
    result := result + [exPrecision];
end;

function SetExceptionMask(const Mask: TFPUExceptionMask): TFPUExceptionMask;
var
  mode : DWord;
  currentcw: TNativeFPUControlWord;
begin
  mode := 0;
  softfloat_exception_mask := mask;
  if (exInvalidOp in Mask) then
    begin
      mode := mode or InvalidOperationMask;
    end;
  if (exOverflow in Mask) then
    begin
      mode := mode or OverflowMask;
    end;
  if (exUnderflow in Mask) then
    begin
      mode := mode or UnderflowMask;
    end;
  if (exZeroDivide in Mask) then
    begin
      mode := mode or ZeroDivideMask;
    end;
  if (exPrecision in Mask) then
    begin
      mode := mode or InexactMask;
    end;

  softfloat_exception_flags := [];
  currentcw:=GetNativeFPUControlWord;
{$ifdef aix}
  currentcw.exceptionmask:=mode;
{$else}
  currentcw:=(currentcw or ExceptionMask) and not mode and not ExceptionsPendingMask;
{$endif}
  SetNativeFPUControlWord(currentcw);
  { also clear out pending exceptions on AIX }
{$ifdef aix}
  { clear pending exceptions }
  feclearexcept(AllExceptionsMask);
{$endif}
  result := Mask - [exDenormalized];
end;


procedure ClearExceptions(RaisePending: Boolean = true);
begin
{$ifdef aix}
  { clear pending exceptions }
  feclearexcept(AllExceptionsMask);
{$else}
  { RaisePending has no effect on PPC, always raises them at the correct location }
  SetNativeFPUControlWord(GetNativeFPUControlWord and (not ExceptionsPendingMask));
{$endif}
  softfloat_exception_flags := [];
end;
