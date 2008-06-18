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
  ExceptionsPendingMask = %11111111111111100000011100000000;

  ExceptionMask        = InvalidOperationMask or OverflowMask or UnderflowMask or ZeroDivideMask or InexactMask;

  AllConfigBits        = ExceptionMask or NonIEEEModeMask or RoundModeMask;

function getFPSCR : DWord; assembler; nostackframe;
asm
  mffs f0
  stfd f0, -12(r1)
  lwz r3, -8(r1)
end;

procedure setFPSCR(newFPSCR : DWord); assembler; nostackframe;
asm
  stw r3, -8(r1)
  lfd f0, -12(r1)
  mtfsf 255, f0
end;

function GetRoundMode: TFPURoundingMode;
begin
  case (getFPSCR and RoundModeMask) of
    0 : result := rmNearest;
    1 : result := rmTruncate;
    2 : result := rmUp;
    3 : result := rmDown;
  end;
end;

function SetRoundMode(const RoundMode: TFPURoundingMode): TFPURoundingMode;
var
  mode : DWord;
begin
  case (RoundMode) of
    rmNearest : mode := 0;
    rmTruncate : mode := 1;
    rmUp : mode := 2;
    rmDown : mode := 3;
  end;
  setFPSCR((getFPSCR and (not RoundModeMask)) or mode);
  result := RoundMode;
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
begin
  result := [];
  if ((getFPSCR and InvalidOperationMask) = 0) then 
    result := result + [exInvalidOp];
  if ((getFPSCR and OverflowMask) = 0) then 
    result := result + [exOverflow];
  if ((getFPSCR and UnderflowMask) = 0) then 
    result := result + [exUnderflow];
  if ((getFPSCR and ZeroDivideMask) = 0) then 
    result := result + [exZeroDivide];
  if ((getFPSCR and InexactMask) = 0) then 
    result := result + [exPrecision];
end;

function SetExceptionMask(const Mask: TFPUExceptionMask): TFPUExceptionMask;
var
  mode : DWord;
begin
  mode := 0;
  if (exInvalidOp in Mask) then
    mode := mode or InvalidOperationMask;
  if (exOverflow in Mask) then
    mode := mode or OverflowMask;
  if (exUnderflow in Mask) then
    mode := mode or UnderflowMask;
  if (exZeroDivide in Mask) then
    mode := mode or ZeroDivideMask;
  if (exPrecision in Mask) then
    mode := mode or InexactMask;
  
  setFPSCR((getFPSCR or ExceptionMask) and not mode and not ExceptionsPendingMask);
  result := Mask - [exDenormalized];
end;


procedure ClearExceptions(RaisePending: Boolean = true);
begin
  { RaisePending has no effect on PPC, always raises them at the correct location }
  setFPSCR(getFPSCR and (not ExceptionsPendingMask));
end;

