{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Florian Klaempfl
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

function GetRoundMode: TFPURoundingMode;
  const
    bits2rm: array[0..3] of TFPURoundingMode = (rmNearest,rmTruncate,rmUp,rmDown);
  begin
    result:=TFPURoundingMode(bits2rm[(GetNativeFPUControlWord shr 30) and 3])
  end;


function SetRoundMode(const RoundMode: TFPURoundingMode): TFPURoundingMode;
  const
    rm2bits: array[TFPURoundingMode] of byte = (0,3,2,1);
  var
    cw: TNativeFPUControlWord;
  begin
    cw:=GetNativeFPUControlWord;
    softfloat_rounding_mode:=RoundMode;
    result:=TFPURoundingMode(cw shr 30);
    SetNativeFPUControlWord((cw and $3fffffff) or (rm2bits[RoundMode] shl 30));
  end;

function GetPrecisionMode: TFPUPrecisionMode;
  begin
    result:=pmDouble;
  end;


function SetPrecisionMode(const Precision: TFPUPrecisionMode): TFPUPrecisionMode;
  begin
    result:=pmDouble;
  end;


function FSR2ExceptionMask(fsr: TNativeFPUControlWord): TFPUExceptionMask;
  begin
    result:=[];
    { invalid operation: bit 27 }
    if (fsr and (1 shl 27))=0 then
      include(result,exInvalidOp);

    { zero divide: bit 24 }
    if (fsr and (1 shl 24))=0 then
      include(result,exZeroDivide);

    { overflow: bit 26 }
    if (fsr and (1 shl 26))=0 then
      include(result,exOverflow);

    { underflow: bit 25 }
    if (fsr and (1 shl 25))=0 then
      include(result,exUnderflow);

    { Precision (inexact result): bit 23 }
    if (fsr and (1 shl 23))=0 then
      include(result,exPrecision);
  end;


function GetExceptionMask: TFPUExceptionMask;
  begin
    result:=FSR2ExceptionMask(GetNativeFPUControlWord);
  end;


function SetExceptionMask(const Mask: TFPUExceptionMask): TFPUExceptionMask;
  var
    fsr : TNativeFPUControlWord;
  begin
    fsr:=GetNativeFPUControlWord;
    result:=FSR2ExceptionMask(fsr);

    { invalid operation: bit 27 }
    if (exInvalidOp in mask) then
      fsr:=fsr and not(1 shl 27)
    else
      fsr:=fsr or (1 shl 27);

    { zero divide: bit 24 }
    if (exZeroDivide in mask) then
      fsr:=fsr and not(1 shl 24)
    else
      fsr:=fsr or (1 shl 24);

    { overflow: bit 26 }
    if (exOverflow in mask) then
      fsr:=fsr and not(1 shl 26)
    else
      fsr:=fsr or (1 shl 26);

    { underflow: bit 25 }
    if (exUnderflow in mask) then
      fsr:=fsr and not(1 shl 25)
    else
      fsr:=fsr or (1 shl 25);

    { Precision (inexact result): bit 23 }
    if (exPrecision in mask) then
      fsr:=fsr and not(1 shl 23)
    else
      fsr:=fsr or (1 shl 23);

    { update control register contents }
    SetNativeFPUControlWord(fsr);
  end;


procedure ClearExceptions(RaisePending: Boolean =true);
  begin
    SetNativeFPUControlWord(GetNativeFPUControlWord and $fffffc1f);
  end;

