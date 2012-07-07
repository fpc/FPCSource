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

{ exported by the system unit }
function get_fsr : dword;external name 'FPC_GETFSR';
procedure set_fsr(fsr : dword);external name 'FPC_SETFSR';

const
  { FPU enable exception bits for FCSR register }
  fpu_enable_inexact   =  $80;
  fpu_enable_underflow = $100;
  fpu_enable_overflow  = $200;
  fpu_enable_div_zero  = $400;
  fpu_enable_invalid   = $800;
  fpu_enable_mask      = $F80;
  default_fpu_enable = fpu_enable_div_zero or fpu_enable_invalid;

  fpu_flags_mask = $7C;
  { FPU rounding mask and values }
  fpu_rounding_mask    = $3;
  fpu_rounding_nearest = 0;
  fpu_rounding_towards_zero = 1;
  fpu_rounding_plus_inf = 2;
  fpu_rounding_minus_inf = 3;


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

function GetRoundMode: TFPURoundingMode;
  begin
    result:=TFPURoundingMode(get_fsr and 3);
  end;

function SetRoundMode(const RoundMode: TFPURoundingMode): TFPURoundingMode;
  var
    fpu_round : longint;
  begin
    
    case (RoundMode) of
      rmNearest :
        begin
		  softfloat_rounding_mode := float_round_nearest_even;
		  fpu_round:=fpu_rounding_nearest;
		end;
      rmTruncate :
        begin
	      softfloat_rounding_mode := float_round_to_zero;
   		  fpu_round:=fpu_rounding_towards_zero;
		end;
   rmUp :
        begin
	      softfloat_rounding_mode := float_round_up;
   		  fpu_round:=fpu_rounding_plus_inf;
		end;
   rmDown :
        begin
	      softfloat_rounding_mode := float_round_down;
   		  fpu_round:=fpu_rounding_minus_inf;
		end;
 end;
    set_fsr((get_fsr and not fpu_rounding_mask) or fpu_round);
//!!!    result:=TFPURoundingMode(get_fsr shr 30);
  end;


function GetPrecisionMode: TFPUPrecisionMode;
  begin
    result:=pmDouble;
  end;


function SetPrecisionMode(const Precision: TFPUPrecisionMode): TFPUPrecisionMode;
  begin
    result:=pmDouble;
  end;


function GetExceptionMask: TFPUExceptionMask;
  var
    fsr : dword;
  begin
    fsr:=get_fsr;
    result:=[];
    { invalid operation }
    if (fsr and fpu_enable_invalid)=0 then
      include(result,exInvalidOp);

    { zero divide }
    if (fsr and fpu_enable_div_zero)=0 then
      include(result,exZeroDivide);

    { overflow }
    if (fsr and fpu_enable_overflow)=0 then
      include(result,exOverflow);

    { underflow: }
    if (fsr and fpu_enable_underflow)=0 then
      include(result,exUnderflow);

    { Precision (inexact result) }
    if (fsr and fpu_enable_inexact)=0 then
      include(result,exPrecision);
  end;



function SetExceptionMask(const Mask: TFPUExceptionMask): TFPUExceptionMask;
  var
    fsr : dword;
  begin
    fsr:=get_fsr;

    { invalid operation }
    if (exInvalidOp in mask) then
      fsr:=fsr and not(fpu_enable_invalid)
    else
      fsr:=fsr or (fpu_enable_invalid);

    { zero divide }
    if (exZeroDivide in mask) then
      fsr:=fsr and not(fpu_enable_div_zero)
    else
      fsr:=fsr or (fpu_enable_div_zero);

    { overflow }
    if (exOverflow in mask) then
      fsr:=fsr and not(fpu_enable_overflow)
    else
      fsr:=fsr or (fpu_enable_overflow);

    { underflow }
    if (exUnderflow in mask) then
      fsr:=fsr and not(fpu_enable_underflow)
    else
      fsr:=fsr or (fpu_enable_underflow);

    { Precision (inexact result) }
    if (exPrecision in mask) then
      fsr:=fsr and not(fpu_enable_inexact)
    else
      fsr:=fsr or (fpu_enable_inexact);

    { update control register contents }
    set_fsr(fsr);

    softfloat_exception_mask:=FPUExceptionMaskToSoftFloatMask(mask);
  end;


procedure ClearExceptions(RaisePending: Boolean =true);
  begin
    set_fsr(get_fsr and $fffffc1f);
  end;

