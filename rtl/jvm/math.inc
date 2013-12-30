{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2011 by Jonas Maebe,
    members of the Free Pascal development team.

    This file implements Java math routines

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$define FPC_SYSTEM_HAS_ARCTAN}
function fpc_arctan_real(d : ValReal) : ValReal;compilerproc;{$ifdef MATHINLINE}inline;{$endif}
  begin
    result:=JLMath.atan(d);
  end;


{$define FPC_SYSTEM_HAS_COS}
function fpc_cos_real(d : ValReal) : ValReal;compilerproc;{$ifdef MATHINLINE}inline;{$endif}
  begin
    result:=JLMath.cos(d);
  end;


{$define FPC_SYSTEM_HAS_EXP}
function fpc_exp_real(d : ValReal) : ValReal;compilerproc;{$ifdef MATHINLINE}inline;{$endif}
  begin
    result:=JLMath.exp(d);
  end;


{$define FPC_SYSTEM_HAS_INT}
function fpc_int_real(d : ValReal) : ValReal;compilerproc;{$ifdef MATHINLINE}inline;{$endif}
  begin
    if d>=0 then
      result:=JLMath.floor(d)
    else
      result:=JLMath.ceil(d);
  end;


{$define FPC_SYSTEM_HAS_LN}
function fpc_ln_real(d : ValReal) : ValReal;compilerproc;{$ifdef MATHINLINE}inline;{$endif}
  begin
    result:=JLMath.log(d);
  end;


{$define FPC_SYSTEM_HAS_SIN}
function fpc_sin_real(d : ValReal) : ValReal;compilerproc;{$ifdef MATHINLINE}inline;{$endif}
  begin
    result:=JLMath.sin(d);
  end;

{$define FPC_SYSTEM_HAS_SQR}
{ handled in the code generator }
function fpc_sqr_real(d : ValReal) : ValReal;compilerproc;{$ifdef MATHINLINE}inline;{$endif}
  begin
    HandleError(219);
  end;


{$define FPC_SYSTEM_HAS_SQRT}
function fpc_sqrt_real(d : ValReal) : ValReal;compilerproc;
  begin
    result:=JLMath.sqrt(d);
  end;

{$define FPC_SYSTEM_HAS_TRUNC}
{ handled in the code generator }
function fpc_trunc_real(d : ValReal) : int64;compilerproc;
  begin
    HandleError(219);
  end;


{$define FPC_SYSTEM_HAS_REAL2DOUBLE}
function real2double(r : real48) : double;
  var
     res : jlong;
     exponent : word;
  begin
     { check for zero }
     if r[0]=0 then
       begin
         real2double:=0.0;
         exit;
       end;

     { copy mantissa }
     res:=(r[1] shl 5) shl 8;
     res:=res or (((r[1] shr 3) or (r[2] shl 5)) shl 16);
     res:=res or (((r[2] shr 3) or (r[3] shl 5)) shl 24);
     res:=res or (((r[3] shr 3) or (r[4] shl 5)) shl 32);
     res:=res or (((r[4] shr 3) or (r[5] and $7f) shl 5) shl 40);
     res:=res or (((r[5] and $7f) shr 3) shl 48);

     { copy exponent }
     { correct exponent: }
     exponent:=(word(r[0])+(1023-129));
     res:=res or (((exponent and $f) shl 4) shl 48);
     res:=res or ((exponent shr 4) shl 56);

     { set sign }
     res:=res or (r[5] and $80) shl 56;
     real2double:=JLDouble.longBitsToDouble(res);
  end;


  {$define FPC_HAS_FLOAT64HIGH}
  function float64high(d: double): longint;
    begin
      result:=JLDouble.doubleToRawLongBits(d) shr 32;
    end;


  procedure float64sethigh(var d: double; l: longint);
    begin
      d:=JLDouble.longBitsToDouble((JLDouble.doubleToRawLongBits(d) and $ffffffff) or (jlong(l) shl 32));
    end;


  {$define FPC_HAS_FLOAT64LOW}
  function float64low(d: double): longint;
  begin
    result:=longint(JLDouble.doubleToRawLongBits(d));
  end;


  procedure float64setlow(var d: double; l: longint);
    begin
      d:=JLDouble.longBitsToDouble((JLDouble.doubleToRawLongBits(d) and $ffffffff00000000) or cardinal(l));
    end;
