{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2006 by the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{$ifdef FPC_INCLUDE_SOFTWARE_LONGWORD_TO_DOUBLE}
function fpc_longword_to_double(i: longword): double; compilerproc;
begin
  qword(result):=(qword(1075) shl 52) + i;
  result:=result - (qword(1) shl 52);
end;
{$endif FPC_INCLUDE_SOFTWARE_LONGWORD_TO_DOUBLE}

{$if defined(FPU68881) or defined(FPUCOLDFIRE)}
{$ifndef FPC_SYSTEM_HAS_ROUND}
{$define FPC_SYSTEM_HAS_ROUND}
    function fpc_round_real(d : ValReal) : int64;compilerproc;
    type
      float64 = record
        high,low: longint;
      end;
    var
      tmp: double;
      j0: longint;
      hx: longword;
      sx: longint;
    const
      H2_52: array[0..1] of double = (
        4.50359962737049600000e+15,
       -4.50359962737049600000e+15
      );
    Begin
      { This basically calculates trunc((d+2**52)-2**52) }
      hx:=float64(d).high;
      j0:=((hx shr 20) and $7ff) - $3ff;
      sx:=hx shr 31;
      hx:=(hx and $fffff) or $100000;

      if j0>=52 then         { No fraction bits, already integer }
        begin
          if j0>=63 then     { Overflow, let trunc() raise an exception }
            exit(trunc(d))   { and/or return +/-MaxInt64 if it's masked }
          else
            result:=((int64(hx) shl 32) or float64(d).low) shl (j0-52);
        end
      else
        begin
          { Rounding happens here. It is important that the expression is not
            optimized by selecting a larger type to store 'tmp'. }

          { The double cast should enforce a memory store and reload, which is the
            fastest way on a 68881/2 to enforce the rounding to double precision.
            The internal operation of the '88x is always in extended. If the rounding
            of the FPU is set to a different precision in the FPCR, the result is a
            a large performance penalty, according to the 68881/68882 Users Manual
            Section 2.2.2. So we keep the FPU in extended, but this means the rounding
            to double trick might conflict with tmp being a regvar. (KB) }
{$ifdef FPU68881}
          tmp:=double(float64(H2_52[sx]+d));
{$else}
          { The above doesn't affect the CF FPU. Its maximum precision is double. }
          tmp:=H2_52[sx]+d;
{$endif}
          d:=tmp-H2_52[sx];
          hx:=float64(d).high;
          j0:=((hx shr 20) and $7ff)-$3ff;
          hx:=(hx and $fffff) or $100000;
          if j0<=20 then
            begin
              if j0<0 then
                exit(0)
              else           { more than 32 fraction bits, low dword discarded }
                result:=hx shr (20-j0);
            end
          else
            result:=(int64(hx) shl (j0-20)) or (float64(d).low shr (52-j0));
        end;
      if sx<>0 then
        result:=-result;
    end;
{$endif FPC_SYSTEM_HAS_ROUND}
{$endif defined(FPU68881) or defined(FPUCOLDFIRE)}
