{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    This file contains some helper routines for int64 and qword

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$Q- no overflow checking }
{$R- no range checking }

    type
{$ifdef ENDIAN_LITTLE}
       tqwordrec = packed record
         low : dword;
         high : dword;
       end;
{$endif ENDIAN_LITTLE}
{$ifdef ENDIAN_BIG}
       tqwordrec = packed record
         high : dword;
         low : dword;
       end;
{$endif ENDIAN_BIG}


{$ifdef  FPC_INCLUDE_SOFTWARE_SHIFT_INT64}

{$ifndef FPC_SYSTEM_HAS_SHL_QWORD}
    function fpc_shl_qword(value,shift : qword) : qword; [public,alias: 'FPC_SHL_QWORD']; compilerproc;
      begin
        shift:=shift and 63;
        if shift=0 then
          result:=value
        else if shift>31 then
          begin
            tqwordrec(result).low:=0;
            tqwordrec(result).high:=tqwordrec(value).low shl (shift-32);
          end
        else
          begin
            tqwordrec(result).low:=tqwordrec(value).low shl shift;
            tqwordrec(result).high:=(tqwordrec(value).high shl shift) or (tqwordrec(value).low shr (32-shift));
          end;
      end;
{$endif FPC_SYSTEM_HAS_SHL_QWORD}


{$ifndef FPC_SYSTEM_HAS_SHR_QWORD}
   function fpc_shr_qword(value,shift : qword) : qword; [public,alias: 'FPC_SHR_QWORD']; compilerproc;
      begin
        shift:=shift and 63;
        if shift=0 then
          result:=value
        else if shift>31 then
          begin
            tqwordrec(result).high:=0;
            tqwordrec(result).low:=tqwordrec(value).high shr (shift-32);
          end
        else
          begin
            tqwordrec(result).high:=tqwordrec(value).high shr shift;
            tqwordrec(result).low:=(tqwordrec(value).low shr shift) or (tqwordrec(value).high shl (32-shift));
          end;
      end;
{$endif FPC_SYSTEM_HAS_SHR_QWORD}


{$ifndef FPC_SYSTEM_HAS_SHL_INT64}
    function fpc_shl_int64(value,shift : int64) : int64; [public,alias: 'FPC_SHL_INT64']; compilerproc;
      begin
        shift:=shift and 63;
        if shift=0 then
          result:=value
        else if shift>31 then
          begin
            tqwordrec(result).low:=0;
            tqwordrec(result).high:=tqwordrec(value).low shl (shift-32);
          end
        else
          begin
            tqwordrec(result).low:=tqwordrec(value).low shl shift;
            tqwordrec(result).high:=(tqwordrec(value).high shl shift) or (tqwordrec(value).low shr (32-shift));
          end;
      end;
{$endif FPC_SYSTEM_HAS_SHL_INT64}


{$ifndef FPC_SYSTEM_HAS_SHR_INT64}
    function fpc_shr_int64(value,shift : int64) : int64; [public,alias: 'FPC_SHR_INT64']; compilerproc;
      begin
        shift:=shift and 63;
        if shift=0 then
          result:=value
        else if shift>31 then
          begin
            tqwordrec(result).high:=0;
            tqwordrec(result).low:=tqwordrec(value).high shr (shift-32);
          end
        else
          begin
            tqwordrec(result).high:=tqwordrec(value).high shr shift;
            tqwordrec(result).low:=(tqwordrec(value).low shr shift) or (tqwordrec(value).high shl (32-shift));
          end;
      end;
{$endif FPC_SYSTEM_HAS_SHR_INT64}


{$endif FPC_INCLUDE_SOFTWARE_SHIFT_INT64}


    function count_leading_zeros(q : qword) : longint;

      var
         r,i : longint;

      begin
         r:=0;
         for i:=0 to 31 do
           begin
              if (tqwordrec(q).high and (dword($80000000) shr i))<>0 then
                begin
                   count_leading_zeros:=r;
                   exit;
                end;
              inc(r);
           end;
         for i:=0 to 31 do
           begin
              if (tqwordrec(q).low and (dword($80000000) shr i))<>0 then
                begin
                   count_leading_zeros:=r;
                   exit;
                end;
              inc(r);
           end;
         count_leading_zeros:=r;
      end;


{$ifndef FPC_SYSTEM_HAS_DIV_QWORD}
    function fpc_div_qword(n,z : qword) : qword;[public,alias: 'FPC_DIV_QWORD']; compilerproc;

      var
         shift,lzz,lzn : longint;

      begin
         fpc_div_qword:=0;
         if n=0 then
           HandleErrorFrame(200,get_frame);
         lzz:=count_leading_zeros(z);
         lzn:=count_leading_zeros(n);
         { if the denominator contains less zeros }
         { then the numerator                     }
         { the d is greater than the n            }
         if lzn<lzz then
           exit;
         shift:=lzn-lzz;
         n:=n shl shift;
         repeat
           if z>=n then
             begin
                z:=z-n;
                fpc_div_qword:=fpc_div_qword+(qword(1) shl shift);
             end;
           dec(shift);
           n:=n shr 1;
         until shift<0;
      end;
{$endif FPC_SYSTEM_HAS_DIV_QWORD}


{$ifndef FPC_SYSTEM_HAS_MOD_QWORD}
    function fpc_mod_qword(n,z : qword) : qword;[public,alias: 'FPC_MOD_QWORD']; compilerproc;

      var
         shift,lzz,lzn : longint;

      begin
         fpc_mod_qword:=0;
         if n=0 then
           HandleErrorFrame(200,get_frame);
         lzz:=count_leading_zeros(z);
         lzn:=count_leading_zeros(n);
         { if the denominator contains less zeros }
         { then the numerator                     }
         { the d is greater than the n            }
         if lzn<lzz then
           begin
              fpc_mod_qword:=z;
              exit;
           end;
         shift:=lzn-lzz;
         n:=n shl shift;
         repeat
           if z>=n then
             z:=z-n;
           dec(shift);
           n:=n shr 1;
         until shift<0;
         fpc_mod_qword:=z;
      end;
{$endif FPC_SYSTEM_HAS_MOD_QWORD}


{$ifndef FPC_SYSTEM_HAS_DIV_INT64}
    function fpc_div_int64(n,z : int64) : int64;[public,alias: 'FPC_DIV_INT64']; compilerproc;

      var
         sign : boolean;
         q1,q2 : qword;

      begin
         if n=0 then
           HandleErrorFrame(200,get_frame);
         { can the fpu do the work? }
           begin
              sign:=false;
              if z<0 then
                begin
                   sign:=not(sign);
                   q1:=qword(-z);
                end
              else
                q1:=z;
              if n<0 then
                begin
                   sign:=not(sign);
                   q2:=qword(-n);
                end
              else
                q2:=n;

              { the div is coded by the compiler as call to divqword }
              if sign then
                fpc_div_int64:=-(q1 div q2)
              else
                fpc_div_int64:=q1 div q2;
           end;
      end;
{$endif FPC_SYSTEM_HAS_DIV_INT64}


{$ifndef FPC_SYSTEM_HAS_MOD_INT64}
    function fpc_mod_int64(n,z : int64) : int64;[public,alias: 'FPC_MOD_INT64']; compilerproc;

      var
         signed : boolean;
         r,nq,zq : qword;

      begin
         if n=0 then
           HandleErrorFrame(200,get_frame);
         if n<0 then
           nq:=-n
         else
           nq:=n;
         if z<0 then
           begin
             signed:=true;
             zq:=qword(-z)
           end
         else
           begin
             signed:=false;
             zq:=z;
           end;
         r:=zq mod nq;
         if signed then
           fpc_mod_int64:=-int64(r)
         else
           fpc_mod_int64:=r;
      end;
{$endif FPC_SYSTEM_HAS_MOD_INT64}


{$ifndef FPC_SYSTEM_HAS_MUL_QWORD}
    { multiplies two qwords
      the longbool for checkoverflow avoids a misaligned stack
    }
    function fpc_mul_qword(f1,f2 : qword;checkoverflow : longbool) : qword;[public,alias: 'FPC_MUL_QWORD']; compilerproc;

      var
         _f1,bitpos : qword;
         l : longint;
         f1overflowed : boolean;
      begin
        fpc_mul_qword:=0;
        bitpos:=1;
        f1overflowed:=false;

        for l:=0 to 63 do
          begin
            if (f2 and bitpos)<>0 then
              begin
                _f1:=fpc_mul_qword;
                fpc_mul_qword:=fpc_mul_qword+f1;

                { if one of the operands is greater than the result an
                  overflow occurs                                      }
                if checkoverflow and (f1overflowed or ((_f1<>0) and (f1<>0) and
                  ((_f1>fpc_mul_qword) or (f1>fpc_mul_qword)))) then
                  HandleErrorFrame(215,get_frame);
              end;
            { when bootstrapping, we forget about overflow checking for qword :) }
            f1overflowed:=f1overflowed or ((f1 and (1 shl 63))<>0);
            f1:=f1 shl 1;
            bitpos:=bitpos shl 1;
          end;
      end;
{$endif FPC_SYSTEM_HAS_MUL_QWORD}


{$ifndef FPC_SYSTEM_HAS_MUL_INT64}
    function fpc_mul_int64(f1,f2 : int64;checkoverflow : longbool) : int64;[public,alias: 'FPC_MUL_INT64']; compilerproc;

      var
         sign : boolean;
         q1,q2,q3 : qword;

      begin
           begin
              sign:=false;
              if f1<0 then
                begin
                   sign:=not(sign);
                   q1:=qword(-f1);
                end
              else
                q1:=f1;
              if f2<0 then
                begin
                   sign:=not(sign);
                   q2:=qword(-f2);
                end
              else
                q2:=f2;
              { the q1*q2 is coded as call to mulqword }
              q3:=q1*q2;

              if checkoverflow and (q1 <> 0) and (q2 <>0) and
              ((q1>q3) or (q2>q3) or
                { the bit 63 can be only set if we have $80000000 00000000 }
                { and sign is true                                         }
                (q3 shr 63<>0) and
                 ((q3<>qword(qword(1) shl 63)) or not(sign))
                ) then
                HandleErrorFrame(215,get_frame);

              if sign then
                fpc_mul_int64:=-q3
              else
                fpc_mul_int64:=q3;
           end;
      end;
{$endif FPC_SYSTEM_HAS_MUL_INT64}

