
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
    function fpc_shl_qword(value : qword;shift : ALUUInt) : qword; [public,alias: 'FPC_SHL_QWORD']; compilerproc;
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


{$ifndef FPC_SYSTEM_HAS_SHL_ASSIGN_QWORD}
    procedure fpc_shl_assign_qword(var value : qword;shift : ALUUInt); [public,alias: 'FPC_SHL_ASSIGN_QWORD']; compilerproc;
      begin
        shift:=shift and 63;
        if shift<>0 then
          begin
            if shift>31 then
              begin
                tqwordrec(value).high:=tqwordrec(value).low shl (shift-32);
                tqwordrec(value).low:=0;
              end
            else
              begin
                tqwordrec(value).high:=(tqwordrec(value).high shl shift) or (tqwordrec(value).low shr (32-shift));
                tqwordrec(value).low:=tqwordrec(value).low shl shift;
              end;
          end;
      end;
{$endif FPC_SYSTEM_HAS_SHL_ASSIGN_QWORD}


{$ifndef FPC_SYSTEM_HAS_SHR_QWORD}
   function fpc_shr_qword(value : qword;shift : ALUUInt) : qword; [public,alias: 'FPC_SHR_QWORD']; compilerproc;
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


{$ifndef FPC_SYSTEM_HAS_SHR_ASSIGN_QWORD}
   procedure fpc_shr_assign_qword(var value : qword;shift : ALUUInt); [public,alias: 'FPC_SHR_ASSIGN_QWORD']; compilerproc;
      begin
        shift:=shift and 63;
        if shift<>0 then
          begin
            if shift>31 then
              begin
                tqwordrec(value).low:=tqwordrec(value).high shr (shift-32);
                tqwordrec(value).high:=0;
              end
            else
              begin
                tqwordrec(value).low:=(tqwordrec(value).low shr shift) or (tqwordrec(value).high shl (32-shift));
                tqwordrec(value).high:=tqwordrec(value).high shr shift;
              end;
          end;
      end;
{$endif FPC_SYSTEM_HAS_SHR_ASSIGN_QWORD}


{$ifndef FPC_SYSTEM_HAS_SHL_INT64}
    function fpc_shl_int64(value : int64;shift : ALUUInt) : int64; [public,alias: 'FPC_SHL_INT64']; compilerproc;
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


{$ifndef FPC_SYSTEM_HAS_SHL_ASSIGN_INT64}
    procedure fpc_shl_assign_int64(var value : int64;shift : ALUUInt); [public,alias: 'FPC_SHL_ASSIGN_INT64']; compilerproc;
      begin
        shift:=shift and 63;
        if shift<>0 then
          begin
            if shift>31 then
              begin
                tqwordrec(value).high:=tqwordrec(value).low shl (shift-32);
                tqwordrec(value).low:=0;
              end
            else
              begin
                tqwordrec(value).high:=(tqwordrec(value).high shl shift) or (tqwordrec(value).low shr (32-shift));
                tqwordrec(value).low:=tqwordrec(value).low shl shift;
              end;
          end;
      end;
{$endif FPC_SYSTEM_HAS_SHL_ASSIGN_INT64}


{$ifndef FPC_SYSTEM_HAS_SHR_INT64}
    function fpc_shr_int64(value : int64;shift : ALUUInt) : int64; [public,alias: 'FPC_SHR_INT64']; compilerproc;
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


{$ifndef FPC_SYSTEM_HAS_SHR_ASSIGN_INT64}
    procedure fpc_shr_assign_int64(var value : int64;shift : ALUUInt); [public,alias: 'FPC_SHR_ASSIGN_INT64']; compilerproc;
      begin
        shift:=shift and 63;
        if shift<>0 then
          begin
            if shift>31 then
              begin
                tqwordrec(value).low:=tqwordrec(value).high shr (shift-32);
                tqwordrec(value).high:=0;
              end
            else
              begin
                tqwordrec(value).low:=(tqwordrec(value).low shr shift) or (tqwordrec(value).high shl (32-shift));
                tqwordrec(value).high:=tqwordrec(value).high shr shift;
              end;
          end;
      end;
{$endif FPC_SYSTEM_HAS_SHR_ASSIGN_INT64}


{$endif FPC_INCLUDE_SOFTWARE_SHIFT_INT64}


{$ifndef FPC_SYSTEM_HAS_DIV_QWORD}
    function fpc_div_qword(n,z : qword) : qword;[public,alias: 'FPC_DIV_QWORD']; compilerproc;

      var
         shift,lzz,lzn : longint;

      begin
         { Use the usually faster 32-bit division if possible }
	       if (hi(z) = 0) and (hi(n) = 0) then
	         begin
	           fpc_div_qword := Dword(z) div Dword(n);
             exit;
	         end;
         fpc_div_qword:=0;
         if n=0 then
           HandleErrorAddrFrameInd(200,get_pc_addr,get_frame);
         if z=0 then
           exit;
         lzz:=BsrQWord(z);
         lzn:=BsrQWord(n);
         { if the denominator contains less zeros }
         { than the numerator                     }
         { then d is greater than the n           }
         if lzn>lzz then
           exit;

         shift:=lzz-lzn;
         n:=n shl shift;
         for shift:=shift downto 0 do
           begin
             if z>=n then
               begin
                  z:=z-n;
                  fpc_div_qword:=fpc_div_qword+(qword(1) shl shift);
               end;
             n:=n shr 1;
           end;
      end;
{$endif FPC_SYSTEM_HAS_DIV_QWORD}


{$ifndef FPC_SYSTEM_HAS_MOD_QWORD}
    function fpc_mod_qword(n,z : qword) : qword;[public,alias: 'FPC_MOD_QWORD']; compilerproc;

      var
         shift,lzz,lzn : longint;

      begin
         { Use the usually faster 32-bit mod if possible }
         if (hi(z) = 0) and (hi(n) = 0) then
           begin
             fpc_mod_qword := Dword(z) mod Dword(n);
             exit;
           end;
         fpc_mod_qword:=0;
         if n=0 then
           HandleErrorAddrFrameInd(200,get_pc_addr,get_frame);
         if z=0 then
           exit;
         lzz:=BsrQword(z);
         lzn:=BsrQword(n);
         { if the denominator contains less zeros }
         { then the numerator                     }
         { the d is greater than the n            }
         if lzn>lzz then
           begin
              fpc_mod_qword:=z;
              exit;
           end;
         shift:=lzz-lzn;
         n:=n shl shift;
         for shift:=shift downto 0 do
           begin
             if z>=n then
               z:=z-n;
             n:=n shr 1;
           end;
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
           HandleErrorAddrFrameInd(200,get_pc_addr,get_frame);
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
           HandleErrorAddrFrameInd(200,get_pc_addr,get_frame);
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
    function fpc_mul_qword(f1,f2 : qword) : qword;[public,alias: 'FPC_MUL_QWORD']; compilerproc;
      var
        b : byte;
      begin
        result:=0;

        for b:=0 to 63 do
          begin
            if odd(f2) then
              result:=result+f1;
            f1:=f1 shl 1;
            f2:=f2 shr 1;
          end;
      end;


    function fpc_mul_qword_checkoverflow(f1,f2 : qword) : qword;[public,alias: 'FPC_MUL_QWORD_CHECKOVERFLOW']; compilerproc;
      var
         _f1,bitpos : qword;
         b : byte;
         f1overflowed : boolean;
      begin
        result:=0;
        bitpos:=1;
        f1overflowed:=false;

        for b:=0 to 63 do
          begin
            if (f2 and bitpos)<>0 then
              begin
                _f1:=result;
                result:=result+f1;

                { if one of the operands is greater than the result an
                  overflow occurs                                      }
                if (f1overflowed or ((_f1<>0) and (f1<>0) and
                  ((_f1>result) or (f1>result)))) then
                  HandleErrorAddrFrameInd(215,get_pc_addr,get_frame);
              end;
            { when bootstrapping, we forget about overflow checking for qword :) }
            f1overflowed:=f1overflowed or ((f1 and (qword(1) shl 63))<>0);
            f1:=f1 shl 1;
            bitpos:=bitpos shl 1;
          end;
      end;
{$endif FPC_SYSTEM_HAS_MUL_QWORD}

{$ifndef FPC_SYSTEM_HAS_MUL_DWORD_TO_QWORD}
    function fpc_mul_qword_compilerproc(f1,f2 : qword) : qword; external name 'FPC_MUL_QWORD';

    function fpc_mul_dword_to_qword(f1,f2 : dword) : qword;[public,alias: 'FPC_MUL_DWORD_TO_QWORD']; compilerproc;
      begin
        fpc_mul_dword_to_qword:=fpc_mul_qword_compilerproc(f1,f2);
      end;
{$endif FPC_SYSTEM_HAS_MUL_DWORD_TO_QWORD}


{$ifndef FPC_SYSTEM_HAS_MUL_INT64}
    function fpc_mul_int64(f1,f2 : int64) : int64;[public,alias: 'FPC_MUL_INT64']; compilerproc;
      begin
        { there's no difference between signed and unsigned multiplication,
          when the destination size is equal to the source size and overflow
          checking is off }
        { qword(f1)*qword(f2) is coded as a call to mulqword }
        result:=int64(qword(f1)*qword(f2));
      end;


    function fpc_mul_int64_checkoverflow(f1,f2 : int64) : int64;[public,alias: 'FPC_MUL_INT64_CHECKOVERFLOW']; compilerproc;
{$ifdef EXCLUDE_COMPLEX_PROCS}
      begin
        runerror(217);
      end;
{$else EXCLUDE_COMPLEX_PROCS}
      var
         sign : boolean;
         q1,q2,q3 : qword;
      begin
        if f1<0 then
          begin
            q1:=qword(-f1);
            sign:=true;
          end
        else
          begin
            q1:=f1;
            sign:=false;
          end;
        if f2<0 then
          begin
            sign:=not(sign);
            q2:=qword(-f2);
          end
        else
          q2:=f2;
        { the q1*q2 is coded as call to mulqword }
        q3:=q1*q2;

        if (q1 <> 0) and (q2 <>0) and
          ((q1>q3) or (q2>q3) or
            { the bit 63 can be only set if we have $80000000 00000000 }
            { and sign is true                                         }
            (q3 shr 63<>0) and
             ((q3<>qword(qword(1) shl 63)) or not(sign))
            ) then
          HandleErrorAddrFrameInd(215,get_pc_addr,get_frame);

        if sign then
          result:=-q3
        else
          result:=q3;
      end;
{$endif EXCLUDE_COMPLEX_PROCS}
{$endif FPC_SYSTEM_HAS_MUL_INT64}

{$ifndef FPC_SYSTEM_HAS_MUL_LONGINT_TO_INT64}
    function fpc_mul_int64_compilerproc(f1,f2 : int64) : int64; external name 'FPC_MUL_INT64';

    function fpc_mul_longint_to_int64(f1,f2 : longint) : int64;[public,alias: 'FPC_MUL_LONGINT_TO_INT64']; compilerproc;
{$ifdef EXCLUDE_COMPLEX_PROCS}
      begin
        runerror(217);
      end;
{$else EXCLUDE_COMPLEX_PROCS}
      begin
        fpc_mul_longint_to_int64:=fpc_mul_int64_compilerproc(f1,f2);
      end;
{$endif EXCLUDE_COMPLEX_PROCS}

{$endif FPC_SYSTEM_HAS_MUL_LONGINT_TO_INT64}

{$ifndef FPC_SYSTEM_HAS_DIV_CURRENCY}
function fpc_div_currency(n,z : currency) : currency; [public,alias: 'FPC_DIV_CURRENCY']; compilerproc;
  begin
    Result:=(int64(z)*10000) div int64(n);
  end;
{$endif FPC_SYSTEM_HAS_DIV_CURRENCY}

{$ifndef FPC_SYSTEM_HAS_MOD_CURRENCY}
function fpc_mod_currency(n,z : currency) : currency; [public,alias: 'FPC_MOD_CURRENCY']; compilerproc;
  begin
    Result:=int64(z) mod int64(n);
  end;
{$endif FPC_SYSTEM_HAS_MOD_CURRENCY}

{$ifndef FPC_SYSTEM_HAS_MUL_CURRENCY}
{ Multiplies two already-scaled (x10000) Int64 Currency internal
  representations and rescales the result back down by one factor of
  10000, i.e. computes (a*b) div 10000.

  This can NOT be done as a plain "int64*int64, then div 10000", because
  both a and b are already scaled by 10000, so their product very easily
  exceeds the range of a 64-bit integer even for operands that are
  themselves well within Currency's legal range (approximately
  +/-922337203685477.5807). The multiplication has to be performed with a
  full 128-bit intermediate result, which is only truncated back down to
  64 bits *after* the division by 10000.

  This routine is only used for the Int64-based Currency implementation
  (s64currencytype.typ=orddef), which is currently the case on Win64
  because x87 extended precision types -- used to implement Currency on
  most other x86(_64) targets, sidestepping this issue entirely -- are
  not supported there.

  Parameters/result are plain Int64 (not Currency): the compiler builds
  and typechecks this call using the raw scaled Int64 representation
  directly (setting the operand nodes' resultdef to s64inttype before
  constructing the call, then re-tagging the finished call node's
  resultdef as Currency afterwards -- see taddnode.do_currency_corrections
  in nadd.pas). This deliberately avoids routing the arguments through
  the compiler's generic Currency-typed parameter-conversion machinery,
  which -- being designed for ordinary source-level Currency expressions,
  not already-scaled internal representations -- would otherwise rescale
  the already-correct raw values a second time. }
function fpc_mul_currency(x,y : currency) : currency; [public,alias: 'FPC_MUL_CURRENCY']; compilerproc;
  var
    a : int64 absolute x;
    b : int64 absolute y;
    neg : boolean;
    ua,ub : qword;
    a_lo,a_hi,b_lo,b_hi : qword;
    t0,t1,t2,t3 : qword;
    mid : qword;
    prod_hi,prod_lo : qword;
    limb : array[0..3] of qword;
    i : longint;
    rem,cur : qword;
    q : array[0..3] of qword;
    result_q : qword;
    result_i : int64 absolute result;
  begin
    neg:=false;
    if int64(a)<0 then
      begin
        neg:=true;
        ua:=qword(-int64(a));
      end
    else
      ua:=qword(a);
    if int64(b)<0 then
      begin
        neg:=not neg;
        ub:=qword(-int64(b));
      end
    else
      ub:=qword(b);

    { full 64x64 -> 128 bit unsigned multiply, done manually via 32-bit
      halves so that it works regardless of whether the target has a
      native widening multiply instruction exposed to the compiler }
    a_lo:=ua and qword($FFFFFFFF);
    a_hi:=ua shr 32;
    b_lo:=ub and qword($FFFFFFFF);
    b_hi:=ub shr 32;

    t0:=a_lo*b_lo;
    t1:=a_hi*b_lo;
    t2:=a_lo*b_hi;
    t3:=a_hi*b_hi;

    mid:=(t0 shr 32)+(t1 and qword($FFFFFFFF))+(t2 and qword($FFFFFFFF));

    prod_lo:=(t0 and qword($FFFFFFFF)) or (mid shl 32);
    prod_hi:=t3+(t1 shr 32)+(t2 shr 32)+(mid shr 32);

    { divide the 128-bit product by 10000 using 32-bit-limb short division;
      this is safe/exact because 10000 fits comfortably in 32 bits, so the
      running remainder (always < 10000) combined with the next limb never
      exceeds 64 bits }
    limb[0]:=prod_lo and qword($FFFFFFFF);
    limb[1]:=prod_lo shr 32;
    limb[2]:=prod_hi and qword($FFFFFFFF);
    limb[3]:=prod_hi shr 32;

    rem:=0;
    for i:=3 downto 0 do
      begin
        cur:=(rem shl 32) or limb[i];
        q[i]:=cur div 10000;
        rem:=cur mod 10000;
      end;

    { for operands whose mathematical product legitimately fits back into
      a 64-bit Currency value, q[3] and q[2] are always 0; if the true
      result exceeds Currency's range, we -- like ordinary Int64
      arithmetic with overflow checking disabled -- silently keep only
      the low 64 bits instead of raising an error }
    result_q:=(q[1] shl 32) or q[0];

    if neg then
      Result_i:=-int64(result_q)
    else
      Result_i:=int64(result_q);
  end;


function fpc_mul_currency_checkoverflow(x,y : currency) : currency;[public,alias: 'FPC_MUL_CURRENCY_CHECKOVERFLOW']; compilerproc;
{$ifdef EXCLUDE_COMPLEX_PROCS}
  begin
    runerror(217);
  end;
{$else EXCLUDE_COMPLEX_PROCS}
var
  a : int64 absolute x;
  b : int64 absolute y;
  neg : boolean;
  ua,ub : qword;
  a_lo,a_hi,b_lo,b_hi : qword;
  t0,t1,t2,t3 : qword;
  mid : qword;
  prod_hi,prod_lo : qword;
  limb : array[0..3] of qword;
  i : longint;
  rem,cur : qword;
  q : array[0..3] of qword;
  result_q : qword;
  result_i : int64 absolute result;
begin
  neg:=false;
  if int64(a)<0 then
    begin
      neg:=true;
      ua:=qword(-int64(a));
    end
  else
    ua:=qword(a);
  if int64(b)<0 then
    begin
      neg:=not neg;
      ub:=qword(-int64(b));
    end
  else
    ub:=qword(b);

  { full 64x64 -> 128 bit unsigned multiply, done manually via 32-bit
    halves so that it works regardless of whether the target has a
    native widening multiply instruction exposed to the compiler }
  a_lo:=ua and qword($FFFFFFFF);
  a_hi:=ua shr 32;
  b_lo:=ub and qword($FFFFFFFF);
  b_hi:=ub shr 32;

  t0:=a_lo*b_lo;
  t1:=a_hi*b_lo;
  t2:=a_lo*b_hi;
  t3:=a_hi*b_hi;

  mid:=(t0 shr 32)+(t1 and qword($FFFFFFFF))+(t2 and qword($FFFFFFFF));

  prod_lo:=(t0 and qword($FFFFFFFF)) or (mid shl 32);
  prod_hi:=t3+(t1 shr 32)+(t2 shr 32)+(mid shr 32);

  { divide the 128-bit product by 10000 using 32-bit-limb short division;
    this is safe/exact because 10000 fits comfortably in 32 bits, so the
    running remainder (always < 10000) combined with the next limb never
    exceeds 64 bits }
  limb[0]:=prod_lo and qword($FFFFFFFF);
  limb[1]:=prod_lo shr 32;
  limb[2]:=prod_hi and qword($FFFFFFFF);
  limb[3]:=prod_hi shr 32;

  rem:=0;
  for i:=3 downto 0 do
    begin
      cur:=(rem shl 32) or limb[i];
      q[i]:=cur div 10000;
      rem:=cur mod 10000;
    end;

  { for operands whose mathematical product legitimately fits back into
    a 64-bit Currency value, q[3] and q[2] are always 0 }
  result_q:=(q[1] shl 32) or q[0];
  if (q[2]<>0) or (q[3]<>0) or
    (
      { bit 63 can be only set if we have $80000000 00000000 }
      { and sign is true                                     }
      (result_q shr 63<>0) and
      ((result_q<>qword(qword(1) shl 63)) or not(neg))
    ) then
    HandleErrorAddrFrameInd(215,get_pc_addr,get_frame);

  if neg then
    Result_i:=-int64(result_q)
  else
    Result_i:=int64(result_q);
end;
{$endif EXCLUDE_COMPLEX_PROCS}
{$endif FPC_SYSTEM_HAS_MUL_CURRENCY}
