{
    $Id$
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
{$ifdef ENDIAN_LITTLE}       
         low : dword;
         high : dword;
{$else not ENDIAN_LITTLE}
{$ifdef ENDIAN_BIG}
         high : dword;
         low  : dword;
{$else not ENDIAN_BIG}
  {$fatal I need to know the endianess for int64.inc code}
{$endif not ENDIAN_BIG}
{$endif not ENDIAN_LITTLE}
       end;
{$endif ENDIAN_LITTLE}
{$ifdef ENDIAN_BIG}
       tqwordrec = packed record
         high : dword;
         low : dword;
       end;
{$endif ENDIAN_BIG}

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

    function fpc_div_qword(n,z : qword) : qword;[public,alias: 'FPC_DIV_QWORD']; {$ifdef hascompilerproc} compilerproc; {$endif}

      var
         shift,lzz,lzn : longint;
         { one : qword; }

      begin
         fpc_div_qword:=0;
         if n=0 then
           HandleErrorFrame(200,get_frame);
{$ifdef i386}
         { the following piece of code is taken from the     }
         { AMD Athlon Processor x86 Code Optimization manual }
         asm
            movl n+4,%ecx
            movl n,%ebx
            movl z+4,%edx
            movl z,%eax
            testl %ecx,%ecx
            jnz .Lqworddivbigdivisor
            cmpl %ebx,%edx
            jae .Lqworddivtwo_divs
            divl %ebx
            movl %ecx,%edx
            leave
            ret $16

         .Lqworddivtwo_divs:
            movl %eax,%ecx
            movl %edx,%eax
            xorl %edx,%edx
            divl %ebx
            xchgl %ecx,%eax
            divl %ebx
            movl %ecx,%edx
            leave
            ret $16

         .Lqworddivbigdivisor:
            movl %ecx,%edi
            shrl $1,%edx
            rcrl $1,%eax
            rorl $1,%edi
            rcrl $1,%ebx
            bsrl %ecx,%ecx
            shrdl %cl,%edi,%ebx
            shrdl %cl,%edx,%eax
            shrl %cl,%edx
            roll $1,%edi
            divl %ebx
            movl z,%ebx
            movl %eax,%ecx
            imull %eax,%edi
            mull n
            addl %edi,%edx
            subl %eax,%ebx
            movl %ecx,%eax
            movl z+4,%ecx
            sbbl %edx,%ecx
            sbbl $0,%eax
            xorl %edx,%edx
            leave
            ret $16
         end;
{$else i386}
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
{$endif i386}
      end;

    function fpc_mod_qword(n,z : qword) : qword;[public,alias: 'FPC_MOD_QWORD']; {$ifdef hascompilerproc} compilerproc; {$endif}

      var
         shift,lzz,lzn : longint;

      begin
         fpc_mod_qword:=0;
         if n=0 then
           HandleErrorFrame(200,get_frame);
{$ifdef i386_not_working_correct}
         { the following piece of code is taken from the     }
         { AMD Athlon Processor x86 Code Optimization manual }
         asm
            movl n+4,%ecx
            movl n,%ebx
            movl z+4,%edx
            movl z,%eax
            testl %ecx,%ecx
            jnz .Lqwordmodr_big_divisior
            cmpl %ebx,%edx
            jae .Lqwordmodr_two_divs
            divl %ebx
            movl %edx,%eax
            movl %ecx,%edx
            leave
            ret $16

         .Lqwordmodr_two_divs:
            movl %eax,%ecx
            movl %edx,%eax
            xorl %edx,%edx
            divl %ebx
            movl %ecx,%eax
            divl %ebx
            movl %edx,%eax
            xorl %edx,%edx
            leave
            ret $16

         .Lqwordmodr_big_divisior:
            movl %ecx,%edi
            shrl $1,%edx
            rcrl $1,%eax
            rorl $1,%edi
            rcrl $1,%ebx
            bsrl %ecx,%ecx
            shrdl %cl,%edi,%ebx
            shrdl %cl,%edx,%eax
            shrl %cl,%edx
            rorl $1,%edi
            divl %ebx
            movl z,%ebx
            movl %eax,%ecx
            imull %eax,%edi
            mull n
            addl %edi,%edx
            subl %eax,%ebx
            movl z+4,%ecx
            movl n,%eax
            sbbl %edx,%ecx
            sbbl %edx,%edx
            andl %edx,%eax
            andl n+4,%edx
            addl %ebx,%eax
            adcl %ecx,%edx
            leave
            ret $16
         end;
{$else i386}
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
{$endif i386}
      end;

    function fpc_div_int64(n,z : int64) : int64;[public,alias: 'FPC_DIV_INT64']; {$ifdef hascompilerproc} compilerproc; {$endif}

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

    function fpc_mod_int64(n,z : int64) : int64;[public,alias: 'FPC_MOD_INT64']; {$ifdef hascompilerproc} compilerproc; {$endif}

      var
         signed : boolean;
         r,nq,zq : qword;

      begin
         if n=0 then
           HandleErrorFrame(200,get_frame);
         if n<0 then
           begin
              nq:=-n;
              signed:=true;
           end
         else
           begin
              signed:=false;
              nq:=n;
           end;
         if z<0 then
           begin
              zq:=qword(-z);
              signed:=not(signed);
           end
         else
           zq:=z;
         r:=zq mod nq;
         if signed then
           fpc_mod_int64:=-int64(r)
         else
           fpc_mod_int64:=r;
      end;

    { multiplies two qwords
      the longbool for checkoverflow avoids a misaligned stack
    }
    function fpc_mul_qword(f1,f2 : qword;checkoverflow : longbool) : qword;[public,alias: 'FPC_MUL_QWORD']; {$ifdef hascompilerproc} compilerproc; {$endif}

      var
         _f1,bitpos : qword;
         l : longint;

{$ifdef i386}
         r : qword;
{$endif i386}

      begin
{$ifdef i386}
         if not(checkoverflow) then
           begin
              { the following piece of code is taken from the     }
              { AMD Athlon Processor x86 Code Optimization manual }
              asm
                 movl f1+4,%edx
                 movl f2+4,%ecx
                 orl %ecx,%edx
                 movl f2,%edx
                 movl f1,%eax
                 jnz .Lqwordmultwomul
                 mull %edx
                 jmp .Lqwordmulready
              .Lqwordmultwomul:
                 imul f1+4,%edx
                 imul %eax,%ecx
                 addl %edx,%ecx
                 mull f2
                 add %ecx,%edx
              .Lqwordmulready:
                 movl %eax,r
                 movl %edx,r+4
              end;
              fpc_mul_qword:=r;
           end
         else
{$endif i386}
           begin
              fpc_mul_qword:=0;
              bitpos:=1;

              // store f1 for overflow checking
              _f1:=f1;

              for l:=0 to 63 do
                begin
                   if (f2 and bitpos)<>0 then
                     fpc_mul_qword:=fpc_mul_qword+f1;

                   f1:=f1 shl 1;
                   bitpos:=bitpos shl 1;
                end;

              { if one of the operands is greater than the result an }
              { overflow occurs                                      }
              if checkoverflow and (_f1 <> 0) and (f2 <>0) and
                 ((_f1>fpc_mul_qword) or (f2>fpc_mul_qword)) then
                HandleErrorFrame(215,get_frame);
           end;
      end;

    function fpc_mul_int64(f1,f2 : int64;checkoverflow : longbool) : int64;[public,alias: 'FPC_MUL_INT64']; {$ifdef hascompilerproc} compilerproc; {$endif}

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
                ((tqwordrec(q3).high and dword($80000000))<>0) and
                 ((q3<>(qword(1) shl 63)) or not(sign))
                ) then
                HandleErrorFrame(215,get_frame);

              if sign then
                fpc_mul_int64:=-q3
              else
                fpc_mul_int64:=q3;
           end;
      end;

    procedure qword_str(value : qword;var s : string);

      var
         hs : string;

      begin
         hs:='';
         repeat
           hs:=chr(longint(value mod qword(10))+48)+hs;
           value:=value div qword(10);
         until value=0;
         s:=hs;
      end;

    procedure int64_str(value : int64;var s : string);

      var
         hs : string;
         q : qword;

      begin
         if value<0 then
           begin
              q:=qword(-value);
              qword_str(q,hs);
              s:='-'+hs;
           end
         else
           qword_str(qword(value),s);
      end;

  procedure fpc_shortstr_qword(v : qword;len : longint;var s : shortstring);[public,alias:'FPC_SHORTSTR_QWORD']; {$ifdef hascompilerproc} compilerproc; {$endif}

    begin
       qword_str(v,s);
        if length(s)<len then
          s:=space(len-length(s))+s;
    end;

  procedure fpc_shortstr_int64(v : int64;len : longint;var s : shortstring);[public,alias:'FPC_SHORTSTR_INT64'];  {$ifdef hascompilerproc} compilerproc; {$endif}

    begin
       int64_str(v,s);
       if length(s)<len then
         s:=space(len-length(s))+s;
    end;

  procedure fpc_ansistr_qword(v : qword;len : longint;var s : ansistring);[public,alias:'FPC_ANSISTR_QWORD']; {$ifdef hascompilerproc} compilerproc; {$endif}

    var
       ss : shortstring;

    begin
       str(v:len,ss);
       s:=ss;
    end;

  procedure fpc_ansistr_int64(v : int64;len : longint;var s : ansistring);[public,alias:'FPC_ANSISTR_INT64']; {$ifdef hascompilerproc} compilerproc; {$endif}

    var
       ss : shortstring;

    begin
       str(v:len,ss);
       s:=ss;
    end;

{$ifdef HASWIDESTRING}
  procedure fpc_widestr_qword(v : qword;len : longint;var s : widestring);[public,alias:'FPC_WIDESTR_QWORD']; {$ifdef hascompilerproc} compilerproc; {$endif}

    var
       ss : shortstring;

    begin
       str(v:len,ss);
       s:=ss;
    end;

  procedure fpc_widestr_int64(v : int64;len : longint;var s : widestring);[public,alias:'FPC_WIDESTR_INT64']; {$ifdef hascompilerproc} compilerproc; {$endif}

    var
       ss : shortstring;

    begin
       str(v:len,ss);
       s:=ss;
    end;
{$endif HASWIDESTRING}


  Function fpc_val_int64_shortstr(Const S: ShortString; var Code: ValSInt): Int64; [public, alias:'FPC_VAL_INT64_SHORTSTR']; {$ifdef hascompilerproc} compilerproc; {$endif}
   type
     QWordRec = packed record
       l1,l2: longint;
     end;

    var
       u, temp, prev, maxint64, maxqword : qword;
       base : byte;
       negative : boolean;

  begin
    fpc_val_int64_shortstr := 0;
    Temp:=0;
    Code:=InitVal(s,negative,base);
    if Code>length(s) then
     exit;
    { high(int64) produces 0 in version 1.0 (JM) }
    with qwordrec(maxint64) do
      begin
        l1 := longint($ffffffff);
        l2 := $7fffffff;
      end;
    with qwordrec(maxqword) do
      begin
        l1 := longint($ffffffff);
        l2 := longint($ffffffff);
      end;

    while Code<=Length(s) do
     begin
       case s[Code] of
         '0'..'9' : u:=Ord(S[Code])-Ord('0');
         'A'..'F' : u:=Ord(S[Code])-(Ord('A')-10);
         'a'..'f' : u:=Ord(S[Code])-(Ord('a')-10);
       else
        u:=16;
       end;
       Prev:=Temp;
       Temp:=Temp*Int64(base);
     If (u >= base) or
        ((base = 10) and
         (maxint64-temp+ord(negative) < u)) or
        ((base <> 10) and
         (qword(maxqword-temp) < u)) or
        (prev > maxqword div qword(base)) Then
       Begin
         fpc_val_int64_shortstr := 0;
         Exit
       End;
       Temp:=Temp+u;
       inc(code);
     end;
    code:=0;
    fpc_val_int64_shortstr:=int64(Temp);
    If Negative Then
      fpc_val_int64_shortstr:=-fpc_val_int64_shortstr;
  end;


  Function fpc_val_qword_shortstr(Const S: ShortString; var Code: ValSInt): QWord; [public, alias:'FPC_VAL_QWORD_SHORTSTR']; {$ifdef hascompilerproc} compilerproc; {$endif}
    type qwordrec = packed record
      l1,l2: longint;
    end;
    var
       u, prev, maxqword: QWord;
       base : byte;
       negative : boolean;
  begin
    fpc_val_qword_shortstr:=0;
    Code:=InitVal(s,negative,base);
    If Negative or (Code>length(s)) Then
      Exit;
    with qwordrec(maxqword) do
      begin
        l1 := longint($ffffffff);
        l2 := longint($ffffffff);
      end;
    while Code<=Length(s) do
     begin
       case s[Code] of
         '0'..'9' : u:=Ord(S[Code])-Ord('0');
         'A'..'F' : u:=Ord(S[Code])-(Ord('A')-10);
         'a'..'f' : u:=Ord(S[Code])-(Ord('a')-10);
       else
        u:=16;
       end;
       prev := fpc_val_qword_shortstr;
       If (u>=base) or
         ((QWord(maxqword-u) div QWord(base))<prev) then
         Begin
           fpc_val_qword_shortstr := 0;
           Exit
         End;
       fpc_val_qword_shortstr:=fpc_val_qword_shortstr*QWord(base) + u;
       inc(code);
     end;
    code := 0;
  end;

{
  $Log$
  Revision 1.19  2003-05-12 11:16:21  florian
    * qword division fixed (MSB/LSB problem)

  Revision 1.18  2003/05/12 07:19:04  jonas
    * fixed for big endian systems (since Florian doesn't seem to want to
      commit this fix :)

  Revision 1.17  2002/09/07 21:21:42  carl
    - remove FPUInt64 variable

  Revision 1.16  2002/09/07 15:07:45  peter
    * old logs removed and tabs fixed

  Revision 1.15  2002/09/01 14:44:01  peter
    * renamed conditional to insert optimized mod_qword for i386. The
      code is broken

  Revision 1.14  2002/07/01 16:29:05  peter
    * sLineBreak changed to normal constant like Kylix

}
