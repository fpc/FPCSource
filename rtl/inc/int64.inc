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
       tqwordrec = packed record
         low : dword;
         high : dword;
       end;

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

    function divqword(n,z : qword) : qword;[public,alias: 'FPC_DIV_QWORD'];

      var
         shift,lzz,lzn : longint;
         { one : qword; }

      begin
         divqword:=0;
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
                divqword:=divqword+(qword(1) shl shift);
             end;
           dec(shift);
           n:=n shr 1;
         until shift<0;
      end;

    function modqword(n,z : qword) : qword;[public,alias: 'FPC_MOD_QWORD'];

      var
         shift,lzz,lzn : longint;

      begin
         modqword:=0;
         if n=0 then
           HandleErrorFrame(200,get_frame);
         lzz:=count_leading_zeros(z);
         lzn:=count_leading_zeros(n);
         { if the denominator contains less zeros }
         { then the numerator                     }
         { the d is greater than the n            }
         if lzn<lzz then
           begin
              modqword:=z;
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
         modqword:=z;
      end;

    function divint64(n,z : int64) : int64;[public,alias: 'FPC_DIV_INT64'];

      var
         sign : boolean;
         q1,q2 : qword;
{$ifdef SUPPORT_COMP}
         c : comp;
{$endif}

      begin
         if n=0 then
           HandleErrorFrame(200,get_frame);
         { can the fpu do the work? }
{$ifdef support_comp}
         if fpuint64 then
           begin
              // the c:=comp(...) is necessary to shut up the compiler
              c:=comp(comp(z)/comp(n));
              divint64:=qword(c);
           end
         else
{$endif}
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
                divint64:=-(q1 div q2)
              else
                divint64:=q1 div q2;
           end;
      end;

    function modint64(n,z : int64) : int64;[public,alias: 'FPC_MOD_INT64'];

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
           modint64:=-int64(r)
         else
           modint64:=r;
      end;

    { multiplies two qwords
      the longbool for checkoverflow avoids a misaligned stack
    }
    function mulqword(f1,f2 : qword;checkoverflow : longbool) : qword;[public,alias: 'FPC_MUL_QWORD'];

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
              mulqword:=r;
           end
         else
{$endif i386}
           begin
              mulqword:=0;
              bitpos:=1;

              // store f1 for overflow checking
              _f1:=f1;

              for l:=0 to 63 do
                begin
                   if (f2 and bitpos)<>0 then
                     mulqword:=mulqword+f1;

                   f1:=f1 shl 1;
                   bitpos:=bitpos shl 1;
                end;

              { if one of the operands is greater than the result an }
              { overflow occurs                                      }
              if checkoverflow and (_f1 <> 0) and (f2 <>0) and
                 ((_f1>mulqword) or (f2>mulqword)) then
                HandleErrorFrame(215,get_frame);
           end;
      end;

    {    multiplies two int64 ....
       fpuint64 = false:
         ... using the the qword multiplication
       fpuint64 = true:
         ... using the comp multiplication
       the longbool for checkoverflow avoids a misaligned stack
     }
    function mulint64(f1,f2 : int64;checkoverflow : longbool) : int64;[public,alias: 'FPC_MUL_INT64'];

      var
         sign : boolean;
         q1,q2,q3 : qword;
{$ifdef support_comp}
         c : comp;
{$endif}

      begin
{$ifdef support_comp}
         { can the fpu do the work ? }
         if fpuint64 and not(checkoverflow) then
           begin
              // the c:=comp(...) is necessary to shut up the compiler
              c:=comp(comp(f1)*comp(f2));
              mulint64:=int64(c);
           end
         else
{$endif}
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
                mulint64:=-q3
              else
                mulint64:=q3;
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
  Revision 1.11  2001-08-13 12:40:16  jonas
    * renamed some str(x,y) and val(x,y) helpers so the naming scheme is the
      same for all string types
    + added the str(x,y) and val(x,y,z) helpers for int64/qword to
      compproc.inc

  Revision 1.10  2001/04/23 18:25:45  peter
    * m68k updates

  Revision 1.9  2001/04/13 22:30:04  peter
    * remove warnings

  Revision 1.8  2001/03/03 12:39:09  jonas
    * fixed qword_str for values with bit 63 = 1

  Revision 1.7  2000/12/10 15:00:14  florian
    * val for int64 hopefully works now correct

  Revision 1.6  2000/12/09 20:52:40  florian
    * val for dword and qword didn't handle the max values
      correctly
    * val for qword works again
    + val with int64/qword and ansistring implemented

  Revision 1.5  2000/12/07 17:19:47  jonas
    * new constant handling: from now on, hex constants >$7fffffff are
      parsed as unsigned constants (otherwise, $80000000 got sign extended
      and became $ffffffff80000000), all constants in the longint range
      become longints, all constants >$7fffffff and <=cardinal($ffffffff)
      are cardinals and the rest are int64's.
    * added lots of longint typecast to prevent range check errors in the
      compiler and rtl
    * type casts of symbolic ordinal constants are now preserved
    * fixed bug where the original resulttype wasn't restored correctly
      after doing a 64bit rangecheck

  Revision 1.4  2000/11/17 17:01:23  jonas
    * fixed bug for val when processing -2147483648 and low(int64) (merged)

  Revision 1.3  2000/07/28 12:29:49  jonas
    * fixed web bug1069
    * fixed similar (and other) problems in val() for int64 and qword
      (both merged from fixes branch)

  Revision 1.2  2000/07/13 11:33:44  michael
  + removed logs

}
