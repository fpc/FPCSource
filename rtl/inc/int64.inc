{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1998 by the Free Pascal development team

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
              if (tqwordrec(q).high and ($80000000 shr i))<>0 then
                begin
                   count_leading_zeros:=r;
                   exit;
                end;
              inc(r);
           end;
         for i:=0 to 31 do
           begin
              if (tqwordrec(q).low and ($80000000 shr i))<>0 then
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
         one : qword;

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

      begin
         if n=0 then
           HandleErrorFrame(200,get_frame);
         { can the fpu do the work? }
         if fpuint64 then
           //!!!!!!!!!!! divint64:=comp(z)/comp(n)
         else
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

    { multiplies two qwords
      the longbool for checkoverflow avoids a misaligned stack
    }
    function mulqword(f1,f2 : qword;checkoverflow : longbool) : qword;[public,alias: 'FPC_MUL_QWORD'];

      var
         zero,bitpos64,bitpos : qword;
         l : longint;


      begin
         zero:=0;
         mulqword:=0;
         { we can't write currently qword constants directly :( }
         tqwordrec(bitpos64).high:=$80000000;
         tqwordrec(bitpos64).low:=0;
         bitpos:=1;

         for l:=0 to 63 do
           begin
              if (f2 and bitpos)<>zero then
                mulqword:=mulqword+f1;

              f1:=f1 shl 1;
              bitpos:=bitpos shl 1;
           end;

         { if one of the operands is greater than the result an }
         { overflow occurs                                      }
         if checkoverflow and ((f1>mulqword) or (f2>mulqword)) then
           HandleErrorFrame(215,get_frame);
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

      begin
         { can the fpu do the work ? }
         if fpuint64 and not(checkoverflow) then
           // !!!!!!! multint64:=comp(f1)*comp(f2)
         else
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

              if checkoverflow and ((q1>q3) or (q2>q3) or
                { the bit 63 can be only set if we have $80000000 00000000 }
                { and sign is true                                         }
                ((tqwordrec(q3).high and $80000000)<>0) and
                 ((q3<>(qword(1) shl 63)) or not(sign))
                ) then
                HandleErrorFrame(215,get_frame);

              if sign then
                mulint64:=-q3
              else
                mulint64:=q3;
           end;
      end;

    procedure int_str(value : qword;var s : string);

      var
         hs : string;

      begin
         hs:='';
         repeat
           hs:=chr(longint(value mod 10)+48)+hs;
           value:=value div 10;
         until value=0;
         s:=hs;
      end;

    procedure int_str(value : int64;var s : string);

      var
         hs : string;
         q : qword;

      begin
         if value<0 then
           begin
              q:=qword(-value);
              int_str(q,hs);
              s:='-'+hs;
           end
         else
           int_str(qword(value),s);
      end;

{
  $Log$
  Revision 1.10  1999-07-01 15:39:50  florian
    + qword/int64 type released

  Revision 1.9  1999/06/30 22:12:40  florian
    * qword div/mod fixed
    + int64 mod/div/* fully implemented
    * int_str(qword) fixed
    + dummies for read/write(qword)

  Revision 1.8  1999/06/28 22:25:25  florian
    * fixed qword division

  Revision 1.7  1999/06/25 12:24:44  pierre
   * qword one was wrong !

  Revision 1.6  1999/06/02 10:13:16  florian
    * multiplication fixed

  Revision 1.5  1999/05/25 20:36:41  florian
    * some bugs removed

  Revision 1.4  1999/05/24 08:43:46  florian
    * fixed a couple of syntax errors

  Revision 1.3  1999/05/23 20:27:27  florian
    + routines for qword div and mod

  Revision 1.2  1999/01/06 12:25:03  florian
    * naming for str(...) routines inserted
    * don't know what in int64 changed

  Revision 1.1  1998/12/12 12:15:41  florian
    + first implementation
}
