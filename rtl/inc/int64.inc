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

    procedure int_overflow;

      begin
         runerror(201);
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

    function divqword(z,n : qword) : qword;[public,alias: 'FPC_DIV_QWORD'];

      var
         shift,lzz,lzn : longint;          

      begin
         {!!!!!!!!
         divqword:=0;
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
           if z>n then
             begin
                z:=z-n;
                divqword:=divqword+(1 shl shift);
             end;
           dec(shift);
           n:=n shr 1;
         until shift<=0;
         }
      end;

    function modqword(z,n : qword) : qword;[public,alias: 'FPC_MOD_QWORD'];

      var
         shift,lzz,lzn : longint;

      begin
         modqword:=z;
         lzz:=count_leading_zeros(z);
         lzn:=count_leading_zeros(n);
         { if the denominator contains less zeros }
         { the d is greater than the n            }
         if lzn<lzz then
           exit;
         shift:=lzn-lzz;
         n:=n shl shift;
         repeat                                
           if z>n then
             z:=z-n;
           dec(shift);
           n:=n shr 1;
         until shift<=0;
         modqword:=z;
      end;

    function divint64(z,n : int64) : int64;[public,alias: 'FPC_DIV_INT64'];

      var
         sign : boolean;
         q1,q2 : qword;

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
         {!!!!!!!
         if sign then
           divint64:=-q1 div q2
         else
           divint64:=q1 div q2;
         }
      end;

    { multiplies two qwords }
    function mulqword(f1,f2 : qword;checkoverflow : boolean) : qword;[public,alias: 'FPC_MUL_QWORD'];

      var
         zero,bitpos64,bitpos : qword;
         l : longint;


      begin
         { we can't write currently qword constants directly :( }
         zero:=zero xor zero;
         mulqword:=zero;
         tqwordrec(bitpos64).high:=$80000000;
         tqwordrec(bitpos64).low:=0;
         tqwordrec(bitpos).high:=0;
         tqwordrec(bitpos).low:=1;

         for l:=0 to 63 do
           begin
              { if the highest bit of f1 is set and it isn't the
                last run, then an overflow occcurs!
              }
              if checkoverflow and (l<>63) and
                ((tqwordrec(f1).high and $80000000)<>0)  then
                int_overflow;

              if (f2 and bitpos)<>zero then
                begin
                   if checkoverflow then
{$Q+}                
                     mulqword:=mulqword+f1
{$Q-}
                   else
                     mulqword:=mulqword+f1;
                end;

              f1:=f1 shl 1;
              bitpos:=bitpos shl 1;
           end;
      end;

    {    multiplies two int64 ....
       fpuint64 = false:
         ... using the the qword multiplication
       fpuint64 = true:
         ... using the comp multiplication
     }
    function mulint64(f1,f2 : int64;checkoverflow : boolean) : int64;[public,alias: 'FPC_MUL_INT64'];

      var
         sign : boolean;
         q1,q2,q3 : qword;

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
         if checkoverflow then
{$Q+}
           q3:=q1*q2 
         else
{$Q-}
           q3:=q1*q2;

         if sign then
           mulint64:=-q3
         else
           mulint64:=q3;
      end;

    procedure int_str(value : qword;var s : string);

      var
         hs : string;

      begin
         {!!!!!!!!!!!
         hs:='';
         repeat
           hs:=chr(longint(value mod 10)+48)+hs;
           value:=value div 10;
         until value=0;
         s:=hs;
         }
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
  Revision 1.5  1999-05-25 20:36:41  florian
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
