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
       qwordrec = packed record
         low : dword;
         high : dword;
       end;

    function count_leading_zero(q : qword) : longint;

      var
         r,i : longint;

      begin
         r:=0;
         for i:=0 to 31 do
           begin
              if (qwordrec(q).high and ($80000000 shr i))<>0 then
                begin
                   count_leading_zero:=r;
                   exit;
                end;
              inc(r);
           end;
         for i:=0 to 31 do
           begin
              if (qwordrec(q).low and ($80000000 shr i))<>0 then
                begin
                   count_leading_zero:=r;
                   exit;
                end;
              inc(r);
           end;
         count_leading_zero:=r;
      end;

    function divqword(z,n : qword) : qword;safecall;

      var
         shift,lzz,lzn : longint;          

      begin
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
      end;

    function modqword(z,n : qword) : qword;safecall;

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

    function divint64(z,n : int64) : int64;safecall;

      var
         sign : boolean;
         q1,q2,q3 : qword;

      begin
         sign:=false;
         if z<0 then
           begin
              sign:=not(sign);
              q1:=qword(-z);
           end
         else
           q1:=z;
         if q<0 then
           begin
              sign:=not(sign);
              q2:=qword(-q);
           end
         else
           q2:=q;

          { is coded by the compiler as call to divqword }
          q3:=q1 div q2;

         if sign then
           divint64:=-q3
         else
           divint64:=q3;
      end;

    { multiplies two qwords }
    function mulqword(f1,f2 : qword;checkoverflow : boolean) : qword;safecall;

      var
         res,bitpos : qword;
         l : longint;

      begin
         res:=0;
         bitpos:=1;

         { we can't write currently qword constants directly :( }
         bitpos64:=1 shl 63;

         for l:=0 to 63 do
           begin
              if (f2 and bitpos)<>0 then
              if checkoverflow then
{$Q+}                
                res:=res+f1
{$Q-}
              else
                res:=res+f1;

              if ((f1 and bitpos64)<>0) and checkoverflow then
                int_overflow;

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
    function mulint64(f1,f2 : int64;checkoverflow : boolean) : int64;safecall;

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
           q3:=q1*q2

         if sign then
           mulint64:=-q3
         else
           mulint64:=q3;
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
  Revision 1.3  1999-05-23 20:27:27  florian
    + routines for qword div and mod

  Revision 1.2  1999/01/06 12:25:03  florian
    * naming for str(...) routines inserted
    * don't know what in int64 changed

  Revision 1.1  1998/12/12 12:15:41  florian
    + first implementation

}
