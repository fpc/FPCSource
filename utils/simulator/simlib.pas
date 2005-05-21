{
    This file is part of the Free Pascal simulator environment
    Copyright (c) 1999-2000 by Florian Klaempfl

    This unit implemements routines for data types which aren't
    support by commonly used compilers

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$N+}
{ we do some strange things here }
{$O-}
{$R-}
unit simlib;

  interface

    uses
       simbase;

    procedure byte_zap(q : qword;b : byte;var r : qword);

    { shifts q b bytes left }
    procedure shift_left_q(q : qword;b : byte;var r : qword);

    { shifts q b bytes right }
    procedure shift_right_q(q : qword;b : byte;var r : qword);

    { returns true if i1<i2 assuming that c1 and c2 are unsigned !}
    function ltu(c1,c2 : qword) : boolean;

    { returns true if i1=<i2 assuming that c1 and c2 are unsigned !}
    function leu(c1,c2 : qword) : boolean;

    { adds to owords, returns true if an overflow occurs }
    function addoword(o1,o2 : oword;var r : oword) : boolean;

    { adds two words, returns true if an overflow occurs }
    function addword(w1,w2 : word;var r : word) : boolean;

    { sets an oword to zero }
    procedure zerooword(var o : oword);

    { multiplies two qwords into a full oword }
    procedure mulqword(q1,q2 : qword;var r : oword);

  implementation

    procedure byte_zap(q : qword;b : byte;var r : qword);

      var
         i : tindex;

      begin
         for i:=0 to 7 do
           if ((1 shl i) and b)=0 then
             tqwordrec(r).bytes[i]:=tqwordrec(q).bytes[i]
           else
             tqwordrec(r).bytes[i]:=0;
      end;

    { shifts q b bytes left }
    procedure shift_left_q(q : qword;b : byte;var r : qword);

      var
         i : tindex;

      begin
         r:=0;
         if b>63 then
         else if b>31 then
           tqwordrec(r).high32:=tqwordrec(q).low32 shl (b-32)
         else
           begin
              { bad solution! A qword shift would be nice! }
              r:=q;
              for i:=1 to b do
                begin
                   tqwordrec(r).high32:=tqwordrec(r).high32 shl 1;
                   if (tqwordrec(r).low32 and $80000000)<>0 then
                     tqwordrec(r).high32:=tqwordrec(r).high32 or 1;
                   tqwordrec(r).low32:=tqwordrec(r).low32 shl 1;
                end;
           end;
      end;

    { shifts q b bytes right }
    procedure shift_right_q(q : qword;b : byte;var r : qword);

      var
         i : tindex;

      begin
         r:=0;
         if b>63 then
         else if b>31 then
           tqwordrec(r).low32:=tqwordrec(q).high32 shr (b-32)
         else
           begin
              { bad solution! A qword shift would be nice! }
              r:=q;
              for i:=1 to b do
                begin
                   tqwordrec(r).low32:=tqwordrec(r).low32 shr 1;
                   if (tqwordrec(r).high32 and 1)<>0 then
                     tqwordrec(r).low32:=tqwordrec(r).low32 or
                       $80000000;
                   tqwordrec(r).high32:=tqwordrec(r).high32 shr 1;
                end;
           end;
      end;

    { returns true if i1<i2 assuming that c1 and c2 are unsigned !}
    function ltu(c1,c2 : qword) : boolean;

      begin
         if (c1>=0) and (c2>=0) then
           ltu:=c1<c2
         else if (c1<0) and (c2>=0) then
           ltu:=false
         else if (c1>=0) and (c2<0) then
           ltu:=true
         else
           ltu:=c1<c2
      end;

    { returns true if i1=<i2 assuming that c1 and c2 are unsigned !}
    function leu(c1,c2 : qword) : boolean;

      begin
         if (c1>=0) and (c2>=0) then
           leu:=c1<=c2
         else if (c1<0) and (c2>=0) then
           leu:=false
         else if (c1>=0) and (c2<0) then
           leu:=true
         else
           leu:=c1<=c2
      end;

    { "ands" two qwords }
    procedure andqword(w1,w2 : qword;var r : qword);

      begin
         tqwordrec(r).low32:=tqwordrec(w1).low32 and tqwordrec(w2).low32;
         tqwordrec(r).high32:=tqwordrec(w1).high32 and tqwordrec(w2).high32;
      end;

    { adds two words, returns true if an overflow occurs }
    function addword(w1,w2 : word;var r : word) : boolean;

      var
         l : longint;

      begin
         l:=w1+w2;
         addword:=(l and $10000)<>0;
         r:=l and $ffff;
      end;

    { adds two owords, returns true if an overflow occurs }
    function addoword(o1,o2 : oword;var r : oword) : boolean;

      var
         i : tindex;
         carry : word;

      begin
         carry:=0;
         for i:=0 to 7 do
           begin
              r[i]:=o1[i]+o2[i]+carry;
              { an overflow has occured, if the r is less
                than one of the summands
              }
              if (r[i]<o1[i]) or (r[i]<o2[i]) then
                carry:=1
              else
                carry:=0;
           end;
         addoword:=carry=1;
      end;

    { sets an oword to zero }
    procedure zerooword(var o : oword);

      begin
         fillchar(o,sizeof(o),0);
      end;

    { multiplies two qwords into a full oword }
    procedure mulqword(q1,q2 : qword;var r : oword);

      var
         i : tindex;
         h,bitpos : qword;
         ho1 : oword;

      begin
         { r is zero }
         zerooword(ho1);
         r:=ho1;
         towordrec(ho1).low64:=q1;

         bitpos:=1;

         for i:=0 to 63 do
           begin
              andqword(q2,bitpos,h);
              if h<>0 then
                addoword(r,ho1,r);

              { ho1:=2*ho1 }
              addoword(ho1,ho1,ho1);
              shift_left_q(bitpos,1,bitpos);
           end;
      end;

end.
