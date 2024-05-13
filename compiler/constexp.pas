{
    Copyright (c) 2007 by Daniel Mantione

    This unit implements a Tconstexprint type. This type simulates an integer
    type that can handle numbers from low(int64) to high(qword) calculations.

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}
unit constexp;

{$i fpcdefs.inc}
{$modeswitch advancedrecords}

interface

{Avoid dependency on cpuinfo because the cpu directory isn't
 searched during utils building.}
{$ifdef GENERIC_CPU}
type  bestreal=extended;
{$else}
{$ifdef x86}
type  bestreal=extended;
{$else}
type  bestreal=double;
{$endif}
{$endif}

type  Tconstexprint=record
        function is_negative: boolean; inline;
        function extract_sign_abs(out abs: qword): boolean;
        procedure div_or_mod(const by: Tconstexprint; isdiv: boolean; out r: Tconstexprint);
        function tobestreal: bestreal;
      var
        overflow:boolean;
        case signed:boolean of
          false:
            (uvalue:qword);
          true:
            (svalue:int64);
      end;

operator := (const u:qword):Tconstexprint;inline;
operator := (const s:int64):Tconstexprint;inline;
operator := (const c:Tconstexprint):qword;
operator := (const c:Tconstexprint):int64;
operator := (const c:Tconstexprint):bestreal;

operator + (const a,b:Tconstexprint):Tconstexprint;
operator - (const a,b:Tconstexprint):Tconstexprint;
operator - (const a:Tconstexprint):Tconstexprint;
operator * (const a,b:Tconstexprint):Tconstexprint;
operator div (const a,b:Tconstexprint):Tconstexprint; inline;
operator mod (const a,b:Tconstexprint):Tconstexprint; inline;
operator / (const a,b:Tconstexprint):bestreal;

operator = (const a,b:Tconstexprint):boolean;
operator > (const a,b:Tconstexprint):boolean; inline; { Are reformulated using <. }
operator >= (const a,b:Tconstexprint):boolean; inline;
operator < (const a,b:Tconstexprint):boolean;
operator <= (const a,b:Tconstexprint):boolean; inline;

operator and (const a,b:Tconstexprint):Tconstexprint;
operator or (const a,b:Tconstexprint):Tconstexprint;
operator xor (const a,b:Tconstexprint):Tconstexprint;
operator shl (const a,b:Tconstexprint):Tconstexprint;
operator shr (const a,b:Tconstexprint):Tconstexprint;

function tostr(const i:Tconstexprint):shortstring;overload;

{****************************************************************************}
implementation
{****************************************************************************}

uses
  cutils;

function Tconstexprint.is_negative: boolean;
begin
  result:=signed and (svalue<0);
end;

{$push} {$q-,r-}
function Tconstexprint.extract_sign_abs(out abs: qword): boolean;
begin
  result:=is_negative;
  if result then
    abs:=qword(-svalue)
  else
    abs:=uvalue;
end;

procedure Tconstexprint.div_or_mod(const by: Tconstexprint; isdiv: boolean; out r: Tconstexprint);
var
  aa, bb: qword;
  negres: boolean;
begin
  if by.uvalue=0 then
    begin
      r:=qword(-int64(isdiv)); { Something. All ones if div, all zeros if mod. }
      r.overflow:=true;
      exit;
    end;
  { the sign of a modulo operation only depends on the sign of the
    dividend }
  negres:=self.extract_sign_abs(aa) xor by.extract_sign_abs(bb) and isdiv;
  r.overflow:=self.overflow or by.overflow;
  if isdiv then
    r.uvalue:=aa div bb
  else
    r.uvalue:=aa mod bb;
  r.signed:=negres or (r.svalue>=0);
  if negres then
    begin
      r.svalue:=-r.svalue;
      r.overflow:=r.overflow or (r.svalue>0); { Strictly > 0! }
    end;
end;
{$pop}

function Tconstexprint.tobestreal: bestreal;
begin
  if overflow then
    internalerrorproc(200706095);
  if signed then
    result:=svalue
  else
    result:=uvalue;
end;

operator := (const u:qword):Tconstexprint;

begin
  result.overflow:=false;
  result.signed:=false;
  result.uvalue:=u;
end;

operator := (const s:int64):Tconstexprint;

begin
  result.overflow:=false;
  result.signed:=true;
  result.svalue:=s;
end;

operator := (const c:Tconstexprint):qword;

begin
  if c.overflow then
    internalerrorproc(200706091);
  if c.is_negative then
    internalerrorproc(200706092);
  result:=c.uvalue;
end;

operator := (const c:Tconstexprint):int64;

begin
  if c.overflow then
    internalerrorproc(200706093);
  if not c.signed and (c.svalue<0) then
    internalerrorproc(200706094);
  result:=c.svalue;
end;

operator := (const c:Tconstexprint):bestreal;

begin
  if c.overflow then
    internalerrorproc(200706095);
  if c.signed then
    result:=c.svalue
  else
    result:=c.uvalue;
end;

{$push} {$q-,r-}
operator + (const a,b:Tconstexprint):Tconstexprint;

var aneg:boolean;

begin
  result.overflow:=a.overflow or b.overflow;
  result.uvalue:=a.uvalue+b.uvalue;
  aneg:=a.is_negative;
  if aneg<>b.is_negative then
    { Negative + positive: cannot overflow, signed if fits (here and below: “fits” means “positive value that fits into svalue”) or if positive operand did fit. }
    result.signed:=(result.svalue>=0) or (a.svalue xor b.svalue<0)
  else if aneg then
    begin
      { Negative + negative: overflow if positive, always signed. }
      result.overflow:=result.overflow or (result.svalue>=0);
      result.signed:=true;
    end
  else
    begin
      { Positive + positive: overflow if became less, signed if fits. }
      result.overflow:=result.overflow or (result.uvalue<a.uvalue);
      result.signed:=result.svalue>=0;
    end;
end;

operator - (const a,b:Tconstexprint):Tconstexprint;

var bneg:boolean;

begin
  result.overflow:=a.overflow or b.overflow;
  result.uvalue:=a.uvalue-b.uvalue;
  bneg:=b.is_negative;
  if a.is_negative then
    begin
      { Negative − negative: cannot overflow, always signed.
        Negative - positive: overflow if positive or b did not fit, always signed. }
      result.signed:=true;
      if not bneg then
        result.overflow:=result.overflow or (b.svalue<0) or (result.svalue>=0);
    end
  else if bneg then
    begin
      { Positive - negative: overflow if became less, signed if fits. }
      result.overflow:=result.overflow or (result.uvalue<a.uvalue);
      result.signed:=result.svalue>=0;
    end
  else
    begin
      { Positive − positive: overflow if a < b but result is positive, signed if a < b or fits. }
      result.overflow:=result.overflow or (a.uvalue<b.uvalue) and (result.svalue>=0);
      result.signed:=(a.uvalue<b.uvalue) or (result.svalue>=0);
    end;
end;

operator - (const a:Tconstexprint):Tconstexprint;

var aneg:boolean;

begin
  aneg:=a.is_negative;
  result.svalue:=-a.svalue;
  result.overflow:=a.overflow or not aneg and (result.svalue>0); { Will trigger on > -Low(int64). }
  result.signed:=not (aneg and (a.svalue=Low(a.svalue))); { Unsigned only if negating Low(int64). }
end;

operator * (const a,b:Tconstexprint):Tconstexprint;

var aa,bb:qword;
    negres:boolean;

begin
  negres:=a.extract_sign_abs(aa) xor b.extract_sign_abs(bb);
  result.uvalue:=aa*bb;
  result.overflow:=a.overflow or b.overflow or
    (Hi(aa) or Hi(bb)<>0) and { Pretest to avoid division in small cases. Must be cheaper than two BsrQWords. }
    (bb<>0) and (high(qword) div bb<aa);
  result.signed:=negres or (result.svalue>=0);
  if negres then
    begin
      result.overflow:=result.overflow or (result.svalue<0);
      result.svalue:=-result.svalue;
    end;
end;
{$pop}

operator div (const a,b:Tconstexprint):Tconstexprint;

begin
  a.div_or_mod(b,true,result);
end;

operator mod (const a,b:Tconstexprint):Tconstexprint;

begin
  a.div_or_mod(b,false,result);
end;

operator / (const a,b:Tconstexprint):bestreal;

begin
  result:=a.tobestreal/b.tobestreal;
end;

operator = (const a,b:Tconstexprint):boolean;

begin
  result:=(a.uvalue=b.uvalue) and (a.is_negative=b.is_negative);
end;

operator > (const a,b:Tconstexprint):boolean;

begin
  result:=b<a;
end;

operator >= (const a,b:Tconstexprint):boolean;

begin
  result:=not(a<b);
end;

operator < (const a,b:Tconstexprint):boolean;

begin
  result:=a.is_negative;
  if result=b.is_negative then
    result:=a.uvalue<b.uvalue; { Works both with positive < positive and unsigned(negative) < unsigned(negative). }
end;

operator <= (const a,b:Tconstexprint):boolean;

begin
  result:=not(b<a);
end;

operator and (const a,b:Tconstexprint):Tconstexprint;

begin
  result.overflow:=false;
  result.signed:=a.signed or b.signed;
  result.uvalue:=a.uvalue and b.uvalue;
end;

operator or (const a,b:Tconstexprint):Tconstexprint;

begin
  result.overflow:=false;
  result.signed:=a.signed or b.signed;
  result.uvalue:=a.uvalue or b.uvalue;
end;

operator xor (const a,b:Tconstexprint):Tconstexprint;

begin
  result.overflow:=false;
  result.signed:=a.signed or b.signed;
  result.uvalue:=a.uvalue xor b.uvalue;
end;

operator shl (const a,b:Tconstexprint):Tconstexprint;

begin
  if b.uvalue>=bitsizeof(a.uvalue) then
    exit(0);
  result.overflow:=false;
  result.signed:=a.signed; { signed(1) shl 63 does not fit into signed }
  result.uvalue:=a.uvalue shl b.uvalue;
end;

operator shr (const a,b:Tconstexprint):Tconstexprint;

begin
  if b.uvalue>=bitsizeof(a.uvalue) then
    exit(0);
  result.overflow:=false;
  result.signed:=a.signed;
  result.uvalue:=a.uvalue shr b.uvalue;
end;

function tostr(const i:Tconstexprint):shortstring;overload;

begin
  if i.signed then
    str(i.svalue,result)
  else
    str(i.uvalue,result);
end;

end.
