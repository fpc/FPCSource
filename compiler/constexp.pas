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

interface

{ bootstrapping with 2.0.x }
{$ifdef VER2_0}
  {$Q-}
  {$R-}
{$endif}

{$ifopt q+}
  {$define ena_q}
{$endif}

type  Tconstexprint=record
        overflow:boolean;
        case signed:boolean of
          false:
            (uvalue:qword);
          true:
            (svalue:int64);
      end;

      errorproc=procedure (i:longint);

{"Uses verbose" gives a dependency on cpuinfo through globals. This leads
 build trouble when compiling the directory utils, since the cpu directory
 isn't searched there. Therefore we use a procvar and make verbose install
 the errorhandler. A dependency from verbose on this unit is no problem.}
var   internalerror:errorproc;

{Same issue, avoid dependency on cpuinfo because the cpu directory isn't
 searched during utils building.}
{$ifdef x86}
type  bestreal=extended;
{$else}
type  bestreal=double;
{$endif}

operator := (const u:qword):Tconstexprint;inline;
operator := (const s:int64):Tconstexprint;inline;
operator := (const c:Tconstexprint):qword;
operator := (const c:Tconstexprint):int64;
operator := (const c:Tconstexprint):bestreal;

operator + (const a,b:Tconstexprint):Tconstexprint;
operator - (const a,b:Tconstexprint):Tconstexprint;
operator - (const a:Tconstexprint):Tconstexprint;
operator * (const a,b:Tconstexprint):Tconstexprint;
operator div (const a,b:Tconstexprint):Tconstexprint;
operator mod (const a,b:Tconstexprint):Tconstexprint;
operator / (const a,b:Tconstexprint):bestreal;

operator = (const a,b:Tconstexprint):boolean;
operator > (const a,b:Tconstexprint):boolean;
operator >= (const a,b:Tconstexprint):boolean;
operator < (const a,b:Tconstexprint):boolean;
operator <= (const a,b:Tconstexprint):boolean;

operator and (const a,b:Tconstexprint):Tconstexprint;
operator or (const a,b:Tconstexprint):Tconstexprint;
operator xor (const a,b:Tconstexprint):Tconstexprint;
operator shl (const a,b:Tconstexprint):Tconstexprint;
operator shr (const a,b:Tconstexprint):Tconstexprint;

function tostr(const i:Tconstexprint):shortstring;overload;

{****************************************************************************}
implementation
{****************************************************************************}



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
    internalerror(200706091)
  else if not c.signed then
    result:=c.uvalue
  else if c.svalue<0 then
    internalerror(200706092)
  else
    result:=qword(c.svalue);
end;

operator := (const c:Tconstexprint):int64;

begin
  if c.overflow then
    internalerror(200706093)
  else if c.signed then
    result:=c.svalue
  else if c.uvalue>high(int64) then
    internalerror(200706094)
  else
    result:=int64(c.uvalue);
end;

operator := (const c:Tconstexprint):bestreal;

begin
  if c.overflow then
    internalerror(200706095)
  else if c.signed then
    result:=c.svalue
  else
    result:=c.uvalue;
end;

function add_to(const a:Tconstexprint;b:qword):Tconstexprint;

var sspace,uspace:qword;

label try_qword;

begin
  result.overflow:=false;

  {Try if the result fits in an int64.}
  if (a.signed) and (a.svalue<0) then
    {$Q-}
    sspace:=qword(high(int64))-qword(-a.svalue)
    {$ifdef ena_q}{$Q+}{$endif}
  else if not a.signed and (a.uvalue>qword(high(int64))) then
    goto try_qword
  else
    sspace:=qword(high(int64))-a.svalue;

  if sspace>=b then
    begin
      result.signed:=true;
      {$Q-}
      result.svalue:=a.svalue+int64(b);
      {$ifdef ena_q}{$Q+}{$endif}
      exit;
    end;

  {Try if the result fits in a qword.}
try_qword:
  if (a.signed) and (a.svalue<0) then
    uspace:=high(qword)-qword(-a.svalue)
{  else if not a.signed and (a.uvalue>qword(high(int64))) then
    uspace:=high(qword)-a.uvalue}
  else
    uspace:=high(qword)-a.uvalue;
  if uspace>=b then
    begin
      result.signed:=false;
      {$Q-}
      result.uvalue:=a.uvalue+b;
      {$ifdef ena_q}{$Q+}{$endif}
      exit;
    end;
  result.overflow:=true;
end;

function sub_from(const a:Tconstexprint;b:qword):Tconstexprint;

const abs_low_int64=qword(9223372036854775808);   {abs(low(int64)) -> overflow error}

var sspace:qword;

label try_qword,ov;

begin
  result.overflow:=false;

  {Try if the result fits in an int64.}
  if (a.signed) and (a.svalue<0) then
    {$Q-}
    sspace:=qword(a.svalue)+abs_low_int64
    {$ifdef ena_q}{$Q+}{$endif}
  else if not a.signed and (a.uvalue>qword(high(int64))) then
    goto try_qword
  else
    sspace:=a.uvalue+qword(abs(low(int64)));
  if sspace>=b then
    begin
      result.signed:=true;
      {$Q-}
      result.svalue:=a.svalue-int64(b);
      {$ifdef ena_q}{$Q+}{$endif}
      exit;
    end;

  {Try if the result fits in a qword.}
try_qword:
  if not(a.signed and (a.svalue<0)) and (a.uvalue>=b) then
    begin
      result.signed:=false;
      {$Q-}
      result.uvalue:=a.uvalue-b;
      {$ifdef ena_q}{$Q+}{$endif}
      exit;
    end;
ov:
  result.overflow:=true;
end;

operator + (const a,b:Tconstexprint):Tconstexprint;

begin
  if a.overflow or b.overflow then
    begin
      result.overflow:=true;
      exit;
    end;
  if b.signed and (b.svalue<0) then
    {$Q-}
    result:=sub_from(a,qword(-b.svalue))
    {$ifdef ena_q}{$Q+}{$endif}
  else
    result:=add_to(a,b.uvalue);
end;

operator - (const a,b:Tconstexprint):Tconstexprint;

begin
  if a.overflow or b.overflow then
    begin
      result.overflow:=true;
      exit;
    end;
  if b.signed and (b.svalue<0) then
    {$Q-}
    result:=add_to(a,qword(-b.svalue))
    {$ifdef ena_q}{$Q+}{$endif}
  else
    result:=sub_from(a,b.uvalue);
end;

operator - (const a:Tconstexprint):Tconstexprint;

begin
  if not a.signed and (a.uvalue>qword(high(int64))) then
    result.overflow:=true
  else
    begin
      result.overflow:=false;
      result.signed:=true;
      result.svalue:=-a.svalue;
    end;
end;


operator * (const a,b:Tconstexprint):Tconstexprint;

var aa,bb,r:qword;
    sa,sb:boolean;

begin
  if a.overflow or b.overflow then
    begin
      result.overflow:=true;
      exit;
    end;
  result.overflow:=false;
  sa:=a.signed and (a.svalue<0);
  if sa then
     aa:=qword(-a.svalue)
  else
    aa:=a.uvalue;
  sb:=b.signed and (b.svalue<0);
  if sb then
    bb:=qword(-b.svalue)
  else
    bb:=b.uvalue;

  if (bb<>0) and (high(qword) div bb<aa) then
    result.overflow:=true
  else
    begin
      r:=aa*bb;
      if sa xor sb then
        begin
          result.signed:=true;
          if r>qword(high(int64)) then
            result.overflow:=true
          else
            result.svalue:=-int64(r);
        end
      else
        begin
          result.signed:=false;
          result.uvalue:=r;
        end;
    end;
end;

operator div (const a,b:Tconstexprint):Tconstexprint;

var aa,bb,r:qword;
    sa,sb:boolean;

begin
  if a.overflow or b.overflow then
    begin
      result.overflow:=true;
      exit;
    end;
  result.overflow:=false;
  sa:=a.signed and (a.svalue<0);
  if sa then
    {$Q-}
    aa:=qword(-a.svalue)
    {$ifdef ena_q}{$Q+}{$endif}
  else
    aa:=a.uvalue;
  sb:=b.signed and (b.svalue<0);
  if sb then
    {$Q-}
    bb:=qword(-b.svalue)
    {$ifdef ena_q}{$Q+}{$endif}
  else
    bb:=b.uvalue;

  if bb=0 then
    result.overflow:=true
  else
    begin
      r:=aa div bb;
      if sa xor sb then
        begin
          result.signed:=true;
          if r>qword(high(int64)) then
            result.overflow:=true
          else
            result.svalue:=-int64(r);
        end
      else
        begin
          result.signed:=false;
          result.uvalue:=r;
        end;
    end;
end;

operator mod (const a,b:Tconstexprint):Tconstexprint;

var aa,bb:qword;

begin
  if a.overflow or b.overflow then
    begin
      result.overflow:=true;
      exit;
    end;
  result.overflow:=false;
  if a.signed then
    aa:=qword(a.svalue)
  else
    aa:=a.uvalue;
  if b.signed then
    bb:=qword(b.svalue)
  else
    bb:=b.uvalue;
  if bb=0 then
    result.overflow:=true
  else
    begin
      result.signed:=false;
      result.uvalue:=aa mod bb;
    end;
end;

operator / (const a,b:Tconstexprint):bestreal;

var aa,bb:bestreal;

begin
  if a.overflow or b.overflow then
    internalerror(200706096);
  if a.signed then
    aa:=a.svalue
  else
    aa:=a.uvalue;
  if b.signed then
    bb:=b.svalue
  else
    bb:=b.uvalue;
  result:=aa/bb;
end;

operator = (const a,b:Tconstexprint):boolean;

begin
  if a.signed and (a.svalue<0) then
    if b.signed and (b.svalue<0) then
      result:=a.svalue=b.svalue
    else if b.uvalue>qword(high(int64)) then
      result:=false
    else
      result:=a.svalue=b.svalue
  else
    if not (b.signed and (b.svalue<0)) then
      result:=a.uvalue=b.uvalue
    else if a.uvalue>qword(high(int64)) then
      result:=false
    else
      result:=a.svalue=b.svalue
end;

operator > (const a,b:Tconstexprint):boolean;

begin
  if a.signed and (a.svalue<0) then
    if b.signed and (b.svalue<0) then
      result:=a.svalue>b.svalue
    else if b.uvalue>qword(high(int64)) then
      result:=false
    else
      result:=a.svalue>b.svalue
  else
    if not (b.signed and (b.svalue<0)) then
      result:=a.uvalue>b.uvalue
    else if a.uvalue>qword(high(int64)) then
      result:=true
    else
      result:=a.svalue>b.svalue
end;

operator >= (const a,b:Tconstexprint):boolean;

begin
  if a.signed and (a.svalue<0) then
    if b.signed and (b.svalue<0) then
      result:=a.svalue>=b.svalue
    else if b.uvalue>qword(high(int64)) then
      result:=false
    else
      result:=a.svalue>=b.svalue
  else
    if not (b.signed and (b.svalue<0)) then
      result:=a.uvalue>=b.uvalue
    else if a.uvalue>qword(high(int64)) then
      result:=true
    else
      result:=a.svalue>=b.svalue
end;

operator < (const a,b:Tconstexprint):boolean;

begin
  if a.signed and (a.svalue<0) then
    if b.signed and (b.svalue<0) then
      result:=a.svalue<b.svalue
    else if b.uvalue>qword(high(int64)) then
      result:=true
    else
      result:=a.svalue<b.svalue
  else
    if not (b.signed and (b.svalue<0)) then
      result:=a.uvalue<b.uvalue
    else if a.uvalue>qword(high(int64)) then
      result:=false
    else
      result:=a.svalue<b.svalue
end;

operator <= (const a,b:Tconstexprint):boolean;

begin
  if a.signed and (a.svalue<0) then
    if b.signed and (b.svalue<0) then
      result:=a.svalue<=b.svalue
    else if b.uvalue>qword(high(int64)) then
      result:=true
    else
      result:=a.svalue<=b.svalue
  else
    if not (b.signed and (b.svalue<0)) then
      result:=a.uvalue<=b.uvalue
    else if a.uvalue>qword(high(int64)) then
      result:=false
    else
      result:=a.svalue<=b.svalue
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
  result.overflow:=false;
  result.signed:=a.signed;
  result.uvalue:=a.uvalue shl b.uvalue;
end;

operator shr (const a,b:Tconstexprint):Tconstexprint;

begin
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
