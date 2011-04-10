{
   This file is part of the Free Pascal FCL library.
   BSD parts (c) 2011 Vlado Boza

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY;without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

**********************************************************************}
{$mode objfpc}

unit gutil;

interface

type generic TLess<T>=class
  class function c(a,b:T):boolean;inline;
end;

type generic TGreater<T>=class
  class function c(a,b:T):boolean;inline;
end;

implementation

class function TLess.c(a,b:T):boolean;inline;
begin
  c:=a<b;
end;

class function TGreater.c(a,b:T):boolean;inline;
begin
  c:=b<a;
end;

end.
