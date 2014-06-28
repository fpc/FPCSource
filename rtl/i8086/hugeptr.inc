{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2014 by the Free Pascal development team.

    Processor dependent implementation for the system unit for
    intel i8086+

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

function fpc_hugeptr_add_longint(p: HugePointer; n: LongInt): HugePointer; compilerproc;
var
  o: LongInt;
begin
  o:=LongInt(Ofs(p^))+n;
  fpc_hugeptr_add_longint:=HugePointer(Ptr(Seg(p^)+SelectorInc*SmallInt(o shr 16),o and $FFFF));
end;

function fpc_hugeptr_add_longint_normalized(p: HugePointer; n: LongInt): HugePointer; compilerproc;
var
  linear: LongInt;
begin
  linear:=(LongInt(Seg(p^)) shl 4)+Ofs(p^)+n;
  fpc_hugeptr_add_longint_normalized:=HugePointer(Ptr(linear shr 4,linear and $F));
end;

function fpc_hugeptr_sub_longint(p: HugePointer; n: LongInt): HugePointer; compilerproc;
var
  o: LongInt;
begin
  o:=LongInt(Ofs(p^))-n;
  fpc_hugeptr_sub_longint:=HugePointer(Ptr(Seg(p^)+SelectorInc*SmallInt(o shr 16),o and $FFFF));
end;

function fpc_hugeptr_sub_longint_normalized(p: HugePointer; n: LongInt): HugePointer; compilerproc;
var
  linear: LongInt;
begin
  linear:=(LongInt(Seg(p^)) shl 4)+Ofs(p^)-n;
  fpc_hugeptr_sub_longint_normalized:=HugePointer(Ptr(linear shr 4,linear and $F));
end;

procedure fpc_hugeptr_inc_longint(var p: HugePointer; n: LongInt); compilerproc;
var
  o: LongInt;
begin
  o:=LongInt(Ofs(p^))+n;
  p:=HugePointer(Ptr(Seg(p^)+SelectorInc*SmallInt(o shr 16),o and $FFFF));
end;

procedure fpc_hugeptr_inc_longint_normalized(var p: HugePointer; n: LongInt); compilerproc;
var
  linear: LongInt;
begin
  linear:=(LongInt(Seg(p^)) shl 4)+Ofs(p^)+n;
  p:=HugePointer(Ptr(linear shr 4,linear and $F));
end;

procedure fpc_hugeptr_dec_longint(var p: HugePointer; n: LongInt); compilerproc;
var
  o: LongInt;
begin
  o:=LongInt(Ofs(p^))-n;
  p:=HugePointer(Ptr(Seg(p^)+SelectorInc*SmallInt(o shr 16),o and $FFFF));
end;

procedure fpc_hugeptr_dec_longint_normalized(var p: HugePointer; n: LongInt); compilerproc;
var
  linear: LongInt;
begin
  linear:=(LongInt(Seg(p^)) shl 4)+Ofs(p^)-n;
  p:=HugePointer(Ptr(linear shr 4,linear and $F));
end;
