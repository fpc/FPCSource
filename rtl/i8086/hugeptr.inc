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

function fpc_hugeptr_cmp_normalized_e(p1, p2: HugePointer): Boolean; compilerproc;
begin
  fpc_hugeptr_cmp_normalized_e:=((LongInt(Seg(p1^)) shl 4)+Ofs(p1^))=((LongInt(Seg(p2^)) shl 4)+Ofs(p2^));
end;

function fpc_hugeptr_cmp_normalized_ne(p1, p2: HugePointer): Boolean; compilerproc;
begin
  fpc_hugeptr_cmp_normalized_ne:=((LongInt(Seg(p1^)) shl 4)+Ofs(p1^))<>((LongInt(Seg(p2^)) shl 4)+Ofs(p2^));
end;

function fpc_hugeptr_cmp_normalized_b(p1, p2: HugePointer): Boolean; compilerproc;
begin
  fpc_hugeptr_cmp_normalized_b:=((LongInt(Seg(p1^)) shl 4)+Ofs(p1^))<((LongInt(Seg(p2^)) shl 4)+Ofs(p2^));
end;

function fpc_hugeptr_cmp_normalized_be(p1, p2: HugePointer): Boolean; compilerproc;
begin
  fpc_hugeptr_cmp_normalized_be:=((LongInt(Seg(p1^)) shl 4)+Ofs(p1^))<=((LongInt(Seg(p2^)) shl 4)+Ofs(p2^));
end;

function fpc_hugeptr_cmp_normalized_a(p1, p2: HugePointer): Boolean; compilerproc;
begin
  fpc_hugeptr_cmp_normalized_a:=((LongInt(Seg(p1^)) shl 4)+Ofs(p1^))>((LongInt(Seg(p2^)) shl 4)+Ofs(p2^));
end;

function fpc_hugeptr_cmp_normalized_ae(p1, p2: HugePointer): Boolean; compilerproc;
begin
  fpc_hugeptr_cmp_normalized_ae:=((LongInt(Seg(p1^)) shl 4)+Ofs(p1^))>=((LongInt(Seg(p2^)) shl 4)+Ofs(p2^));
end;
