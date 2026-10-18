{ %opt=-O2 }

{ a 64-bit variable kept in registers, shifted by 32 and assigned to itself
  where its registers cannot follow the result: on a path that joins another }
program tb0751;

function shl32(a: qword; c: boolean): qword;
var
  x: qword;
begin
  x:=a;
  if c then
    x:=x shl 32;
  shl32:=x;
end;

function shl32i(a: int64; c: boolean): int64;
var
  x: int64;
begin
  x:=a;
  if c then
    x:=x shl 32;
  shl32i:=x;
end;

function shr32(a: qword; c: boolean): qword;
var
  x: qword;
begin
  x:=a;
  if c then
    x:=x shr 32;
  shr32:=x;
end;

function shl33(a: qword; c: boolean): qword;
var
  x: qword;
begin
  x:=a;
  if c then
    x:=x shl 33;
  shl33:=x;
end;

begin
  if shl32($12345678,true)<>qword($1234567800000000) then
    halt(1);
  if shl32i($12345678,true)<>int64($1234567800000000) then
    halt(2);
  if shr32(qword($1234567800000000),true)<>$12345678 then
    halt(3);
  if shl33($12345678,true)<>qword($2468ACF000000000) then
    halt(4);
  writeln('ok');
end.
