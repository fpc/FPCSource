program tw22133;

{$mode objfpc}{$H+}

type
  uint64 = qword;

var
  T64:UInt64;

//force checking constants in compile-time
{$RANGECHECKS ON}

{$inline on}

function testshift(a:uint64; b: byte): uint64; inline;
begin
  result:=a shl b;
end;

begin
  T64:=UInt64(qword(1) shl 63);
  if T64<>uint64(high(int64)+1) then
    halt(1);
  T64:=UInt64(1) shl 63;
  if T64<>uint64(high(int64)+1) then
    halt(2);
  T64:=testshift(1,63);
  if T64<>uint64(high(int64)+1) then
    halt(3);
end.

