{ %CPU=x86_64 }
{ %OPT=-O4 }
{ tests the x86_64 peephole optimizations of 64-bit constants loaded into a
  register and used by and/or/xor/test/cmp }
program tmovimm64;

{$mode objfpc}

{$R-}
{$Q-}

var
  g: qword;

{ and $not(2^k) -> btr $k }
function ClearBit63(x: qword): qword; noinline;
begin
  Result := x and $7FFFFFFFFFFFFFFF;
end;

{ the flags are used, btr must not be used }
function ClearBit63Zero(x: qword): longint; noinline;
begin
  g := x and $7FFFFFFFFFFFFFFF;
  if g = 0 then
    Result := 1
  else
    Result := 2;
end;

{ or $2^k -> bts $k, also on memory }
procedure SetBit52(var x: qword); noinline;
begin
  x := x or $0010000000000000;
end;

{ xor $2^k -> btc $k }
function FlipBit40(x: qword): qword; noinline;
begin
  Result := x xor $0000010000000000;
end;

{ and $(2^k-1) -> shl/shr }
function LowMask(x: qword): qword; noinline;
begin
  Result := x and $000FFFFFFFFFFFFF;
end;

{ and $ffffffff -> andl }
function LowMask32(x: qword): qword; noinline;
begin
  Result := x and $FFFFFFFF;
end;

{ and $(2^k-1) with the result and the flags used }
function LowMaskZero(x: qword): longint; noinline;
begin
  g := x and $3FFFFFFFFFFFFFFF;
  if g <> 0 then
    Result := 1
  else
    Result := 2;
end;

{ and $(2^k-1) with only the flags used -> shl }
function LowMaskOnlyZero(x: qword): longint; noinline;
begin
  if (x and $3FFFFFFFFFFFFFFF) <> 0 then
    Result := 1
  else
    Result := 2;
end;

{ and $not(2^k-1) -> shr/shl }
function HighMask(x: qword): qword; noinline;
begin
  Result := x and $FFF0000000000000;
end;

{ and $2^63 with only the flags used -> shr }
function Sign(x: qword): boolean; noinline;
begin
  Result := (x and $8000000000000000) <> 0;
end;

{ several users of the flags }
function SignTwice(x: qword; var y: longint): longint; noinline;
begin
  if (x and $8000000000000000) = 0 then
    begin
      y := 9;
      Result := 1;
    end
  else
    Result := 2;
end;

{ test with a bit of memory -> bt }
function TestBit(const x: qword): boolean; noinline;
begin
  Result := (x and $0000020000000000) <> 0;
end;

{ cmp $2^63 -> neg }
function IsMinInt(x: int64): boolean; noinline;
begin
  Result := x = int64($8000000000000000);
end;

var
  q: qword;
  y: longint;
begin
  if ClearBit63($FFFFFFFFFFFFFFFF) <> $7FFFFFFFFFFFFFFF then
    halt(1);
  if ClearBit63Zero($8000000000000000) <> 1 then
    halt(2);
  if ClearBit63Zero($8000000000000001) <> 2 then
    halt(3);
  q := 1;
  SetBit52(q);
  if q <> $0010000000000001 then
    halt(4);
  if FlipBit40($0000010000000001) <> 1 then
    halt(5);
  if FlipBit40(1) <> $0000010000000001 then
    halt(6);
  if LowMask($FFFFFFFFFFFFFFFF) <> $000FFFFFFFFFFFFF then
    halt(7);
  if LowMask32($123456789ABCDEF0) <> $9ABCDEF0 then
    halt(8);
  if LowMaskZero($C000000000000000) <> 2 then
    halt(9);
  if (LowMaskZero($C000000000000001) <> 1) or (g <> 1) then
    halt(10);
  if LowMaskOnlyZero($C000000000000000) <> 2 then
    halt(11);
  if LowMaskOnlyZero($2000000000000000) <> 1 then
    halt(12);
  if HighMask($FFFFFFFFFFFFFFFF) <> $FFF0000000000000 then
    halt(13);
  if not Sign($8000000000000000) or Sign($7FFFFFFFFFFFFFFF) then
    halt(14);
  y := 0;
  if (SignTwice($8000000000000000, y) <> 2) or (y <> 0) then
    halt(15);
  if (SignTwice($7FFFFFFFFFFFFFFF, y) <> 1) or (y <> 9) then
    halt(16);
  if not TestBit($0000020000000000) or TestBit($FFFFFDFFFFFFFFFF) then
    halt(17);
  if not IsMinInt(int64($8000000000000000)) or IsMinInt(0) or IsMinInt(-1) then
    halt(18);
  writeln('ok');
end.
