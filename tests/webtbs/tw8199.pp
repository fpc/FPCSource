{ %cpu=i386 }
{$APPTYPE CONSOLE}

{$ifdef fpc}
  {$mode delphi}
{$endif}

program AsmDifference;

type
  TnxInt64 = Int64; {64-bit signed Integer}
  TnxValueRelationship = -1..1;

const
  nxSmallerThan = Low(TnxValueRelationship);
  nxEqual = 0;
  nxGreaterThan = High(TnxValueRelationship);

function nxCmpI64(const a, b : TnxInt64) : TnxValueRelationship;
//begin
// if a = b then
// Result := nxEqual
// else if a < b then
// Result := nxSmallerThan
// else
// Result := nxGreaterThan;
//end;
asm
  xor eax, eax
  mov edx, [ebp+20]
  cmp edx, [ebp+12]
  jg @@GT
  jl @@LT
  mov edx, [ebp+16]
  cmp edx, [ebp+8]
  ja @@GT
  je @@EQ
@@LT:
  dec eax
  dec eax
@@GT:
  inc eax
@@EQ:
end;

var
  a, b: TnxInt64;

begin
  a := 12884901889;
  b := 12884901890;

  if nxCmpI64(a, b)<>-1 then
    halt(1);
  writeln('ok');
end.
