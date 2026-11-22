{ %CPU=i386,x86_64 }
{ %OPT=-O- -O2 }

{ 0.0 and -0.0 are different constants: the constant optimization of -O2
  must not load one of them for the other }

{$mode objfpc}
{$ifdef cpui386}
{$fputype sse2}
{$endif cpui386}

const
  NegZeroBits = qword($8000000000000000);
  NegZeroBitsSingle = dword($80000000);

var
  d: array[0..3] of double;
  s: array[0..3] of single;

procedure FillNegFirst;
begin
  d[0]:=-0.0;
  d[1]:=0.0;
  d[2]:=-0.0;
  d[3]:=0.0;
end;

procedure FillPosFirst;
begin
  d[0]:=0.0;
  d[1]:=-0.0;
  d[2]:=0.0;
  d[3]:=-0.0;
end;

procedure FillSingle;
begin
  s[0]:=-0.0;
  s[1]:=0.0;
  s[2]:=-0.0;
  s[3]:=0.0;
end;

function Bits(const aValue: double): qword;
begin
  Move(aValue,Result,SizeOf(Result));
end;

function SingleBits(const aValue: single): dword;
begin
  Move(aValue,Result,SizeOf(Result));
end;

begin
{$ifdef cpui386}
  if not(has_sse2_support) then
    begin
      writeln('CPU does not support SSE2, skipping test...');
      halt(0);
    end;
{$endif cpui386}
  FillNegFirst;
  if Bits(d[0])<>NegZeroBits then
    halt(1);
  if Bits(d[1])<>0 then
    halt(2);
  if Bits(d[2])<>NegZeroBits then
    halt(3);
  if Bits(d[3])<>0 then
    halt(4);
  FillPosFirst;
  if Bits(d[0])<>0 then
    halt(5);
  if Bits(d[1])<>NegZeroBits then
    halt(6);
  if Bits(d[2])<>0 then
    halt(7);
  if Bits(d[3])<>NegZeroBits then
    halt(8);
  FillSingle;
  if SingleBits(s[0])<>NegZeroBitsSingle then
    halt(9);
  if SingleBits(s[1])<>0 then
    halt(10);
  if SingleBits(s[2])<>NegZeroBitsSingle then
    halt(11);
  if SingleBits(s[3])<>0 then
    halt(12);
  writeln('ok');
end.
