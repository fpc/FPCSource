{$mode objfpc}{$H+}

uses
  Classes, SysUtils;

var
  code: integer;

function bitstostring(b: TBits): string;
var
  i: integer;
  bs: string;
begin
  bs:= '';
  for i:= 0 to b.Size - 1 do
    if b[i] then
      bs:= bs + '1'
    else
      bs:= bs + '0';
  Result:= bs;
end;

procedure Test;
var
  i, bsize1, bsize2: integer;
  b1, b2, b3: TBits;
begin
  code := 0;
  // OR
  bsize1:= 16;
  bsize2:= 164;
  b1:= TBits.Create;
  b2:= TBits.Create(bsize2);
  b3:= TBits.Create(bsize2);
  for i:= 0 to bsize1 - 1 do
    b1[i]:= True;

  b2.OrBits(b1);
  b3.OrBits(b1);
  for i:= b1.Size to bsize2 - 1 do
    b3[i]:= False;
  if not b2.Equals(b3) then
  begin
    WriteLn('OR');
    writeln(bitstostring(b1));
    writeln(bitstostring(b2));
    writeln(bitstostring(b3));
    code := code or 1;
  end;
  b1.Free;
  b2.Free;
  b3.Free;

  // XOR
  bsize1:= 16;
  bsize2:= 164;
  b1:= TBits.Create;
  b2:= TBits.Create(bsize2);
  b3:= TBits.Create(bsize2);
  for i:= 0 to bsize1 - 1 do
    b1[i]:= True;

  b2.XOrBits(b1);
  b3.XOrBits(b1);
  for i:= b1.Size to bsize2 - 1 do
    b3[i]:= False;
  if not b2.Equals(b3) then
  begin
    WriteLn('XOR');
    writeln(bitstostring(b1));
    writeln(bitstostring(b2));
    writeln(bitstostring(b3));
    code := code or 2;
  end;
  b1.Free;
  b2.Free;
  b3.Free;

  // AND
  bsize1:= 16;
  bsize2:= 164;
  b1:= TBits.Create;
  b2:= TBits.Create(bsize2);
  b3:= TBits.Create(bsize2);
  for i:= 0 to bsize1 - 1 do
    b1[i]:= True;

  b2.AndBits(b1);
  b3.AndBits(b1);
  for i:= b1.Size to bsize2 - 1 do
    b3[i]:= False;
  if not b2.Equals(b3) then
  begin
    WriteLn('AND');
    writeln(bitstostring(b1));
    writeln(bitstostring(b2));
    writeln(bitstostring(b3));
    code := code or 4;
  end;
  b1.Free;
  b2.Free;
  b3.Free;
end;

begin
  test;
  halt(code);
end.


