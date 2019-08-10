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
  // XOR
  bsize1:= 16;
  bsize2:= 164;
  b1:= TBits.Create;
  b2:= TBits.Create(bsize2);
  b3:= TBits.Create(bsize2);
  for i:= 0 to bsize1 - 1 do
    begin
      b1[i]:= True;
      b2[i]:= True;
    end;

  b2.NotBits(b1);
  if not b2.Equals(b3) then
  begin
    WriteLn('Not');
    writeln(bitstostring(b1));
    writeln(bitstostring(b2));
    writeln(bitstostring(b3));
    code := code or 1;
  end;
  b1.Free;
  b2.Free;
  b3.Free;

  b1:= TBits.Create;
  b2:= TBits.Create(bsize2);
  b3:= TBits.Create(bsize2);
  for i:= 0 to bsize1 - 1 do
    begin
      b1[i]:= True;
      b3[i]:= True;
    end;

  b2.NotBits(b1);
  if not b2.Equals(b3) then
  begin
    WriteLn('Not');
    writeln(bitstostring(b1));
    writeln(bitstostring(b2));
    writeln(bitstostring(b3));
    code := code or 2;
  end;
  b1.Free;
  b2.Free;
  b3.Free;

  b1:= TBits.Create;
  b2:= TBits.Create(bsize2);
  b3:= TBits.Create(bsize2);
  for i:= 0 to bsize1 - 1 do
    begin
      b2[i]:= True;
      b3[i]:= True;
    end;

  b2.NotBits(b1);
  if not b2.Equals(b3) then
  begin
    WriteLn('Not');
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
  if code=0 then
    writeln('ok');
  halt(code);
end.


