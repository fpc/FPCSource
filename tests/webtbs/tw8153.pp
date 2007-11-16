{ %cpu=i386 }

{$ifdef fpc}
{$mode delphi}
{$endif}

type
  tc = class
    procedure v; virtual;
    procedure testintel; virtual;
    procedure testatt; virtual;
  end;

procedure tc.testintel; assembler; nostackframe;
asm
  mov ecx,[eax]
  jmp [ecx + vmtoffset tc.v]
end;

{$asmmode att}

procedure tc.testatt; assembler; nostackframe;
asm
  movl (%eax),%ecx
  jmpl +vmtoffset tc.v(%ecx)
end;

var
  l : longint;

procedure tc.v;
begin
  l := 5;
end;

var
  c: tc;
begin
  c := tc.create;
  c.testintel;
  if l <> 5 then
    halt(1);
  l := 0;
  c.testatt;
  if l <> 5 then
    halt(1);
  c.free;
end.

