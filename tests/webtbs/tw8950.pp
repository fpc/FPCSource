{ %cpu=i386 }

{$ifdef fpc}
{$mode delphi}
{$endif}

procedure SetBitBuffer(var Value; const Bit: Cardinal);
asm
  BTS    [Value], Bit
end;

var
  l: longint;
begin
  l := 0;
  setbitbuffer(l,5);
  if (l <> 32) then
    halt(1);
end.
