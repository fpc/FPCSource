{ %CPU=i386 }
{ Old file: tbs0226.pp }
{ Asm, offset of var is not allowed as constant        OK 0.99.11 (PFV) }

{$ifdef fpc}{$asmmode intel}{$endif}
var
  test : longint;
begin
  exit; { don't run this code below !! }
  asm
    dd    test
  end;
end.
