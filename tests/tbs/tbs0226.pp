{$ifdef fpc}{$asmmode intel}{$endif}
var
  test : longint;
begin
  exit; { don't run this code below !! }
  asm
    dd    test
  end;
end.
