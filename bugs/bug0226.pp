{$ifdef fpc}{$asmmode intel}{$endif}
var
  test : longint;
begin
  asm
    dd    test
  end;
end.
