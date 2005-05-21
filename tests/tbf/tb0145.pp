{ %FAIL }

{ This should fail compilation because open parameters are not
  allowed with cdecl'ed routines.
}

procedure TestOpen(var s: array of byte); cdecl;
var
 b: byte;
begin
 b:=high(s);
end;



Begin
end.
