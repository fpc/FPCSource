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

{
   $Log$
   Revision 1.1  2002-11-26 19:24:30  carl
     * some small fixes
     + added several new tests

}