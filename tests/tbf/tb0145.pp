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
   $Log: tb0145.pp,v $
   Revision 1.2  2005/02/14 17:13:35  peter
     * truncate log

}
