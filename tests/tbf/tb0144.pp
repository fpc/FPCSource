{ %FAIL }

{ OpenString with high should not be allowed }
program tb0144;

procedure TestOpen(var s: OpenString); cdecl;
var
 b: byte;
begin
 b:=high(s);
end;



Begin
end.

{
   $Log: tb0144.pp,v $
   Revision 1.2  2005/02/14 17:13:35  peter
     * truncate log

}
