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
