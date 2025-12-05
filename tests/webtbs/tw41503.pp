{ %NORUN }

program tw41503;
{$mode objFPC}

procedure kek; public; cdecl;
begin

end;

procedure kek0;              cdecl; varargs; external {'libkek'} name 'kek';
procedure kek1(dummy: byte); cdecl; varargs; external {'libkek'} name 'kek';

begin
 kek1(0,1,2,3);  // OK \u2705
 kek0;           // OK \u2705
 kek0(0,1,2,3);  // IE \U0001f41e
end.

