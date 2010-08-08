{ %interactive }

{ should fill the rightmost column of the window with a yellow bar }

uses crt;

var
  i: longint;
begin
for i := 1 to SCREENHEIGHT do
begin
textbackground(YELLOW);
gotoxy(SCREENWIDTH, i);
write(' ');
end;
end.
