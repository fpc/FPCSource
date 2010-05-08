{ %interactive }

{ the bug was that this put the 'x' at the *end* of the second line instead
  of on position 14 }

uses crt; // my terminal is 80x25
var s:string;

begin
clrscr;
s:='';
gotoxy(1,2); // you need this row
while length(s)<80 do s:=s+' ';
write(s);
gotoxy(14,2); // you need this row
write('x'); // position of 'x' is wrong
end.
