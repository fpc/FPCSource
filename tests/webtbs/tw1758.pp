{ %interactive }

uses crt;

begin
clrscr;
Window(1,2,80,3); {this does not work; if you set the last parameter to 4 (=3 lines) everything is fine}
gotoxy(1,1); write('clear now ? (this is only to fill some text...)');
readkey;
clrscr;
readkey;
write('cleared !');
readkey;

end.
