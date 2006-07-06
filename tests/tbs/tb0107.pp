{ Old file: tbs0125.pp }
{ wrong colors with DOS CRT unit                        OK 0.99.6 (PFV) }

{ %skiptarget=wince }

uses
crt;
var
i:integer;
begin
clrscr;
textcolor(blue);
writeln('ole');
textcolor(red);
writeln('rasmussen');
writeln(i);
end.
