{ %norun }

{$inline on}
program TEST;
{$EXTENDEDSYNTAX OFF}
VAR
   IO: WORD; { or LONGINT if wanted }

function test: word; inline;
begin
  test:=1;
  writeln(io);
end;

BEGIN
   IO:=test;
END.
