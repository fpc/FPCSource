{ %norun }

program TEST;
{$EXTENDEDSYNTAX OFF}
USES
  DOS;
VAR
  IO: WORD; { or LONGINT if wanted }
BEGIN
  IO:=IORESULT;
END.


