{ %INTERACTIVE }
{ checking of invalid drive size }

USES DOS;
var
  d : longint;
BEGIN
   {$I-}
   d:=DISKSIZE(1);
   writeln(d);
   WRITELN(IORESULT);
   {$I+}
   if d<>-1 then
    halt(1);
END.
