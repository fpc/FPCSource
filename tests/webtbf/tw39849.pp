{ %FAIL }

PROGRAM tw39849;
   {$APPTYPE CONSOLE}
VAR
   F: TEXT;
BEGIN
   ASSIGN(F, 'doublepoint.txt');
   REWRITE(F);
   WRITELN(F, 'Hello');   { ',' is legal - compiler just do its job }
   WRITELN(F: 'Hello');   { ':' is not legal - compiler should emit an error, but 3.2.2 fail to }
   CLOSE(F);
END.

