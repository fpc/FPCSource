PROGRAM t9;

PROCEDURE Eeep;
VAR
   X: BYTE;
   NewNG: STRING;
PROCEDURE SubProc;
   BEGIN
      newng := 'alt';
      FOR X := 1 TO LENGTH(NewNG) DO BEGIN
         WRITELN(X);
   END;
END;
BEGIN
   SubProc;
END;

BEGIN
        Eeep;
END.

