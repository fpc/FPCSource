{ %OPT=-O2 }
Program Ts;

Procedure ShortTest;
VAR
   a: byte;
   b: shortint absolute a;
BEGIN
  for a := 0 to 255 do 
    begin
      writeln (a:4,b:5);
      if (a>127) and (a=b) then
        halt(1);
    end;
END;

BEGIN
  Shorttest
END.
