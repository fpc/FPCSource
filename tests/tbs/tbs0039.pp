VAR a : BYTE;
BEGIN
  a := 1;
  IF a=0 THEN
    IF a=1 THEN a:=2
    ELSE
  ELSE a:=3;        { "Illegal expression" }
END.


