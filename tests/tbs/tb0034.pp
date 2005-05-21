{ Old file: tbs0039.pp }
{  shows the else-else problem                         OK 0.9.9 (FK) }

VAR a : BYTE;
BEGIN
  a := 1;
  IF a=0 THEN
    IF a=1 THEN a:=2
    ELSE
  ELSE a:=3;        { "Illegal expression" }
END.
