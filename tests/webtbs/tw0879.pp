PROGRAM TEST;
TYPE
        ta      = ARRAY[3..8] OF word;
VAR
        aa      : ^ta;
        i       : word;
BEGIN
   NEW(aa);
   FOR i:=LOW(aa^) TO HIGH(aa^) DO
     aa^[i]:=0;
END.
