TYPE Rec = RECORD
             x:WORD;
             y:WORD;
           END;
 
VAR s:WORD;
    r:Rec;
 
CONST p1:POINTER = @s;    { Works fine }
      p2:POINTER = @R.x;  { illegal expression }
 
BEGIN
END.

