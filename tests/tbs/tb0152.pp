{ Old file: tbs0182.pp }
{ @record.field doesn't work in constant expr           OK 0.99.9 (PM) }

TYPE Rec = RECORD
             x:WORD;
             y:WORD;
           END;

     Rec1 = Record
            x,y : longint;
            end;
     Rec2 = Record
            r,s : Rec1;
            z : word;
            end;
     plongint = ^longint;

VAR s:WORD;
    r:Rec;
    rr : Rec2;

CONST p1:POINTER = @s;    { Works fine }
      p2:POINTER = @R.y;  { illegal expression }
      p3:pointer = @rr.s.y;
      p4:plongint = @rr.s.y;
BEGIN
   rr.s.y:=15;
   if plongint(p3)^<>15 then
     Begin
        Writeln('Error : wrong code generated');
        Halt(1);
     End;
END.
