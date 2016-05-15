{ %opt=-g-t }

program trashtest;

{$MODE OBJFPC}

type
        TTestRec = record
                Field1 : Int64;
        end;

operator := (i: TTestRec) fR: Int64;
begin
        fR := i.Field1;
end;

function TestFunc:TTestRec;
begin //error reported here
        TestFunc.Field1 := 1;
end;

begin
        WriteLn(TestFunc.Field1);
end.
