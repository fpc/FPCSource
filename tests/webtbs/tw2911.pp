{ %version=1.1 }
{ %opt=-gh }

{ Source provided for Free Pascal Bug Report 2911 }
{ Submitted by "Chris Hilder" on  2004-01-19 }
{ e-mail: cj.hilder@astronomyinyourhands.com }
program bug_demo;

{$ifdef fpc}{$Mode objfpc}{$endif}
{$LONGSTRINGS ON}

type
        RecordWithStrings =
                record
                        one,
                        two : string;
                end;

var
        onestring,
        twostring : string;
        ARecordWithStrings : RecordWithStrings;

function FunctionResultIsRecord(a : RecordWithStrings) : RecordWithStrings;
begin
        result := a;
end;

begin
        HaltOnNotReleased := true;
        onestring := 'one';
        twostring := 'two';
        ARecordWithStrings.one := onestring + twostring;
        twostring := onestring + twostring;
        ARecordWithStrings := FunctionResultIsRecord(ARecordWithStrings);
        twostring := onestring + twostring;
        ARecordWithStrings := FunctionResultIsRecord(ARecordWithStrings);
        twostring := onestring + twostring;
        ARecordWithStrings := FunctionResultIsRecord(ARecordWithStrings);
        twostring := onestring + twostring;
end.
