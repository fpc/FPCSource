{ %version=1.1 }

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

procedure RefCount(const s : string;expect:longint);
type
        PLongint = ^Longint;
var
        P : psizeint;
        rc : longint;
begin
        P := psizeint(s);
        rc:=0;
        if (p = nil)
        then writeln('Nil string.')
        else
{$ifdef  fpc}
  {$if defined(ver1_0) or defined(ver1_9_4)}
         rc:=(p-1)^;
  {$else}
         rc:=psizeint(pchar(p)-sizeof(sizeint)*2)^;
  {$endif}
{$else}
         rc:=psizeint(pchar(p)-sizeof(sizeint)*2)^;
{$endif}
  writeln('Ref count is ',rc,' expected ',expect);
  if rc<>expect then
    halt(1);
end;

function FunctionResultIsRecord(a : RecordWithStrings) : RecordWithStrings;
begin
        result := a;
end;

begin
        onestring := 'one';
        twostring := 'two';
        ARecordWithStrings.one := onestring + twostring;
        twostring := onestring + twostring;
        RefCount(ARecordWithStrings.one,1);
        { Here we allocate a temp so refcount will be 2 }
        ARecordWithStrings := FunctionResultIsRecord(ARecordWithStrings);
        twostring := onestring + twostring;
        RefCount(ARecordWithStrings.one,2);
        { Temp is reused, refcount should stay 2 }
        ARecordWithStrings := FunctionResultIsRecord(ARecordWithStrings);
        twostring := onestring + twostring;
        RefCount(ARecordWithStrings.one,2);
        { Temp is reused, refcount should stay 2 }
        ARecordWithStrings := FunctionResultIsRecord(ARecordWithStrings);
        twostring := onestring + twostring;
        RefCount(ARecordWithStrings.one,2);
end.
