{ Record composition: a named composition of a record with management operators
  Initialize and Finalize, a local composing record calls each exactly once }
program record_compose_test;

{$Mode ObjFPC}{$H+}
{$ModeSwitch AdvancedRecords}
{$ModeSwitch RecordComposition}

type
  TChildRec = record
    C: Integer;
    class operator Initialize(var r: TChildRec);
    class operator Finalize(var r: TChildRec);
  end;

  TComposed = record
    A: Integer;
    contains child: TChildRec;
    B: Integer;
  end;

var
  InitCount: Integer = 0;
  FinalCount: Integer = 0;

class operator TChildRec.Initialize(var r: TChildRec);
begin
  r.C := 42;
  Inc(InitCount);
end;

class operator TChildRec.Finalize(var r: TChildRec);
begin
  Inc(FinalCount);
end;

procedure Test;
var
  c: TComposed;
begin
  WriteLn('InitCount: ', InitCount);
  if InitCount<>1 then
    halt(1);
  if c.C<>42 then
    halt(2);
  if FinalCount<>0 then
    halt(3);
end;

begin
  Test;
  WriteLn('FinalCount: ', FinalCount);
  if InitCount<>1 then
    halt(4);
  if FinalCount<>1 then
    halt(5);
  WriteLn('ok');
end.
