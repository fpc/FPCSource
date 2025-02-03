{ %opt=-O3 }
program project2;

{$mode delphi}

{$OPTIMIZATION DFA}
{$OPTIMIZATION FORLOOP}

type
  TRec = record
    EntryCount : sizeuint; { see TInterfaceTable.EntryCount }
  end;
  PRec = ^TRec;

const
  ANYCONST = 2;

function TestLoopUnsigned(Rec: PRec): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Rec^.EntryCount - ANYCONST do
    Inc(Result);
end;

var
  Rec: TRec;
  N: SizeInt;
begin
  Rec.EntryCount := ANYCONST;
  N := TestLoopUnsigned(@Rec);
  if N <> 1 then
  begin
    WriteLn('ERROR! N=', N);
    Halt(1);
  end;
  WriteLn('OK');
end.
