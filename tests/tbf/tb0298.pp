{ %FAIL }

program tb0298;

{$mode delphi}
{$modeswitch functionreferences}

type
  TMyProc = reference to procedure(aArg: LongInt);

procedure Test(aArg: TObject);
begin
end;

var
  arr: array of TMyProc;
begin
  Insert(Test, arr, 0);
end.
