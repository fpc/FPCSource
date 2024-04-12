{ %FAIL }

program tw40725;

{$mode delphi}
{$ModeSwitch functionreferences}

type
  TMyProc = reference to procedure(const A: Integer; const B: string);
  TMyProcArray = array of TMyProc;

function GetArray: TMyProcArray;
  procedure MyProc(const A: TObject);
  begin

  end;
begin
  //Result := [MyProc]; // compiler error -> OK
  Result := Result + [MyProc]; // NO COMPILER ERROR -> BUG
end;

var
  A: TMyProcArray;
  P: TMyProc;
begin
  A := GetArray;
  for P in A do
    P(1, '');
end.

