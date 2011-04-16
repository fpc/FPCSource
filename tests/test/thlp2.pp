{ %NORUN }

{ tests that helpers can introduce properties }
program thlp2;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TObjectHelper = class helper for TObject
    class function GetTest: Integer; static;
    class procedure SetTest(aValue: Integer); static;
    class property Test: Integer read GetTest write SetTest;
  end;

class function TObjectHelper.GetTest: Integer;
begin
end;

class procedure TObjectHelper.SetTest(aValue: Integer);
begin

end;

begin
  TObject.Test := TObject.Test;
end.
