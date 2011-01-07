{ %NORUN }

{ first simple scope test for class helpers }
program tchlp10;

{$ifdef fpc}
  {$mode objfpc}
{$endif}

type
  TObjectHelper = class helper for TObject
    procedure SomeMethod;
  end;

procedure TObjectHelper.SomeMethod;
begin

end;

var
  o: TObject;
begin
  o.SomeMethod;
end.

