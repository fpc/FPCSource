{ %NORUN }

{ second simple scope test for class helpers }
program tchlp11;

{$ifdef fpc}
  {$mode objfpc}
{$endif}

type
  TObjectHelper = class helper for TObject
    class procedure SomeMethod;
  end;

class procedure TObjectHelper.SomeMethod;
begin

end;

begin
  TObject.SomeMethod;
end.

