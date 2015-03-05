{ %FAIL }

program tb0250;

{$mode delphi}

type
  TTest<T> = class
  class var
    fTest: TClass;
    procedure Test;
  end;

procedure TTest<T>.Test;
begin
  fTest.ToString;
end;

begin

end.

