{ %FAIL }

program tw36014;

{$mode objfpc}

type

  { TTest }

  TTest = class
  public
    procedure Test;
  end;
var
  T: TTest;

{ TTest }

procedure TTest.Test;
begin

end;

begin
  T := TTest.Create;
  T.specialize Test<T>;
  T.Free;
end.

