{ %FAIL }

{ class constructors *must* be named "Create" }

program tw30729a;

{$mode objfpc}

type
  TTest = class
    class constructor Create2;
  end;

class constructor TTest.Create2;
begin
end;

begin

end.
