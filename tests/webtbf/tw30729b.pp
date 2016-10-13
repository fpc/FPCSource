{ %FAIL }

{ class destructors *must* be named "Destroy" }

program tw30729b;

{$mode objfpc}

type
  TTest = class
    class destructor Destroy2;
  end;

class destructor TTest.Destroy2;
begin
end;

begin

end.
