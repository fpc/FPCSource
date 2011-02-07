unit uchlp50; 

{$mode objfpc}{$H+}

interface

type
  TFoo = class
  end;

  TFooHelper1 = class helper for TFoo
    function Test: Integer;
  end;

  TFooHelper2 = class helper for TFoo
    function Test: Integer;
  end;

implementation

function TFooHelper1.Test: Integer;
begin
  Result := 1;
end;

function TFooHelper.Test: Integer;
begin
  Result := 2;
end;

end.

