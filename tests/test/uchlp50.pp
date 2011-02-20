unit uchlp50; 

{$ifdef fpc}
  {$mode objfpc}{$H+}
{$endif}

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

function TFooHelper2.Test: Integer;
begin
  Result := 2;
end;

end.

