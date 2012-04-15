unit ugeneric59b;

{$ifdef fpc}
  {$mode delphi}
{$endif}

interface

type
  TTest<T> = class
    class function Test: Integer;
  end;

implementation

class function TTest<T>.Test: Integer;
begin
  Result := 2;
end;

end.

