unit ugeneric59a;

{$ifdef fpc}
  {$mode delphi}
{$endif}

interface

type
  TTest<T> = class
    class function Test: Integer;
  end;

  TTest<T, S> = class
  end;

implementation

class function TTest<T>.Test: Integer;
begin
  Result := 1;
end;

end.
