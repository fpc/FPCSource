{$mode delphi}

unit uw38216a;

interface

type
  TTestRecord<T> = record
    function TestFunction: T;
  end;

implementation

function TTestRecord<T>.TestFunction: T;
begin
  Result := Default(T);
end;

end.
