{$mode delphi}

unit uw38216b;

interface

uses
  uw38216a;

function Test1<T>: TTestRecord<T>;
function Test2<T>: TTestRecord<T>;

implementation

function Test1<T>: TTestRecord<T>;
begin
  Result := Default(TTestRecord<T>);
end;

function Test2<T>: TTestRecord<T>;
begin
  Result := Test1<T>;
end;

end.
