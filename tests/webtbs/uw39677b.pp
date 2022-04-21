unit uw39677b;
{$mode Delphi}
interface
function Test<T>(const A: TArray<T>): Integer; overload;
implementation
function Test<T>(const A: TArray<T>): Integer;
begin
  Writeln('b');
end;
end.

