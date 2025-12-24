unit ub0725;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

type
  TValue = record
    f: Pointer;
    generic class function From<T>(constref aValue: T): TValue; static;
  end;

procedure Test;

implementation

var
  { trigger creation of init and fini procs }
  s: String = 'Hello World';

generic class function TValue.From<T>(constref aValue: T): TValue;
begin
  Result.f := TypeInfo(T);
end;

procedure Test;
type
  TArr = array[0..0] of LongInt;
var
  v: TValue;
  arr: TArr;
begin
  arr[0] := 42;
  v := TValue.specialize From<TArr>(arr);
end;

end.

