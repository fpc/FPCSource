{$mode objfpc}
type
  TIntegerDynArray = array of Integer;

Function Write(Const Args: TIntegerDynArray) : Integer;

function GetArgs : TIntegerDynArray;
  begin
    GetArgs:=Args;
  end;

Var
  I : Integer;
begin
  result:=0;
  For I in GetArgs do
    begin
      inc(result,i);
      inc(result);
    end;
end;

var
  IntegerDynArray : TIntegerDynArray;

begin
  SetLength(IntegerDynArray,4);
  if Write(IntegerDynArray)<>4 then
    halt(1);
  writeln('ok');
end.

