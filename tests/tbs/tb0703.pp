
{$mode delphi}
uses
  Types;

type
  TArrayFunc = function: TIntegerDynArray;

function MyFunc : TIntegerDynArray;
  begin
    SetLength(Result,1);
    Result[0]:=$12345678;
  end;

var
  f: TArrayFunc;
  i: integer;
begin
 f := @MyFunc;
 i := f[0];
 if i<>$12345678 then
   halt(1);
end.
