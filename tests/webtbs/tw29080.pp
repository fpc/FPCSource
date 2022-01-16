unit tw29080;

{$mode delphi}

interface

function Min<T>(const A, B: T): T;
function Max<T>(const A, B: T): T;
procedure Test<T>(const A, B: T);

implementation

function Min<T>(const A, B: T): T;
// Error on line below, GenericTest.pas(14,1) Error: Internal error 200301231
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

function Max<T>(const A, B: T): T;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;

procedure Test<T>(const A, B: T);
var
  Value: T;
begin
  // This should be legal
  Value := Min<T>(A, B);
  WriteLn('The Min<T> of values, ', A, ' and ', B, ' are: ', Value);
  // As well as this
  Value := Max<T>(A, B);
  WriteLn('The Max<T> of values, ', A, ' and ', B, ' are: ', Value);
end;

procedure TestFix;
begin
  Test<LongInt>(42, 21);
end;

end.
