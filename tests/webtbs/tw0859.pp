type
  TBoolArray = array [0..1048576] of Boolean;

procedure OrBoolProc(var Vector1; const Vector2; Count: Integer);
var
  I: Integer;
begin
  for I:=0 to Count - 1 do
    TBoolArray(Vector1)[I]:=TBoolArray(Vector1)[I] or TBoolArray(Vector2)[I];
end;

var
  A, B: array [0..10] of Boolean;
  I: Integer;
const
  error : boolean = false;
begin
  for I:=0 to High(A) do A[I]:=False;
  for I:=0 to High(B) do B[I]:=True;
  OrBoolProc(A, B, SizeOf(A));
  for I:=0 to High(A) do
    begin
      write(A[I], ' ');
      if not A[i] then
        error:=true;
    end;
  writeln;
  if error then
    Halt(1);

end.
