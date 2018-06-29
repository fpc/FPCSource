program tb0646b;

{$MODE DELPHI}

procedure TestProc;
begin
  Writeln('Hello');
end;

var
  arr1,
  arr2,
  arr3: array [1..10] of Byte;

begin
  Move(TestProc, arr1, 10);
  Move((@TestProc)^, arr2, 10);
  Move(@TestProc^, arr3, 10);
  if (CompareByte(arr1, arr2, 10) <> 0) or
     (CompareByte(arr2, arr3, 10) <> 0) then
  begin
    Writeln('Error!');
    Halt(1);
  end
  else
    Writeln('Ok!');
end.
