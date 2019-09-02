{ %NORUN }

program tw35955;

{$mode delphi}

type
  TVariantArray = array of Variant;

var
  S: string;
  A: TVariantArray;
begin
  S := 'xyz';
  A := [S]; // << project1.lpr(13,8) Error: Compilation raised exception internally
  Writeln(A[0]);
  Readln;
end.
