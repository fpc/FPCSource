{$optimization on}
program Project1;

{$mode objfpc}{$H+}

type
 TMyArray = record
  Stuff: array of Longword;
 end;

Var
 MyArray: TMyArray;
 I: Integer;

procedure WriteNumbers(A: TMyArray);
begin
 for I := 0 to High(A.Stuff) do WriteLn(A.Stuff[I]);
end;

begin
 SetLength(MyArray.Stuff, 100);
 for I := 0 to High(MyArray.Stuff) do MyArray.Stuff[I] := I;
 WriteNumbers(MyArray);
end.
