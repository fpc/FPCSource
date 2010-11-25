{$mode objfpc}

program test;

uses
  fgl;

type
  TIntList = specialize TFPGList<Integer>;

var
  A, B: TIntList;
	i: Integer;

begin
  A := TIntList.Create;
	B := TIntList.Create;
	for i := 0 to 9 do
	  A.Add(i);
	B.Assign(A);
	for i:= 0 to 9 do
	begin
	  if B[i] <> i then
		  Halt(1);
	end;
end.
