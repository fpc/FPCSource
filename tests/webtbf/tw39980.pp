{ %opt=-O3 }
{ %fail }

{ This test has been known to generate internal error 2014091205 }
  
program tcond1.pp;

const
  Expected: array[0..3] of array[0..2] of Integer =
    ((-10, 3), (-10, 4), (0, -10), (0, -10));

function TestInput(Input, TestAns: Integer): Boolean;
  var
    O1, O2, O3: Integer;
  begin
    if Input < 2 then 
      begin
		O1 := -10;
		O2 := TestAns;
	  end
	else
	  begin
	    O1 := 0;
		O2 := -10;
	  end;
	  
	TestInput :=
	  (O1 = Expected[Input][0]) and
	  (O2 = Expected[Input][1]);
  end;
  
var
  X: Integer;
  
begin
  for X := 0 to 3 do
    begin
	  if not TestInput(X, X + 3) then
	    Halt(1);
	end;
	
  WriteLn('ok');
end.