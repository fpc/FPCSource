{ %CPU=i386,x86_64 }
{ %OPT=-a -O2 -CpCOREI }

{ This test evaluates OptPass2Jcc's ability to create CMOV instructions with
  constants while ensuring correct code is still generated. }
  
program tcond1;

uses
  CPU;

const
  Expected: array[0..3] of array[0..2] of LongInt =
    ((-10, 3, 2), (-10, 4, 2), (0, 0, -10), (0, 0, -10));

function TestInput(Input, TestAns: LongInt): Boolean;
  var
    O1, O2, O3: LongInt;
  begin
    if Input < 2 then 
      begin
        O1 := -10;
        O2 := TestAns;
        O3 := 2;
      end
    else
      begin
        O1 := 0;
        O2 := 0;
        O3 := -10;
      end;

    TestInput :=
      (O1 = Expected[Input][0]) and
      (O2 = Expected[Input][1]) and
      (O3 = Expected[Input][2]);
  end;

var
  X: LongInt;
  
begin
  if not CMOVSupport then
    begin
      WriteLn('unsupported');
      Halt(0);
    end;      

  for X := 0 to 3 do
    begin
      if not TestInput(X, X + 3) then
        Halt(1);
    end;
    
  WriteLn('ok');
end.
