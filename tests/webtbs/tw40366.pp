{ %OPT=-O3 -Sg }

{ #40366 - bad code generation in x86 peephole optimizer (TEST/JNE/TEST/JNE merge) caused some conditions to be reordered or omitted }

program tw40366;

function TestFunc(Input1, Input2: Cardinal): Cardinal;
label
  PrintNegative, PrintPositive;
begin
  { I know gotos are ugly, but I need to force a specific assembly genereation }
  if (Input1 and $1) <> 0 then
    goto PrintPositive;
  if Input2 > 3 then
    goto PrintNegative;
  if (Input1 and $2) <> 0 then
    goto PrintPositive;

PrintNegative:
  { Use arithmetic to prevent unwanted JMP/RET optimisations }
  Exit(Input2 or $80000000);

PrintPositive:
  { Use arithmetic to prevent unwanted JMP/RET optimisations }
  Exit(Input1 + Input2);
end;

const
  Inputs1:  array[0..4] of Cardinal = (1, 2, 2, 3, 4);
  Inputs2:  array[0..4] of Cardinal = (4, 3, 5, 6, 7);
  Expected: array[0..4] of Cardinal = (5, 5, $80000005, 9, $80000007);
  
var
  X: Integer; Output: Cardinal;
begin
  for X := Low(Inputs1) to High(Inputs2) do
    begin
      Output := TestFunc(Inputs1[X], Inputs2[X]);
      if Output <> Expected[X] then
        begin
          WriteLn('FAIL: TestFunc(', Inputs1[X], ', ', Inputs2[X], ') returned ', Output, ' but expected ', Expected[X]);
          Halt(1);
        end;
    end;
  
  WriteLn('ok');
end.