Program Example49;

{ Program to demonstrate the Random and Randomize functions. }

Var I,Count,guess : Longint;
    R : Real;

begin
  Randomize; { This way we generate a new sequence every time
               the program is run}
  Count:=0;
  For i:=1 to 1000 do
    If Random>0.5 then inc(Count);
  Writeln ('Generated ',Count,' numbers > 0.5');
  Writeln ('out of 1000 generated numbers.');
  count:=0;
  For i:=1 to 5 do
    begin
    write ('Guess a number between 1 and 5 : ');
    readln(Guess);
    If Guess=Random(5)+1 then inc(count);
    end;
  Writeln ('You guessed ',Count,' out of 5 correct.');
end.
