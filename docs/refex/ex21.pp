Program Example21;

{ Program to demonstrate the Exit function. }

Procedure DoAnExit (Yes : Boolean);

{ This procedure demonstrates the normal Exit }

begin
  Writeln ('Hello from DoAnExit !');
  If Yes then
    begin
    Writeln ('Bailing out early.');
    exit;
    end;
  Writeln ('Continuing to the end.');
end;

Function Positive (Which : Integer) : Boolean;

{ This function demonstrates the extra FPC feature of Exit :
  You can specify a return value for the function }

begin
  if Which>0 then
    exit (True)
  else
    exit (False);
end;

begin
  { This call will go to the end }
  DoAnExit (False);
  { This call will bail out early }
  DoAnExit (True);
  if Positive (-1) then
    Writeln ('The compiler is nuts, -1 is not positive.')
  else
    Writeln ('The compiler is not so bad, -1 seems to be negative.');
end.
