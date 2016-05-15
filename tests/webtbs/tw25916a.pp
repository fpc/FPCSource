{ %OPT=-Sh}
{$MODE OBJFPC}
{$OPTIMIZATION DFA}
{$HINTS ON}
program test;

procedure TestText(var F: Text);
begin
   Writeln(F, 'Test'); // Hint: Local variable "F" does not seem to be initialized
end;

begin
   TestText(Output);
end.
