{ %NORUN }

program tw39736;

{$mode objfpc}{$H+}
{$ModeSwitch functionreferences}
{$ModeSwitch anonymousfunctions}

{$warn 5028 error}

type
  TProc = reference to procedure;

var
  p: TProc;
procedure Fly(w: word);
begin
  p:=procedure
    begin
      writeln('TBird.Fly w=',w);
    end;
  p();
end;

begin
  Fly(3);
end.

