Program Example88;

{ Program to demonstrate the AssignFile and CloseFile functions. }

{$MODE Delphi}

Var F : text;

begin
  AssignFile(F,'textfile.tmp');
  Rewrite(F);
  Writeln (F,'This is a silly example of AssignFile and CloseFile.');
  CloseFile(F);
end.
