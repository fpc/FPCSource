Program Example5;

{ Program to demonstrate the Assign function. }

Var F : text;

begin
  Assign (F,'');
  Rewrite (f);
  { The following can be put in any file by redirecting it
    from the command line.}
  Writeln (f,'This goes to standard output !');
  Close (f);
  Assign (F,'Test.txt');
  rewrite (f);
  writeln (f,'This doesn''t go to standard output !');
  close (f);
end.
