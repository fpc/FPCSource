Program Example20;

{ Program to demonstrate the Erase function. }

Var F : Text;

begin
  { Create a file with a line of text in it}
  Assign (F,'test.txt');
  Rewrite (F);
  Writeln (F,'Try and find this when I''m finished !');
  close (f);
  { Now remove the file }
  Erase (f);
end.
