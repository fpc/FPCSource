Program Example62;

{ Program to demonstrate the ReadLink function. }

Uses oldlinux;

Var F : Text;
    S : String;

begin
  Assign (F,'test.txt');
  Rewrite (F);
  Writeln (F,'This is written to test.txt');
  Close(f);
  { new.txt and test.txt are now the same file }
  if not SymLink ('test.txt','new.txt') then
    writeln ('Error when symlinking !');
  S:=ReadLink('new.txt');
  If S='' then
    Writeln ('Error reading link !')
  Else
    Writeln ('Link points to : ',S);
 { Now remove links }
 If not Unlink ('new.txt') then
   Writeln ('Error when unlinking !');
 If not Unlink ('test.txt') then
   Writeln ('Error when unlinking !');
end.
