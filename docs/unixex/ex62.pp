Program Example62;

{ Program to demonstrate the ReadLink function. }

Uses BaseUnix,Unix;

Var F : Text;
    S : String;

begin
  Assign (F,'test.txt');
  Rewrite (F);
  Writeln (F,'This is written to test.txt');
  Close(f);
  { new.txt and test.txt are now the same file }
  if fpSymLink ('test.txt','new.txt')<>0 then
    writeln ('Error when symlinking !');
  S:=fpReadLink('new.txt');
  If S='' then
    Writeln ('Error reading link !')
  Else
    Writeln ('Link points to : ',S);
 { Now remove links }
 If fpUnlink ('new.txt')<>0 then
   Writeln ('Error when unlinking !');
 If fpUnlink ('test.txt')<>0 then
   Writeln ('Error when unlinking !');
end.
