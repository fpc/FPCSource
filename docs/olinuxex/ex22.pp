Program Example22;

{ Program to demonstrate the SymLink and UnLink functions. }

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
  { Removing test.txt still leaves new.txt
    Pointing now to a non-existent file ! }
  If not Unlink ('test.txt') then
    Writeln ('Error when unlinking !');
  Assign (f,'new.txt');
  { This should fail, since the symbolic link
    points to a non-existent file! }
  {$i-}
  Reset (F);
  {$i+}
  If IOResult=0 then
    Writeln ('This shouldn''t happen');
 { Now remove new.txt also }
 If not Unlink ('new.txt') then
   Writeln ('Error when unlinking !');
end.
