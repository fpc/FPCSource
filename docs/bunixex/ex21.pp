Program Example21;

{ Program to demonstrate the Link and UnLink functions. }

Uses BaseUnix;

Var F : Text;
    S : String;
begin
  Assign (F,'test.txt');
  Rewrite (F);
  Writeln (F,'This is written to test.txt');
  Close(f);
  { new.txt and test.txt are now the same file }
  if fpLink ('test.txt','new.txt')<>0 then
    writeln ('Error when linking !');
  { Removing test.txt still leaves new.txt }
  If fpUnlink ('test.txt')<>0 then
    Writeln ('Error when unlinking !');
  Assign (f,'new.txt');
  Reset (F);
  While not EOF(f) do
    begin
    Readln(F,S);
    Writeln ('> ',s);
    end;
 Close (f);
 { Remove new.txt also }
 If not FPUnlink ('new.txt')<>0 then
   Writeln ('Error when unlinking !');
end.
