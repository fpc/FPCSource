Program Example24;

{ Program to demonstrate the Chown function. }

Uses BaseUnix;

Var UID : TUid;
    GID : TGid;
    F : Text;

begin

  Writeln ('This will only work if you are root.');
  Write ('Enter a UID : ');readln(UID);
  Write ('Enter a GID : ');readln(GID);
  Assign (f,'test.txt');
  Rewrite (f);
  Writeln (f,'The owner of this file should become : ');
  Writeln (f,'UID : ',UID);
  Writeln (f,'GID : ',GID);
  Close (F);
  if fpChown ('test.txt',UID,GID)<>0 then
    if fpgeterrno=ESysEPERM then
      Writeln ('You are not root !')
    else
      Writeln ('Chmod failed with exit code : ',fpgeterrno)
  else
    Writeln ('Changed owner successfully !');
end.
