Program Example24;

{ Program to demonstrate the Chown function. }

Uses oldlinux;

Var UID,GID : Longint;
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
  if not Chown ('test.txt',UID,GID) then
    if LinuxError=Sys_EPERM then
      Writeln ('You are not root !')
    else
      Writeln ('Chmod failed with exit code : ',LinuxError)
  else
    Writeln ('Changed owner successfully !');
end.
