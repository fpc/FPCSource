Program Example47;

{ Program to demonstrate the DirName function. }

Uses oldlinux;

Var S : String;

begin
  S:=FExpand(Paramstr(0));
  Writeln ('This program is in directory : ',Dirname(S));
end.
