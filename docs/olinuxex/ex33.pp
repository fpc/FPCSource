Program Example33;

{ Program to demonstrate the Select function. }

Uses oldlinux;

Var FDS : FDSet;

begin
  FD_Zero (FDS);
  FD_Set (0,FDS);
  Writeln ('Press the <ENTER> to continue the program.');
  { Wait until File descriptor 0 (=Input) changes }
  Select (1,@FDS,nil,nil,nil);
  { Get rid of <ENTER> in buffer }
  readln;
  Writeln ('Press <ENTER> key in less than 2 seconds...');
  FD_Zero (FDS);
  FD_Set (0,FDS);
  if Select (1,@FDS,nil,nil,2000)>0 then
    Writeln ('Thank you !')
    { FD_ISSET(0,FDS) would be true here. }
  else
    Writeln ('Too late !');
end.
