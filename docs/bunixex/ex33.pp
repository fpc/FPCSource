Program Example33;

{ Program to demonstrate the Select function. }

Uses BaseUnix;

Var FDS : Tfdset;

begin
  fpfd_zero(FDS);
  fpfd_set(0,FDS);
  Writeln ('Press the <ENTER> to continue the program.');
  { Wait until File descriptor 0 (=Input) changes }
  fpSelect (1,@FDS,nil,nil,nil);
  { Get rid of <ENTER> in buffer }
  readln;
  Writeln ('Press <ENTER> key in less than 2 seconds...');
  Fpfd_zero(FDS);
  FpFd_set (0,FDS);
  if fpSelect (1,@FDS,nil,nil,2000)>0 then
    Writeln ('Thank you !')
    { FD_ISSET(0,FDS) would be true here. }
  else
    Writeln ('Too late !');
end.
