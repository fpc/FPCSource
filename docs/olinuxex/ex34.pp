Program Example33;

{ Program to demonstrate the SelectText function. }

Uses oldlinux;

Var tv : TimeVal;

begin
  Writeln ('Press the <ENTER> to continue the program.');
  { Wait until File descriptor 0 (=Input) changes }
  SelectText (Input,nil);
  { Get rid of <ENTER> in buffer }
  readln;
  Writeln ('Press <ENTER> key in less than 2 seconds...');
  tv.sec:=2;
  tv.usec:=0;
  if SelectText (Input,@tv)>0 then
    Writeln ('Thank you !')
  else
    Writeln ('Too late !');
end.
