Program Example27;

{ Program to demonstrate the Umask function. }

Uses BaseUnix;

begin
  Writeln ('Old Umask was : ',fpUmask(&111));
  WRiteln ('New Umask is  : ',&111);
end.
