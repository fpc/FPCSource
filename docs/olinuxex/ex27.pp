Program Example27;

{ Program to demonstrate the Umask function. }

Uses oldlinux;

begin
  Writeln ('Old Umask was : ',Umask(Octal(111)));
  WRiteln ('New Umask is  : ',Octal(111));
end.
