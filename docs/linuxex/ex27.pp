Program Example27;

{ Program to demonstrate the Umask function. }

Uses linux;

begin
  Writeln ('Old Umask was : ',Umask(Octal(111)));
  WRiteln ('New Umask is  : ',Octal(111));
end.
