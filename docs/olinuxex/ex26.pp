Program Example26;

{ Program to demonstrate the Access function. }

Uses oldlinux;

begin
  if Access ('/etc/passwd',W_OK) then
    begin
    Writeln ('Better check your system.');
    Writeln ('I can write to the /etc/passwd file !');
    end;
end.
