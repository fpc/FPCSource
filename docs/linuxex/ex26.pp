Program Example26;

{ Program to demonstrate the Access function. }

Uses BaseUnix;

begin
  if fpAccess ('/etc/passwd',W_OK)=0 then
    begin
    Writeln ('Better check your system.');
    Writeln ('I can write to the /etc/passwd file !');
    end;
end.
