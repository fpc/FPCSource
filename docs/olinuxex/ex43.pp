Program Example43;

{ Program to demonstrate the Uname function. }

Uses oldlinux;

Var UN : utsname;

begin
  if Uname (UN) then
    With UN do
      begin
      Writeln ('Name       : ',pchar(@sysname[0]));
      Writeln ('Nodename   : ',pchar(@Nodename[0]));
      Writeln ('release    : ',pchar(@Release[0]));
      Writeln ('Version    : ',pchar(@Version[0]));
      Writeln ('Machine    : ',pchar(@Machine[0]));
      Writeln ('Domainname : ',pchar(@domainname[0]));
      end;
end.
