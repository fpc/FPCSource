Program Example43;

{ Program to demonstrate the Uname function. }

Uses BaseUnix;

Var UN : utsname;

begin
  if fpUname (UN)=0 then
    With UN do
      begin
      Writeln ('Name       : ',pchar(@sysname[0]));
      Writeln ('Nodename   : ',pchar(@Nodename[0]));
      Writeln ('release    : ',pchar(@Release[0]));
      Writeln ('Version    : ',pchar(@Version[0]));
      Writeln ('Machine    : ',pchar(@Machine[0]));
{$ifdef Linux} // linuxism
      Writeln ('Domainname : ',pchar(@domainname[0]));
{$endif}
      end;
end.
