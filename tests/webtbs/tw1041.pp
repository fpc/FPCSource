uses
  sysutils,dos;

begin
Writeln('Dos DiskSize = ',Dos.DiskSize(0));
Writeln('Sysutils DiskSize = ',SysUtils.DiskSize(0));
Writeln('Dos DiskFree = ',Dos.DiskFree(0));
Writeln('Sysutils DiskFree = ',SysUtils.DiskFree(0));
if Dos.DiskSize(0)<>SysUtils.DiskSize(0) then
  Begin
    Writeln('Error with DiskSize');
    Halt(1);
  End;
end.
