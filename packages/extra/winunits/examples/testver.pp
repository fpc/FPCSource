program testver;

uses winver;

begin
  Writeln('Platform      : ',Win32Platform);
  Writeln('Major version : ',Win32MajorVersion);
  Writeln('Minor version : ',Win32MinorVersion);
  Writeln('Build number  : ',Win32BuildNumber);
  Writeln('CSD version   : ',Win32CSDVersion);
end.
