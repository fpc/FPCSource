{$h+}
program example92;

{ This program demonstrates the
  GetEnvironmentVariable function }

uses sysutils;

begin
   Writeln('PATH is: ',GetEnvironmentVariable('PATH'));
end.