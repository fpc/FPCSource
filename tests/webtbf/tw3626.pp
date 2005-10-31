{ %skipcpu=x86_64,powerpc64 }
{ %fail }

{ Source provided for Free Pascal Bug Report 3626 }
{ Submitted by "Marc Geldon" on  2005-02-02 }
{ e-mail: marc.geldon@proitsystems.de }
program project1;

{$H+}

uses
  SysUtils
  { add your units here };

var
  i: Integer;
begin
  { This should give an range check error at compile time for
    32bit systems }
  for i := 0 to 90000000000000000 do
  begin
    writeln(inttostr(i));
  end;
end.
