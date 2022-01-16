program project1;

{$mode delphi}

uses
  SysUtils;

var
  f: Double;
  n: Int64;

begin
  n := 9876543210;
  f := Double(n);
  if f<>9876543210.0 then
    halt(1);
end.
