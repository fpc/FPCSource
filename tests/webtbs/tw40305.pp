program tw40305;

{$mode objfpc}

uses
  SysUtils;

var
  r: Real;
  s: String;
begin
  r := 2.42;
  FormatSettings.DecimalSeparator:='.';
  s := r.ToString;
  if s <> '2.42' then
    Halt(1);
end.
