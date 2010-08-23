program project1;
{$ifdef fpc}
{$mode objfpc}{$H+}
{$endif}
uses SysUtils, uw17220;

var
  A, B: int64;
begin
  writeln(uw17220.IntToHEX(16, 0)); {Here ERROR: called SysUtils.IntToHEX }
  if uw17220.IntToHEX(16, 0)<>'passed' then
    halt(1);
end.

