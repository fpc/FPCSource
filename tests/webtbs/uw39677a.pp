unit uw39677a;
{$mode Delphi}
interface
function Test(const A: Rawbytestring): Integer; overload;
implementation
function Test(const A: Rawbytestring): Integer;
begin
  Writeln('a');
end;
end.

