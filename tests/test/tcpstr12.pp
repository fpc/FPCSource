program tcpstr12;

// check that 'test' constants assigned to ansistring variables have different codepage

{$mode delphi}
type
  cp866 = type AnsiString(866);
var
  A: cp866;
  B: AnsiString;
begin
  B := 'test';
//  if StringCodePage(B) <> DefaultSystemCodePage then
//    halt(1);
  A := 'test';
  if StringCodePage(A) <> 866 then
    halt(2);
end.
