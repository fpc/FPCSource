{$mode objfpc}{H+}
program test_widestrings;
var
  W: WideString;
begin
  W := '123456';
  if Length(W)<>6 then
    begin
      WriteLn('Test1 Failed, it''s ', Length(W));
      halt(1);
    end
  else
    WriteLn('Test1 Passed');

  SetLength(W, 6);
  if Length(W)<>6 then
    begin
      WriteLn('Test2 Failed, it''s ', Length(W));
      halt(1);
    end
  else
    WriteLn('Test2 Passed');
end.
