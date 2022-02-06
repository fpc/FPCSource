{ anonymous functions can be called like nested functions }

program tanonfunc2;

{$mode delphi}
{$modeswitch anonymousfunctions}

procedure Test;
var
  v: LongInt;
begin
  v := 0;
  procedure begin v := 1; end();
  if v <> 1 then
    Halt(4);
  procedure(aArg: LongInt) begin v := aArg; end(2);
  if v <> 2 then
    Halt(5);
  if function(aArg: LongInt): LongInt begin Result := aArg; end(3) <> 3 then
    Halt(6);
end;

var
  v: LongInt;
begin
  v := 0;
  procedure begin v := 1; end();
  if v <> 1 then
    Halt(1);
  procedure(aArg: LongInt) begin v := aArg; end(2);
  if v <> 2 then
    Halt(2);
  if function(aArg: LongInt): LongInt begin Result := aArg; end(3) <> 3 then
    Halt(3);
  Test;
end.
