program tw40031;{$mode objfpc}{$modeswitch functionreferences}{$modeswitch anonymousfunctions}
type Aoc0 = reference to procedure (aoc: array of const);
type Aoc1 = reference to procedure (var aoc: array of const);
type Aoc2 = reference to procedure (constref aoc: array of const);
type Aoc3 = reference to procedure (const aoc: array of const);
var
  t: aoc0;
  i: longint;
begin
  i := 0;
  t := procedure(aArgs: array of const) begin i:=length(aArgs); end;
  t([1, 'Hello']);
  if i <> 2 then
    halt(1);
end.

