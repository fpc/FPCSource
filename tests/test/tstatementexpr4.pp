{$mode macpas}
{$modeswitch statementexpressions}

var
  i, j: LongInt;
begin
  i := 13;
  j := case i of
    0..10: 'ABCD';
    11..12: $12345678;
    13..42: 'ZYXW';
    otherwise 0;
  Writeln(HexStr(j, 8));
end.
