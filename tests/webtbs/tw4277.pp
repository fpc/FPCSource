{$mode delphi}
{$J-}

const
  RFCMonthNames : Array[1..12] of String = (
    'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
    'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');

begin
  writeln(RFCMonthNames[10]);
end.
