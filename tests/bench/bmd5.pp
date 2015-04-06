{$mode objfpc}
uses
  timer,md5;


const
  Strings: array[1..7] of string = (
    '',
    'a',
    'abc',
    'message digest',
    'abcdefghijklmnopqrstuvwxyz',
    'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789',
    '12345678901234567890123456789012345678901234567890123456789012345678901234567890'
    );

var
  i,j : integer;
begin
  start;
  for i:=1 to 500000 do
    for j:=low(Strings) to high(Strings) do
      MDString(Strings[j],MD_VERSION_5);
  stop;
  writeln;
end.
