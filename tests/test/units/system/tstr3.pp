{ %OPT=-O3 }

program tstr2;

type
  veryshortstring = string[3];

var
  TestStr: veryshortstring;

begin
  Str(-12:4, TestStr);
  if TestStr <> ' -1' then
    Halt(1);
  WriteLn('ok');
end.