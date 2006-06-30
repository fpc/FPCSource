program bug7104;

{$mode objfpc}{$H+}

Uses SysUtils;

function count: integer;
begin
  result := 5;
end;

var
  i: Integer;

begin
  for i := 0 to count-1 do begin
    writeln(i);
    if FileExists('asdkjasdjalsj') then;

    if i>5 then halt(1);
  end;
end.

