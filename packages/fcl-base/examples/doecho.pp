program doecho;

uses sysutils;

var r : integer;

begin
  for r := 1 to 25 do
    writeln ('Line : ', inttostr (r));
end.
