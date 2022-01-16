{ %fail }

program project1;
{$mode objfpc}{$H+}
function bug(s: string): string;
var
  p:pointer=@s;//internal error 2014062901
begin
end;

begin
end.
