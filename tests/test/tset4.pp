{$mode objfpc}
{$packset 1}
uses
  sysutils;

function possetex (const c:string;const s : ansistring;count:Integer ):Integer;

var cset : TSysCharSet;
    i    : integer;
begin
  cset:=[];
  if length(c)>0 then
  for i:=1 to length(c) do
    include(cset,c[i]);
end;

begin
end.

