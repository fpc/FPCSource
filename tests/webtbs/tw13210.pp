{$ifdef FPC}
{$mode macpas}
{$endif}
{$ifdef __GPC__}
{$mac-objects}
{$endif}
program withtest2;
type obj = object i: integer end;
var p, q, r: obj;
begin
    new( p);
    new( q);
    p.i:= 1;
    q.i:= 2;
    r:= p;
    with r do
    begin
        r:=q;
        writeln( i);
        if (i<>1) then
          halt(1);
    end
end.
