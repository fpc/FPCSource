{$mode delphi}
uses ub0489;
type oo = class
            function getmyint:integer;
            property someprop:integer read getmyint;
            end;

function oo.getmyint:integer;

begin
  result:=1;
end;



procedure test2;

var ch:char;
    x : oo;

begin
  test(x.someprop,ch,1);
end;

begin
end.
