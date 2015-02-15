{ %target=darwin }
{ %recompile }
{ %norun }

{$modeswitch objectivec2}

uses uobjc42;

var
  i: id;
begin
  i.mytest;
end.
