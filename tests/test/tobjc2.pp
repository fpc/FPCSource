{ %target=darwin}
{ %cpu=powerpc,i386}
{ %norun }

{$mode objfpc}
{$modeswitch objectivec1}

var
  a: NSObject;
  b: id;
begin
  a:=b;
  b:=a;
end.
