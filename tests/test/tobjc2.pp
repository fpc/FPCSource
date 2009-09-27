{ %target=darwin}
{ %cpu=powerpc,powerpc64,i386,x86_64,arm }
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
